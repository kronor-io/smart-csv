{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OrPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvRunner.JobHandlers.SmartGenerateCsv (SmartGraphqlCsvGenerate) where

import Control.Exception qualified
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..), RAW, Selection (..), unpackName)
import Data.String.Interpolate (iii)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (withFrozenCallStack)
import JobSchemas.GenerateSettlementCsv qualified as Gscv
import JobSchemas.SmartGraphqlCsvGenerate
import Kronor.Db qualified as Db
import Kronor.Db.Types.Bigint (Bigint (..))
import Kronor.Logger (RequestId (..), requestId)
import Kronor.SmartCsv.ColumnConfig (ColumnConfig)
import Kronor.SmartCsv.ColumnConfig qualified as SmartCsvColumnConfig
import Kronor.SmartCsv.ErrorHandling qualified as SmartCsvErrorHandling
import Kronor.SmartCsv.Notification qualified as SmartCsvNotification
import Kronor.SmartCsv.Pagination qualified as SmartCsv
import Kronor.SmartCsv.Query (DecodedResponsePage (..), GenericQuery (..))
import Kronor.SmartCsv.Query qualified as SmartCsvQuery
import Kronor.SmartCsv.Statements qualified as SmartCsvStatements
import Kronor.SmartCsv.TokenClaims qualified as SmartCsvTokenClaims
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http.Tls
import Network.HTTP.Types.Status qualified as Http.Status
import RIO hiding ((%~), (.~), (^.), (^..), (^?))
import RIO.List (headMaybe)
import RIO.Vector qualified as Vector
import SmartCsvApi.Auth qualified as SmartCsvAuth
import SmartCsvRunner.AWS.Types
import SmartCsvRunner.CsvGeneration.Generate qualified as Generate
import SmartCsvRunner.Env (Options (..))
import SmartCsvRunner.Job (Job, JobEnv (jobEnv), getJobId)
import SmartCsvRunner.Job qualified as Job
import SmartCsvRunner.Job.SmartCsvEnv (SmartCsvEnv (..))
import SmartCsvRunner.MultipartUpload (EncodedCsvPage (..))
import SmartCsvRunner.Job.Type (JobProcessor (..), subJobEnv)

logSource :: LogSource
logSource = "smart-csv-runner:SmartCsvRunner.JobHandlers.SmartGenerateCsv"

instance JobProcessor SmartCsvEnv SmartGraphqlCsvGenerate where
  processJob (SmartGraphqlCsvGenerate payload) = subJobEnv (\env -> (smartCsvS3Config env, smartCsvOptions env)) do
    generateCSV payload
  closeJob (SmartGraphqlCsvGenerate _) = subJobEnv smartCsvS3Config do
    Job.giveupS logSource "Payment CSV generation job was closed"

type CsvRow = Map Text Csv.Field

graphqlChunkTargetBytes :: Int64
graphqlChunkTargetBytes = 5_242_880

generateCSV ::
  (HasCallStack) =>
  Payload ->
  Job (S3Config, Options) ()
generateCSV payload = do
  pId <- getJobId
  generatedCsvPayload <-
    Db.readOr
      (retry . displayShow)
      ( Db.statement payload.csvId SmartCsvStatements.selectGeneratedCsvPayload
          <&> \gcsv ->
            Gscv.Payload
              { shardId = Bigint gcsv.shardId,
                stateMachineId = gcsv.stateMachineId,
                reportId = gcsv.reportId,
                startDate = gcsv.startDate,
                endDate = gcsv.endDate
              }
      )
  (gq, tokenClaims, recipient, mInlineConfig, mConfigName) <-
    Db.readOr
      (retry . displayShow)
      ( Db.statement
          (coerce payload.shardId, payload.csvId)
          SmartCsvStatements.selectGeneratorConfig
      )
  -- Resolve column config: inline > named preset > pass-through
  resolvedColumnConfig <- case mInlineConfig of
    Just inlineJson -> pure (SmartCsvColumnConfig.parseColumnConfig inlineJson)
    Nothing -> case mConfigName of
      Just configName -> do
        mNamedConfig <-
          Db.readOr
            (retry . displayShow)
            (Db.statement configName SmartCsvStatements.selectColumnConfigByName)
        pure $ maybe mempty SmartCsvColumnConfig.parseColumnConfig mNamedConfig
      Nothing -> pure mempty
  case parseRequest (GQLRequest {query = gq.query, operationName = Nothing, variables = Nothing}) of
    Success executableDocument warnings -> do
      logWarn (displayBytesUtf8 (toStrictBytes (Aeson.encode warnings)))
      options <- asks $ snd . jobEnv
      inferredRootField <-
        maybe
          (Job.giveupS logSource "SelectionSet is empty, could not infer root query")
          pure
          (headMaybe (toList executableDocument.operation.operationSelection))
      let inferredRoot = unpackName inferredRootField.selectionName
          fileName = [iii|#{inferredRoot}.csv|]
          s3Path = [iii|smartPaymentCsv/#{shardId payload}/#{csvId payload}/|] :: Text
          fileKey = s3Path <> fileName
          inferredHeadersFromGql = SmartCsv.inferHeaders resolvedColumnConfig inferredRootField
          emptyMap = Map.fromList ((,mempty) <$> inferredHeadersFromGql)
      authToken <- genTokenFromClaims tokenClaims
      httpManager <- liftIO $ Http.newManager Http.Tls.tlsManagerSettings
      let paginationFields = SmartCsvQuery.resolvePaginationFields gq
      eSignedLink <-
        tryAny
          $ subJobEnv fst
          $ Generate.generateCsv
            (gqlQuery httpManager resolvedColumnConfig pId inferredRootField paginationFields authToken gq inferredRoot emptyMap (Vector.fromList (encodeUtf8 <$> inferredHeadersFromGql)) options.optionsGraphqlPageSize options.optionsGraphqlUrl)
            (pure True)
            (Vector.fromList (encodeUtf8 <$> inferredHeadersFromGql))
            SmartCsv.encodePaginationCursor
            fileKey
            generatedCsvPayload
      mSignedLink <- either throwM pure eSignedLink
      sendCsvDoneEmail recipient mSignedLink
    Failure errs -> do
      Job.giveupS logSource (displayBytesUtf8 (toStrictBytes (Aeson.encode errs)))
  where
    gqlCursor :: ColumnConfig -> Job.PayloadId -> Selection RAW -> NonEmpty SmartCsv.PaginationField -> CsvRow -> SmartCsv.PaginationCursor
    gqlCursor colConfig pId rootSelection paginationFields v =
      case SmartCsv.extractCursor colConfig rootSelection paginationFields v of
        Left cursorErr ->
          case SmartCsvErrorHandling.classifyCursorError cursorErr of
            SmartCsvErrorHandling.Retry _ -> error "Unexpected retry for cursor error"
            SmartCsvErrorHandling.Giveup msg ->
              Control.Exception.throw $ Job.NonRetryableException pId $ Job.StringyException logSource msg
        Right cursor -> cursor
    gqlQuery :: Http.Manager -> ColumnConfig -> Job.PayloadId -> Selection RAW -> NonEmpty SmartCsv.PaginationField -> ByteString -> GenericQuery -> Text -> CsvRow -> Vector Csv.Name -> Int -> Text -> Maybe SmartCsv.PaginationCursor -> Job S3Config (Maybe (EncodedCsvPage SmartCsv.PaginationCursor))
    gqlQuery httpManager colConfig pId rootSelection paginationFields authToken GenericQuery {..} root emptyCsvRow header batchSize graphqlUrl mCursor = do
      let reqBody = SmartCsvQuery.buildRequestBody paginationFields batchSize mCursor GenericQuery {..}
      eRes <- streamResponsePage httpManager graphqlUrl authToken reqBody colConfig root emptyCsvRow header
      case eRes of
        Right page ->
          pure $ case page.lastRow of
            Nothing -> Nothing
            Just lastParsedRow ->
              Just
                EncodedCsvPage
                  { encodedRows = page.encodedRows,
                    encodedRowBytes = page.encodedRowBytes,
                    lastCursor = gqlCursor colConfig pId rootSelection paginationFields lastParsedRow,
                    rowCount = page.rowCount
                  }
        Left responseErr ->
          case SmartCsvErrorHandling.classifyResponseError responseErr of
            SmartCsvErrorHandling.Retry msg -> retry (display msg)
            SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)

    streamResponsePage :: Http.Manager -> Text -> ByteString -> LByteString -> ColumnConfig -> Text -> CsvRow -> Vector Csv.Name -> Job S3Config (Either SmartCsvQuery.ResponseError DecodedResponsePage)
    streamResponsePage httpManager graphqlUrl authToken reqBody colConfig root emptyCsvRow header = do
      requestIdValue <- asks requestId
      request0 <-
        liftIO (Http.parseRequest (Text.unpack graphqlUrl))
          `catchAny` \_ -> Job.giveupS logSource "Could not build graphql url."
      let request =
            request0
              { Http.method = "POST",
                Http.requestBody = Http.RequestBodyLBS reqBody,
                Http.requestHeaders =
                  [ ("Authorization", "Bearer " <> authToken),
                    ("Content-Type", "application/json"),
                    ("X-Request-Id", encodeUtf8 (coerce @RequestId @Text requestIdValue))
                  ]
              }
      parseResult <-
        liftIO
          $ Http.withResponse request httpManager
          $ \response -> do
            let status = Http.Status.statusCode response.responseStatus
            if status < 200 || status >= 300
              then pure (Left status)
              else Right <$> SmartCsvQuery.decodeResponseChunkWith graphqlChunkTargetBytes colConfig root emptyCsvRow header (Http.responseBody response)
      case parseResult of
        Left status ->
          case SmartCsvErrorHandling.classifyHttpStatusError status of
            SmartCsvErrorHandling.Retry msg -> retry (display msg)
            SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)
        Right (Right responsePage) -> pure responsePage
        Right (Left err) ->
          case SmartCsvErrorHandling.classifyJsonDecodeError err of
            SmartCsvErrorHandling.Retry msg -> retry (display msg)
            SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)

sendCsvDoneEmail :: Text -> Maybe Text -> Job a ()
sendCsvDoneEmail recipient mUrl = do
  let completionEmail = SmartCsvNotification.mkCompletionEmail mUrl
      enqueueMeta = SmartCsvNotification.defaultEnqueueMeta
  Db.writeOr (retry . displayShow)
    $ Db.statement
      ( recipient,
        completionEmail.subject,
        completionEmail.htmlBody,
        enqueueMeta.caller,
        enqueueMeta.tag,
        enqueueMeta.requestId,
        enqueueMeta.priority
      )
      SmartCsvStatements.enqueueCompletionEmail

genTokenFromClaims :: Aeson.Value -> Job (S3Config, Options) ByteString
genTokenFromClaims tokenClaims = do
  parsedClaims <-
    either
      ( \err -> case SmartCsvErrorHandling.classifyTokenClaimsError err of
          SmartCsvErrorHandling.Retry msg -> retry (display msg)
          SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)
      )
      pure
      (SmartCsvTokenClaims.parseTokenClaims tokenClaims)
  options <- snd . Job.jobEnv <$> ask
  token <-
    liftIO
      ( SmartCsvAuth.signJwtFromClaims
          options.optionsJwtSecret
          parsedClaims.associatedEmail
          parsedClaims.hasuraClaims
          parsedClaims.tokenType
          parsedClaims.tokenId
      )
      >>= either (Job.giveupS logSource . display) pure
  return $ Text.encodeUtf8 token

retry :: Utf8Builder -> Job env a
retry s = withFrozenCallStack do
  timeUntilRetry <- Job.defaultTimeUntilNextAttempt <$> asks Job.jobFailedAttempts
  Job.retryS logSource timeUntilRetry s
