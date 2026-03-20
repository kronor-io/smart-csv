{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OrPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvRunner.JobHandlers.SmartGenerateCsv (SmartGraphqlCsvGenerate) where

import Control.Exception qualified
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Coerce (coerce)
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..), Selection (..), unpackName)
import Data.String.Interpolate (iii)
import Data.Text.Encoding qualified as Text
import GHC.Stack (withFrozenCallStack)
import JobSchemas.GenerateSettlementCsv qualified as Gscv
import JobSchemas.SmartGraphqlCsvGenerate
import Kronor.Db qualified as Db
import Kronor.Db.Types.Bigint (Bigint (..))
import Kronor.Http qualified as Req
import Kronor.SmartCsv.ColumnConfig qualified as SmartCsvColumnConfig
import Kronor.SmartCsv.ErrorHandling qualified as SmartCsvErrorHandling
import Kronor.SmartCsv.Notification qualified as SmartCsvNotification
import Kronor.SmartCsv.Pagination qualified as SmartCsv
import Kronor.SmartCsv.Query (GenericQuery (..))
import Kronor.SmartCsv.Query qualified as SmartCsvQuery
import Kronor.SmartCsv.Statements qualified as SmartCsvStatements
import Kronor.SmartCsv.TokenClaims qualified as SmartCsvTokenClaims
import RIO hiding ((%~), (.~), (^.), (^..), (^?))
import RIO.List (headMaybe)
import RIO.Vector qualified as Vector
import SmartCsvRunner.AWS.Types
import SmartCsvRunner.CsvGeneration.Generate qualified as Generate
import SmartCsvRunner.Env (Options (..))
import SmartCsvRunner.Job (Job, JobEnv (jobEnv), getJobId)
import SmartCsvRunner.Job qualified as Job
import SmartCsvRunner.Job.SmartCsvEnv (SmartCsvEnv (..))
import SmartCsvRunner.Job.Type (JobProcessor (..), subJobEnv)
import Text.URI (mkURI)

logSource :: LogSource
logSource = "smart-csv-runner:SmartCsvRunner.JobHandlers.SmartGenerateCsv"

instance JobProcessor SmartCsvEnv SmartGraphqlCsvGenerate where
  processJob (SmartGraphqlCsvGenerate payload) = subJobEnv (\env -> (smartCsvS3Config env, smartCsvOptions env)) do
    generateCSV payload
  closeJob (SmartGraphqlCsvGenerate _) = subJobEnv smartCsvS3Config do
    Job.giveupS logSource "Payment CSV generation job was closed"

resolver :: Text -> ByteString -> LByteString -> Job a LByteString
resolver graphqlUrl token b = do
  let muri = Req.useURI =<< mkURI graphqlUrl
  maybe
    (Job.giveupS logSource "Could not build graphql url.")
    ( either
        runRequest
        runRequest
    )
    muri
  where
    runRequest :: (Req.Url scheme, Req.Option scheme) -> Job a LByteString
    runRequest (uri, options) =
      Req.responseBody
        <$> Req.run
          Req.defaultHttpConfig
          Req.POST
          uri
          (Req.ReqBodyLbs b)
          Req.lbsResponse
          ( options
              <> mconcat
                [ Req.header "Authorization" $ "Bearer " <> token,
                  Req.header "Content-Type" "application/json"
                ]
          )

type CsvRow = Map Text Csv.Field

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
      let paginationKey = SmartCsvQuery.resolvePaginationKey gq
      mSignedLink <-
        subJobEnv fst
          $ Generate.generateCsv
            (gqlQuery resolvedColumnConfig paginationKey authToken gq inferredRoot emptyMap 1000 options.optionsGraphqlUrl)
            (pure True)
            (Vector.fromList (encodeUtf8 <$> inferredHeadersFromGql))
            (gqlCursor resolvedColumnConfig paginationKey pId inferredRoot)
            id
            fileKey
            generatedCsvPayload
      sendCsvDoneEmail recipient mSignedLink
    Failure errs -> do
      Job.giveupS logSource (displayBytesUtf8 (toStrictBytes (Aeson.encode errs)))
  where
    gqlCursor :: Map Text (Maybe Text) -> Aeson.Key -> Job.PayloadId -> Text -> CsvRow -> Text
    gqlCursor colConfig pKey pId root v =
      case SmartCsv.extractCursor colConfig root (Aeson.Key.toText pKey) v of
        Left cursorErr ->
          case SmartCsvErrorHandling.classifyCursorError cursorErr of
            SmartCsvErrorHandling.Retry _ -> error "Unexpected retry for cursor error"
            SmartCsvErrorHandling.Giveup msg ->
              Control.Exception.throw $ Job.NonRetryableException pId $ Job.StringyException logSource msg
        Right cursor -> cursor
    gqlQuery :: Map Text (Maybe Text) -> Aeson.Key -> ByteString -> GenericQuery -> Text -> CsvRow -> Int -> Text -> Maybe Text -> Job S3Config (Vector CsvRow)
    gqlQuery colConfig pKey authToken GenericQuery {..} root emptyCsvRow batchSize graphqlUrl mCursor = do
      let reqBody = SmartCsvQuery.buildRequestBody pKey batchSize mCursor GenericQuery {..}
      eRes <-
        Aeson.eitherDecode <$> resolver graphqlUrl authToken reqBody
      case eRes of
        Right (r :: Aeson.Value) -> do
          let logResponseBody = logWarn (displayBytesUtf8 (toStrictBytes (Aeson.encode r)))
          case SmartCsvQuery.decodeResponseRows colConfig root emptyCsvRow r of
            Right rows -> pure rows
            Left responseErr -> do
              logResponseBody
              case SmartCsvErrorHandling.classifyResponseError responseErr of
                SmartCsvErrorHandling.Retry msg -> retry (display msg)
                SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)
        Left err ->
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

genTokenFromClaims :: Aeson.Value -> Job a ByteString
genTokenFromClaims tokenClaims = do
  parsedClaims <-
    either
      ( \err -> case SmartCsvErrorHandling.classifyTokenClaimsError err of
          SmartCsvErrorHandling.Retry msg -> retry (display msg)
          SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)
      )
      pure
      (SmartCsvTokenClaims.parseTokenClaims tokenClaims)
  token <-
    Db.readOr
      (retry . displayShow)
      ( Db.statement
          (parsedClaims.associatedEmail, parsedClaims.hasuraClaims, parsedClaims.tokenType, parsedClaims.tokenId)
          SmartCsvStatements.signJwtFromClaims
      )
  return $ Text.encodeUtf8 token

retry :: Utf8Builder -> Job env a
retry s = withFrozenCallStack do
  timeUntilRetry <- Job.defaultTimeUntilNextAttempt <$> asks Job.jobFailedAttempts
  Job.retryS logSource timeUntilRetry s
