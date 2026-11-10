{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OrPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvRunner.JobHandlers.SmartGenerateCsv (SmartGraphqlCsvGenerate) where

import Control.Exception qualified
import Control.Exception.Annotated (AnnotatedException (..))
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LByteString
import Data.Coerce (coerce)
import Data.Csv.Builder qualified as Csv.Builder
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
import Kronor.SmartCsv.ExportFormat (ExportFormat (..))
import Kronor.SmartCsv.ExportFormat qualified as ExportFormat
import Kronor.SmartCsv.Notification qualified as SmartCsvNotification
import Kronor.SmartCsv.Pagination qualified as SmartCsv
import Kronor.SmartCsv.Query (EncodedResponsePage (..), GenericQuery (..))
import Kronor.SmartCsv.Query qualified as SmartCsvQuery
import Kronor.SmartCsv.Schema qualified as SmartCsvSchema
import Kronor.SmartCsv.Statements qualified as SmartCsvStatements
import Kronor.SmartCsv.TokenClaims qualified as SmartCsvTokenClaims
import Kronor.SmartCsv.Xlsx qualified as Xlsx
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
import System.Timeout qualified

logSource :: LogSource
logSource = "smart-csv-runner:SmartCsvRunner.JobHandlers.SmartGenerateCsv"

instance JobProcessor SmartCsvEnv SmartGraphqlCsvGenerate where
  processJob (SmartGraphqlCsvGenerate payload) = subJobEnv (\env -> (smartCsvS3Config env, smartCsvOptions env)) do
    generateCSV payload
  closeJob (SmartGraphqlCsvGenerate _) = subJobEnv smartCsvS3Config do
    Job.giveupS logSource "Payment CSV generation job was closed"

graphqlChunkTargetBytes :: Int64
graphqlChunkTargetBytes = 5_242_880

-- | zlib's default level.  Each page is compressed as it arrives, ahead of the
-- uploads, so this is on the fetch path; it costs about 10 ms per 1000 rows.
xlsxCompressionLevel :: Int
xlsxCompressionLevel = 6

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
  (gq, tokenClaims, recipient, mInlineConfig, mConfigName, detectNumericColumns, fileFormatText) <-
    Db.readOr
      (retry . displayShow)
      ( Db.statement
          (coerce payload.shardId, payload.csvId)
          SmartCsvStatements.selectGeneratorConfig
      )
  recordNonRetryableFailure generatedCsvPayload do
    exportFormat <-
      maybe
        (Job.giveupS logSource ("Unknown export format " <> display fileFormatText))
        pure
        (ExportFormat.parseExportFormat fileFormatText)
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
            fileName = inferredRoot <> "." <> ExportFormat.exportFormatExtension exportFormat
            s3Path = [iii|smartPaymentCsv/#{shardId payload}/#{csvId payload}/|] :: Text
            fileKey = s3Path <> fileName
            inferredHeadersFromGql = SmartCsv.inferHeaders resolvedColumnConfig inferredRootField
            emptyMap = Map.fromList ((,mempty) <$> inferredHeadersFromGql)
        authToken <- genTokenFromClaims tokenClaims
        httpManager <- liftIO $ Http.newManager Http.Tls.tlsManagerSettings
        columnConfig <-
          if detectNumericColumns
            then markNumericColumnsFromSchema httpManager options.optionsGraphqlUrl authToken resolvedColumnConfig inferredRootField
            else pure resolvedColumnConfig
        let paginationFields = SmartCsvQuery.resolvePaginationFields gq
            fetchRows encodeRow = gqlQuery httpManager pId inferredRootField paginationFields authToken gq inferredRoot encodeRow options.optionsGraphqlPageSize options.optionsGraphqlUrl
        (fetchPage, filePrefix, maxRows) <- case exportFormat of
          Csv -> do
            let csvHeader = Vector.fromList (encodeUtf8 <$> inferredHeadersFromGql)
            pure
              ( fetchRows (SmartCsvQuery.csvRowBytes columnConfig inferredRoot emptyMap csvHeader),
                Builder.toLazyByteString (Csv.Builder.encodeHeader csvHeader),
                options.optionsCsvGenerationMaxRows
              )
          Xlsx -> do
            let styles = Xlsx.mkXlsxStyles columnConfig
                xlsxHeaders = Vector.fromList inferredHeadersFromGql
            (archivePrefix, archive, sheetEntry) <- liftIO (Xlsx.startArchive xlsxCompressionLevel inferredRoot styles xlsxHeaders)
            sheetEntryRef <- newIORef (Just sheetEntry)
            pure
              ( xlsxPages archive sheetEntryRef (fetchRows (Xlsx.xlsxRowBytes columnConfig styles xlsxHeaders)),
                archivePrefix,
                min options.optionsCsvGenerationMaxRows Xlsx.excelMaxDataRows
              )
        let generateAction =
              subJobEnv fst
                $ Generate.generateCsv
                  fetchPage
                  (pure True)
                  filePrefix
                  SmartCsv.encodePaginationCursor
                  fileKey
                  (ExportFormat.exportFormatContentType exportFormat)
                  maxRows
                  generatedCsvPayload
            timeoutSeconds = options.optionsCsvGenerationTimeoutSeconds
        jobEnvForTimeout <- ask
        mSignedLinkOrTimeout <-
          if timeoutSeconds <= 0
            then Just <$> liftIO (runRIO jobEnvForTimeout generateAction)
            else do
              let timeoutMicros = timeoutSeconds * 1_000_000
              liftIO $ System.Timeout.timeout timeoutMicros (runRIO jobEnvForTimeout generateAction)
        mSignedLink <- case mSignedLinkOrTimeout of
          Nothing -> do
            let msg = csvGenerationTimeoutMessage options.optionsCsvGenerationTimeoutSeconds
            subJobEnv fst $ Generate.onError generatedCsvPayload msg
            Job.giveupS logSource (display msg)
          Just result -> pure result
        sendCsvDoneEmail recipient (ExportFormat.exportFormatFileKind exportFormat) mSignedLink
      Failure errs -> do
        Job.giveupS logSource (displayBytesUtf8 (toStrictBytes (Aeson.encode errs)))
  where
    csvGenerationTimeoutMessage timeoutSeconds =
      "CSV generation exceeded timeout: maximum is "
        <> tshow timeoutSeconds
        <> " seconds"

    gqlCursor :: Job.PayloadId -> Selection RAW -> NonEmpty SmartCsv.PaginationField -> Aeson.Value -> SmartCsv.PaginationCursor
    gqlCursor pId rootSelection paginationFields v =
      case SmartCsv.extractCursorValue rootSelection paginationFields v of
        Left cursorErr ->
          case SmartCsvErrorHandling.classifyCursorError cursorErr of
            SmartCsvErrorHandling.Retry _ -> error "Unexpected retry for cursor error"
            SmartCsvErrorHandling.Giveup msg ->
              Control.Exception.throw $ Job.NonRetryableException pId $ Job.StringyException logSource msg
        Right cursor -> cursor
    gqlQuery :: Http.Manager -> Job.PayloadId -> Selection RAW -> NonEmpty SmartCsv.PaginationField -> ByteString -> GenericQuery -> Text -> (Aeson.Value -> LByteString) -> Int -> Text -> Maybe SmartCsv.PaginationCursor -> Job S3Config (Maybe (EncodedCsvPage SmartCsv.PaginationCursor))
    gqlQuery httpManager pId rootSelection paginationFields authToken GenericQuery {..} root encodeRow batchSize graphqlUrl mCursor = do
      let reqBody = SmartCsvQuery.buildRequestBody paginationFields batchSize mCursor GenericQuery {..}
      eRes <- streamResponsePage httpManager graphqlUrl authToken reqBody root encodeRow
      case eRes of
        Right page ->
          pure $ case page.lastRawRow of
            Nothing -> Nothing
            Just lastParsedRow ->
              Just
                EncodedCsvPage
                  { encodedRows = page.encodedRows,
                    encodedRowBytes = page.encodedRowBytes,
                    lastCursor = gqlCursor pId rootSelection paginationFields lastParsedRow,
                    rowCount = page.rowCount
                  }
        Left responseErr ->
          case SmartCsvErrorHandling.classifyResponseError responseErr of
            SmartCsvErrorHandling.Retry msg -> retry (display msg)
            SmartCsvErrorHandling.Giveup msg -> Job.giveupS logSource (display msg)

    streamResponsePage :: Http.Manager -> Text -> ByteString -> LByteString -> Text -> (Aeson.Value -> LByteString) -> Job S3Config (Either SmartCsvQuery.ResponseError EncodedResponsePage)
    streamResponsePage httpManager graphqlUrl authToken reqBody root encodeRow = do
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
              else Right <$> SmartCsvQuery.encodeResponseChunkWith graphqlChunkTargetBytes root encodeRow (Http.responseBody response)
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

-- | Compress each page into the sheet as it arrives, so a page's raw XML never
-- waits for an upload, and once the rows run out hand back the end of the
-- archive as one last page: being last, it may be smaller than an S3 part has to be.
xlsxPages ::
  Xlsx.XlsxArchive ->
  IORef (Maybe Xlsx.SheetEntry) ->
  (Maybe cursor -> Job env (Maybe (EncodedCsvPage cursor))) ->
  Maybe cursor ->
  Job env (Maybe (EncodedCsvPage cursor))
xlsxPages archive sheetEntryRef fetchPage mCursor =
  readIORef sheetEntryRef >>= \case
    -- The archive was closed by the previous call.
    Nothing -> pure Nothing
    Just sheetEntry ->
      fetchPage mCursor >>= \case
        Just page -> do
          (segment, sheetEntry') <- liftIO (Xlsx.appendRows archive sheetEntry page.encodedRows)
          unless (Xlsx.archiveFits archive sheetEntry')
            $ Job.giveupS logSource "Excel file exceeded 4 GB, the most an XLSX file without ZIP64 can hold"
          writeIORef sheetEntryRef (Just sheetEntry')
          pure
            $ Just
              EncodedCsvPage
                { encodedRows = segment,
                  encodedRowBytes = LByteString.length segment,
                  lastCursor = page.lastCursor,
                  rowCount = page.rowCount
                }
        Nothing -> case mCursor of
          -- No rows at all: there is no file to close, the export is empty.
          Nothing -> pure Nothing
          Just cursor -> do
            trailer <- liftIO (Xlsx.finishArchive archive sheetEntry)
            writeIORef sheetEntryRef Nothing
            pure
              $ Just
                EncodedCsvPage
                  { encodedRows = trailer,
                    encodedRowBytes = LByteString.length trailer,
                    lastCursor = cursor,
                    rowCount = 0
                  }

-- | A giveup only logs and marks the *job* failed; the report row keeps whatever
-- status it had, so a user watching the CSV jobs page sees INITIALIZED with no
-- message however the generation actually failed. Record non-retryable failures on
-- the report before they propagate. Retryable ones are left alone — the job will
-- run again and the report is still pending.
recordNonRetryableFailure ::
  Gscv.Payload ->
  Job (S3Config, Options) a ->
  Job (S3Config, Options) a
recordNonRetryableFailure generatedCsvPayload action =
  -- Job failures travel wrapped by annotated-exception, the same shape the dequeuer
  -- matches on; rethrow the value untouched so its annotations survive.
  action `catch` \annotated@(AnnotatedException _ userException) -> do
    case userException of
      Job.NonRetryableException _ cause ->
        subJobEnv fst $ Generate.onError generatedCsvPayload (Text.pack (displayException cause))
      -- Every retryable variant: the job runs again, so the report stays pending.
      _ -> pure ()
    throwIO (annotated :: AnnotatedException Job.UserException)


-- | Flag the columns that print a numeric scalar, so a number Hasura sends as a
-- string still gets a decimal comma.  The schema is read with the export's own
-- token, so it is the schema of the role that runs the query.  When it cannot be
-- read the export goes on without it, and numbers sent as strings keep their
-- decimal point.
markNumericColumnsFromSchema :: Http.Manager -> Text -> ByteString -> ColumnConfig -> Selection RAW -> Job env ColumnConfig
markNumericColumnsFromSchema httpManager graphqlUrl authToken colConfig rootSelection = do
  requestIdValue <- asks requestId
  eSchema <- tryAny do
    request0 <- liftIO (Http.parseRequest (Text.unpack graphqlUrl))
    let request =
          request0
            { Http.method = "POST",
              Http.requestBody = Http.RequestBodyLBS SmartCsvSchema.introspectionRequestBody,
              Http.requestHeaders =
                [ ("Authorization", "Bearer " <> authToken),
                  ("Content-Type", "application/json"),
                  ("X-Request-Id", encodeUtf8 (coerce @RequestId @Text requestIdValue))
                ]
            }
    response <- liftIO (Http.httpLbs request httpManager)
    pure (Aeson.eitherDecode (Http.responseBody response) >>= SmartCsvSchema.parseIntrospectionResponse)
  case either (Left . displayException) id eSchema of
    Left err -> do
      logWarn ("Could not read the GraphQL schema, numbers sent as strings keep their decimal point: " <> fromString err)
      pure colConfig
    Right schema ->
      pure (SmartCsvSchema.markNumericColumns (SmartCsvSchema.numericColumns schema colConfig rootSelection) colConfig)

sendCsvDoneEmail :: Text -> Text -> Maybe Text -> Job a ()
sendCsvDoneEmail recipient fileKind mUrl = do
  let completionEmail = SmartCsvNotification.mkCompletionEmail fileKind mUrl
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
