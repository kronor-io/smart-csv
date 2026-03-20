{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvRunner.CsvGeneration.Generate (generateCsv, onError, getDbPage) where

import Amazonka qualified as Aws
import Amazonka.S3 qualified as Aws
import Data.Aeson qualified as Aeson
import Data.Csv qualified as Csv
import Hasql.TH
import JobSchemas.GenerateSettlementCsv
import Kronor.Db qualified
import Kronor.Db qualified as Db
import Kronor.Db.Error.Common (clarifyError)
import Kronor.Db.Error.Common qualified as Db
import Kronor.Db.Types.Bigint (toInt64)
import RIO
import RIO.Time
import SmartCsvRunner.AWS.S3 qualified as Utilities
import SmartCsvRunner.AWS.Types
import SmartCsvRunner.Job (Job, giveupS, jobEnv)
import SmartCsvRunner.Job qualified as Job
import SmartCsvRunner.MultipartUpload (ProcessingError (..), ReportGenerationRow (..), multiPartUploadFromPagination)
import SmartCsvRunner.ReportLink (ReportLinkStatus (..), statusToText)

logSource :: LogSource
logSource = "smart-csv-runner:SmartCsvRunner.CsvGeneration.Generate"

tableName :: Text
tableName = "smart_csv.generated_csv"

generateCsv ::
  forall env cursor r.
  (Csv.ToNamedRecord r) =>
  (HasS3Config env) =>
  (HasCallStack) =>
  (Maybe cursor -> Job env (Vector r)) ->
  Db.Transaction Bool ->
  Vector Csv.Name ->
  (r -> cursor) ->
  (cursor -> Text) ->
  Text ->
  Payload ->
  Job env (Maybe Text)
generateCsv fetchPage transactionsTest transactionsHeader getCursor toText fileKey payload@Payload {shardId, reportId, startDate, endDate, stateMachineId} = do
  Db.writeOr (retryS <=< clarifyError tableName) do
    Kronor.Db.statement
      (toInt64 payload.shardId, stateMachineId, Aeson.toJSON payload)
      [resultlessStatement|
                select from
                fsm.notify_state_machine(
                  $1::bigint,
                  $2::bigint,
                  'csv_report.generate',
                  $3::jsonb
              )
              |]

  transactionsExist <-
    Db.readOr
      (giveupS logSource . displayShow <=< clarifyError "Failed to check for existence of transactions")
      transactionsTest

  if transactionsExist
    then do
      s3Config <- asks (\x -> x.jobEnv ^. getS3ConfigL)

      let reportGenerationRow =
            ReportGenerationRow
              { reportPath = fileKey,
                status = INITIALIZED,
                bucketName = s3Config.bucket ^. Aws._BucketName,
                lastPaginationKey = Nothing,
                uploadId = Nothing,
                lastUploadedPart = Nothing,
                partEntity = "",
                startDate,
                endDate,
                count = 0
              }

      eAwsS3MultiUpload <-
        multiPartUploadFromPagination
          reportGenerationRow
          fetchPage
          getCursor
          transactionsHeader
          fetchPartEntities
          updateProgress
          onSuccess

      case eAwsS3MultiUpload of
        Left EmptySet -> do
          onError' "Upload error: no rows to report"
          Job.giveupS logSource "No rows to report (EmptySet), but an AWS bucket was created (should be avoided)"
        Left err -> do
          onError' "AWS S3 upload error"
          Job.giveupS logSource ("Failed to upload to S3: " <> displayShow err)
        Right signedUrl -> do
          logGeneric logSource LevelInfo "Uploaded files to S3"
          pure (Just signedUrl)
    else do
      Db.writeOr (retryS <=< clarifyError tableName) do
        Kronor.Db.statement
          reportId
          [resultlessStatement|
                    update smart_csv.generated_csv
                    set status = 'DONE'
                    where id = $1::bigint
                |]

        Kronor.Db.statement
          ( toInt64 payload.shardId,
            stateMachineId,
            Aeson.toJSON payload
          )
          [resultlessStatement|
                  select from fsm.notify_state_machine ($1::bigint, $2::bigint, 'csv_report.done', $3::jsonb)
                |]
      pure Nothing
  where
    onError' = onError payload

    onSuccess :: ReportGenerationRow cursor -> Job env Text
    onSuccess rgr = do
      s3Config <- asks (\x -> x.jobEnv ^. getS3ConfigL)
      time <- liftIO getCurrentTime
      awsEnvForPresign <- presignAwsEnv s3Config
      eSignedUrl <- Utilities.presign awsEnvForPresign s3Config.bucket (Aws.ObjectKey rgr.reportPath) time s3Config.signedUrlExpiryTime

      case eSignedUrl of
        Left err -> do
          onError' "AWS Presigning failed"
          Job.giveupS logSource ("Failed to presign S3 URL: " <> displayShow err)
        Right (decodeUtf8Lenient -> signedReportUrl) -> do
          -- Add the expiry time to current time to store the exact time of expiry of presigned link in the db
          let expiryTime = addUTCTime (realToFrac $ Aws.toSeconds s3Config.signedUrlExpiryTime) time

          Db.writeOr (retryS <=< clarifyError tableName) do
            Kronor.Db.statement
              (reportId, signedReportUrl, expiryTime, rgr.reportPath)
              [resultlessStatement|
                            update smart_csv.generated_csv
                            set status = 'DONE',
                                link = $2::text,
                                expires_at = $3::timestamptz,
                                file_path = $4::text
                            where id = $1::bigint
                          |]

            Kronor.Db.statement
              ( toInt64 payload.shardId,
                stateMachineId,
                Aeson.toJSON payload
              )
              [resultlessStatement|
                          select from fsm.notify_state_machine ($1::bigint, $2::bigint, 'csv_report.done', $3::jsonb)
                          |]

          logGeneric logSource LevelDebug $ "Presign Link: " <> displayShow signedReportUrl

          pure signedReportUrl

    fetchPartEntities :: Job env (Vector ByteString)
    fetchPartEntities =
      Kronor.Db.readOr (retryS <=< clarifyError tableName) do
        Kronor.Db.statement
          reportId
          [singletonStatement|
                    select part_entities::bytea[]
                    from smart_csv.generated_csv
                    where id = $1::bigint
                |]

    updateProgress ::
      ReportGenerationRow cursor -> Job env ()
    updateProgress r =
      Kronor.Db.writeOr
        (retryS <=< clarifyError tableName)
        do
          Kronor.Db.statement
            ( toInt64 shardId,
              reportId,
              r.reportPath,
              statusToText r.status,
              r.bucketName,
              toText r.lastPaginationKey,
              r.uploadId,
              r.lastUploadedPart,
              r.partEntity,
              fromIntegral r.count
            )
            [resultlessStatement|
                        update smart_csv.generated_csv
                        set
                            file_path = $3::text,
                            status = $4::text,
                            bucket_name = $5::text,
                            last_pagination_key = $6::text,
                            upload_id = $7::text?,
                            last_uploaded_part = $8::int?,
                            last_updated_at = now(),
                            part_entities = case  ($9::bytea = '')
                                when true then generated_csv.part_entities
                                else generated_csv.part_entities || $9::bytea
                                end,
                            err_message = null,
                            number_of_rows = generated_csv.number_of_rows + $10::int
                        where
                            shard_id = $1::bigint
                            and id = $2::bigint
                    |]

onError :: Payload -> Text -> Job env ()
onError payload errMessage = do
  Db.writeOr (retryS <=< clarifyError "fsm.notify_state_machine:csv_report.error") do
    Kronor.Db.statement
      ( payload.reportId,
        errMessage
      )
      [resultlessStatement|
                update smart_csv.generated_csv
                set status = 'ERROR',
                    err_message = $2::text
                where id = $1::bigint
            |]

    Kronor.Db.statement
      ( toInt64 payload.shardId,
        payload.stateMachineId,
        payload.reportId
      )
      [resultlessStatement|
              select from
              fsm.notify_state_machine(
                $1::bigint,
                $2::bigint,
                'csv_report.error',
                jsonb_build_object(
                    'reportId', $3::bigint
                )
              )
            |]

retryS :: Text -> Job env a
retryS err = do
  failedAttempts <- asks Job.jobFailedAttempts
  Job.retryS
    logSource
    (Job.defaultTimeUntilNextAttempt failedAttempts)
    (display err)

getDbPage :: (cursor -> Db.Transaction a) -> cursor -> Job env a
getDbPage transactionsQuery mCursor = do
  dbResult <- Db.readOnNamedPool "csv-replica" (transactionsQuery mCursor)
  timeUntilRetry <- Job.defaultTimeUntilNextAttempt <$> asks Job.jobFailedAttempts
  clarifiedResult <- Db.clarifyResult tableName dbResult
  case clarifiedResult of
    Left err -> Job.retryS "kronor-worker:Aws.MultipartUpload" timeUntilRetry (display err)
    Right res -> pure res
