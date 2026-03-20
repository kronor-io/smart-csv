module SmartCsvRunner.Cli
  ( buildS3Config,
    InitEnv (..),
    mkEmailServer,
    workerLoop,
  )
where

import Amazonka qualified as AwsSdk
import Amazonka.S3 qualified as AwsS3
import Colog.Json qualified
import Control.Exception.Annotated.UnliftIO qualified as AnnException
import Data.Aeson qualified as Aeson
import Data.Annotation qualified as Annotation
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Hasql.Connection qualified
import Hasql.Pool qualified
import Kronor.Logger (RequestId (..))
import Kronor.Logger qualified
import Kronor.Tracer qualified
import RIO
import RIO.Process qualified as Process
import RIO.Text qualified as Text
import SmartCsvRunner.AWS qualified as Aws
import SmartCsvRunner.AWS.Types (S3Config (..))
import SmartCsvRunner.Dequeuer qualified as Dequeuer
import SmartCsvRunner.Dispatcher (JobItem (..), Meta (..))
import SmartCsvRunner.Env (Options (..))
import SmartCsvRunner.Job (JobEnv (..), PayloadId (..), giveupS)
import SmartCsvRunner.Job qualified as Job
import SmartCsvRunner.Job.Payload qualified as Payload
import SmartCsvRunner.Job.SmartCsvEnv (SmartCsvEnv)
import SmartCsvRunner.Job.Type (JobPayloadAnnotation (..), JobProcessorF (..), jobName)
import SmartCsvRunner.JobHandlers.Email qualified as Email
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Streamly
import System.Environment qualified as Env
import System.Timeout qualified

data InitEnv = InitEnv
  { initLogFunc :: Colog.Json.LoggerEnv -> LogFunc,
    initLogEnv :: Colog.Json.LoggerEnv,
    initProcessContext :: Process.ProcessContext
  }

instance HasLogFunc InitEnv where
  logFuncL = lens (\x -> x.initLogFunc x.initLogEnv) (\x _ -> x)

instance Process.HasProcessContext InitEnv where
  processContextL = lens initProcessContext (\x y -> x {initProcessContext = y})

buildS3Config :: Aws.Env -> IO (S3Config, Aws.Env)
buildS3Config awsEnv = do
  bucket <- maybe "kronor-local" fromString <$> Env.lookupEnv "KRONOR_S3_BUCKET"
  expiry <- fromMaybe 3600 . (>>= readMaybe) <$> Env.lookupEnv "KRONOR_SIGNED_URL_EXPIRY_TIME_IN_SECONDS"
  -- Support local minio endpoint override for testing
  mS3Host <- Env.lookupEnv "KRONOR_TEST_S3_ENDPOINT_HOSTNAME"
  mS3Port <- Env.lookupEnv "KRONOR_TEST_S3_ENDPOINT_PORT"
  let awsEnv' = case (mS3Host, mS3Port >>= readMaybe) of
        (Just host, Just port) ->
          AwsSdk.configureService
            ( AwsS3.defaultService {AwsSdk.s3AddressingStyle = AwsSdk.S3AddressingStylePath}
                & AwsSdk.setEndpoint False (BS.toStrict (fromString host)) port
            )
            awsEnv
        _ -> awsEnv
  pure
    ( S3Config
        { bucket = AwsS3.BucketName bucket,
          signedUrlExpiryTime = AwsSdk.Seconds (fromIntegral (expiry :: Int)),
          presignAwsEnv = pure awsEnv',
          presignUserName = "",
          presignUserSecretId = ""
        },
      awsEnv'
    )

mkEmailServer :: Options -> Email.EmailServer
mkEmailServer options
  | options.optionsMailSES = Email.EmailServerProdSES
  | options.optionsMailDev =
      Email.EmailServerDev
        { emailServerHost = Text.unpack $ fromMaybe "localhost" options.optionsMailHost,
          emailServerPort = fromIntegral $ fromMaybe 1025 options.optionsMailPort
        }
  | otherwise =
      Email.EmailServerProdSMTP
        { emailServerHost = Text.unpack $ fromMaybe "localhost" options.optionsMailHost,
          emailUserName = fromMaybe "" options.optionsMailUser,
          emailPassword = fromMaybe "" options.optionsMailPassword
        }

-- | Data type to distinguish between database notifications and poll timeout signals
data NotificationOrPoll
  = NotificationReceived
  | PollForJobs

-- | Create a Streamly stream of notifications from the database or poll timeouts.
-- Listens for "job_created" channel notifications, with a fallback to polling every 3 seconds.
getDatabaseNotifications ::
  Hasql.Connection.Connection ->
  Streamly.Stream (Job.Job SmartCsvEnv) NotificationOrPoll
getDatabaseNotifications conn = do
  let channels = [("job_created" :: Text, const NotificationReceived :: BS.ByteString -> NotificationOrPoll)]
  -- Stream that emits PollForJobs immediately and then repeatedly either:
  -- - waits for a database notification on any of the channels, or
  -- - times out after 3 seconds and emits PollForJobs
  PollForJobs
    `Streamly.cons` Streamly.repeatM do
      mbNotif <- liftIO $ System.Timeout.timeout 3_000_000 (Payload.notifyPayload (first encodeUtf8 <$> channels) conn)
      pure $ fromMaybe PollForJobs mbNotif

-- | Main worker loop: listen for job notifications and process jobs.
-- Uses Streamly instead of polling with a hard 1-second sleep.
-- The dequeuerPool provides a separate connection per job so that concurrent
-- workers don't share a single connection for BEGIN/COMMIT transactions.
-- The listenerConn is only used for LISTEN/NOTIFY and job count queries.
workerLoop :: Hasql.Pool.Pool -> Hasql.Connection.Connection -> Int -> Job.Job SmartCsvEnv ()
workerLoop dequeuerPool listenerConn retries = do
  let config = Streamly.maxThreads 2 . Streamly.maxBuffer 10
  Streamly.fold Fold.drain do
    Streamly.parConcatMap
      config
      (\_ -> Streamly.mapM (const $ dequeueJob dequeuerPool retries) (Dequeuer.streamJobsCount listenerConn))
      (getDatabaseNotifications listenerConn)

-- | Acquire a dedicated connection from the dequeuer pool and run the
-- dequeue + job processing cycle on it. This ensures each concurrent worker
-- thread operates on its own connection, avoiding shared-transaction conflicts.
dequeueJob :: Hasql.Pool.Pool -> Int -> Job.Job SmartCsvEnv ()
dequeueJob pool retries = do
  env <- ask
  result <- liftIO $ Hasql.Pool.withConn pool $ \conn ->
    runRIO env $ Dequeuer.withDequeue conn retries (pure False) doJob
  case result of
    Left err -> do
      logErrorS "smart-csv-runner:Cli" ("Dequeuer pool error: " <> displayShow err)
    Right () -> pure ()

doJob :: (HasCallStack) => Payload.Payload -> Job.Job SmartCsvEnv ()
doJob payload =
  Kronor.Logger.withAddedNamespace "smart-csv-runner" do
    jobEnv0 <- ask
    case Aeson.fromJSON payload.pValue of
      Aeson.Error s ->
        giveupS
          "smart-csv-runner:Cli"
          ("Decoding json payload failed: " <> fromString s)
      Aeson.Success (JobItem (Meta reqId meta) jt) -> do
        let name = jobName jt
        let jId = coerce payload.pId :: Int64
        let RequestId requestId = reqId
        let jobAttributes =
              [ ("job_type", Kronor.Tracer.toAttribute name),
                ("job_id", Kronor.Tracer.toAttribute jId),
                ("hasura.request.id", Kronor.Tracer.toAttribute requestId)
              ]
        let jobAttributesMap = Kronor.Tracer.convertToLogAttributes jobAttributes
        let jobEnv1 =
              jobEnv0
                { jobId = payload.pId,
                  jobFailedAttempts = fromIntegral payload.pAttempts,
                  jobTracing = reqId
                }

        AnnException.checkpoint (Annotation.toAnnotation (JobPayloadAnnotation (jobAttributesMap <> meta))) do
          Kronor.Tracer.withConsumerTrace name (Kronor.Tracer.extractRemoteContext meta) do
            Kronor.Tracer.addTopSpanTags jobAttributes
            logAttributes <- Kronor.Tracer.getLogAttributes
            Kronor.Logger.withAddedContextMap (meta <> logAttributes <> jobAttributesMap) do
              runRIO jobEnv1 $ if payload.pExpired then expireJobF jt else processJobCallMeF jt
