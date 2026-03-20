module SmartCsvRunner.ThreadManager
  ( startAllServices,
  )
where

import Colog.Json qualified
import Colog.Json.Action (logToHandle)
import Control.Immortal qualified as Immortal
import Hasql.Connection qualified
import Hasql.Pool qualified as Pool
import Hasql.Session qualified
import Kronor.Logger qualified
import Kronor.Tracer qualified
import Network.HTTP.Client.TLS as HTTPS
import RIO
import RIO.Process qualified as Process
import RIO.Vector qualified as Vector
import SmartCsvApi.Env (ApiEnv (..), ApiOptions (..))
import SmartCsvApi.RestServer qualified as RestServer
import SmartCsvRunner.AWS qualified as Aws
import SmartCsvRunner.Cli
  ( InitEnv (..),
    buildS3Config,
    mkEmailServer,
    workerLoop,
  )
import SmartCsvRunner.Dequeuer qualified as Dequeuer
import SmartCsvRunner.Env (Env (..), Options (..), loadOptions)
import SmartCsvRunner.Job (JobEnv (..), PayloadId (..))
import SmartCsvRunner.Job.SmartCsvEnv (mkSmartCsvEnv)
import System.IO qualified

-- | Start all services (API server and Worker) in separate immortal threads
startAllServices :: IO ()
startAllServices = do
  -- Setup basic environment
  System.IO.hSetBuffering stdout NoBuffering

  -- Load options - these options may have API configuration
  options <- loadOptions

  processCtx <- Process.mkDefaultProcessContext

  let loggerEnv = Colog.Json.mkLogger (logToHandle stderr)
      mkLf = mkLogFunc . Kronor.Logger.jsonLogFunc LevelDebug

  -- Initialize AWS environment (with S3 endpoint override for minio)
  baseAwsEnv <- runRIO (InitEnv mkLf loggerEnv processCtx) Aws.initAwsEnv
  (s3Config, awsEnv) <- buildS3Config baseAwsEnv

  -- Initialize OpenTelemetry tracer and run everything inside its scope
  Kronor.Tracer.withGlobalTracer "smart-csv-runner" \tracer -> do
    let workerEnv =
          Env
            { envLogFunc = mkLf,
              envLogEnv = loggerEnv,
              envProcessContext = processCtx,
              envOptions = options,
              envS3Config = s3Config
            }

    (dequeuerPool, _) <- runRIO workerEnv options.optionsPgPoolDequeuer
    workerPool <- runRIO workerEnv options.optionsPgPoolWorker
    readCsvPool <- runRIO workerEnv options.optionsPgPoolReplicaCSV
    listenerSettings <- runRIO workerEnv options.optionsListenerConn

    eConn <- Hasql.Connection.acquire listenerSettings
    conn <- case eConn of
      Left err -> throwString $ "Could not acquire listener connection: " <> show err
      Right c -> pure c

    _ <- Hasql.Session.run (Hasql.Session.sql "SET search_path TO job_queue,public") conn
    Dequeuer.startListening (Vector.fromList ["job_created"]) conn

    let emailServer = mkEmailServer options
        smartCsvEnv = mkSmartCsvEnv s3Config options emailServer

    httpManager <- HTTPS.newTlsManager
    let apiEnv =
          ApiEnv
            { envDbPool = workerPool,
              envHttpManager = httpManager,
              envGraphqlUrl = options.optionsGraphqlUrl,
              envPortalUrl = options.optionsPortalUrl,
              envJwtSecret = options.optionsJwtSecret
            }

    let apiOpts =
          ApiOptions
            { apiHost = options.optionsApiHost,
              apiPort = options.optionsApiPort,
              graphqlUrl = options.optionsGraphqlUrl,
              portalUrl = options.optionsPortalUrl,
              jwtSecret = options.optionsJwtSecret,
              logLevel = options.optionsLogLevel
            }

    apiThread <- Immortal.create $ \_ -> do
      RestServer.startRestApiServer apiEnv apiOpts

    workerThread <- Immortal.createWithLabel "worker" $ \_ ->
      do
        System.IO.hPutStrLn System.IO.stderr "[smart-csv-runner] Worker thread starting"
        System.IO.hFlush System.IO.stderr
        runResult <- Pool.withConn dequeuerPool $ \jobConn -> do
          let jobEnv =
                JobEnv
                  { jobEnv = smartCsvEnv,
                    jobLogFunc = mkLf,
                    jobLogEnv = loggerEnv,
                    jobProcessContext = processCtx,
                    jobPgPool = workerPool,
                    jobPgReadCSVPool = readCsvPool,
                    jobTracing = mempty,
                    jobId = PayloadId (-1),
                    jobThreadConnection = jobConn,
                    jobFailedAttempts = 0,
                    jobTracer = tracer,
                    jobTopSpan = Nothing,
                    jobCurrentSpan = Nothing,
                    jobAwsEnv = awsEnv
                  }

          runRIO jobEnv $ workerLoop dequeuerPool conn options.optionsNumRetries

        case runResult of
          Left err -> do
            System.IO.hPutStrLn System.IO.stderr $ "[smart-csv-runner] Dequeuer pool error: " <> show err
            System.IO.hFlush System.IO.stderr
            throwString $ "Dequeuer pool error: " <> show err
          Right () -> pure ()
        `catch` \(e :: SomeException) -> do
          System.IO.hPutStrLn System.IO.stderr $ "[smart-csv-runner] Worker thread crashed: " <> displayException e
          System.IO.hFlush System.IO.stderr
          throwIO e

    _ <- Immortal.wait apiThread
    _ <- Immortal.wait workerThread
    pure ()
