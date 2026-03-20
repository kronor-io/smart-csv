module SmartCsvRunner.Env (
    Env (..),
    Options (..),
    loadOptions,
) where

import Colog.Json qualified
import Data.Version qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool.Config
import OptEnvConf hiding (env)
import OptEnvConf qualified
import RIO
import RIO.Process (ProcessContext)
import SmartCsvRunner.AWS.Types (S3Config)


data Env = Env
    { envLogFunc :: Colog.Json.LoggerEnv -> LogFunc
    , envLogEnv :: Colog.Json.LoggerEnv
    , envProcessContext :: ProcessContext
    , envOptions :: Options
    , envS3Config :: S3Config
    }


data Options = Options
    { optionsListenerConn :: RIO Env [Hasql.Connection.Setting.Setting]
    , optionsPgPoolDequeuer :: RIO Env (Pool.Pool, Int)
    , optionsPgPoolWorker :: RIO Env Pool.Pool
    , optionsPgPoolReplicaCSV :: RIO Env Pool.Pool
    , optionsGraphqlUrl :: Text
    , optionsPortalUrl :: Text
    , optionsNumRetries :: Int
    , optionsMailHost :: Maybe Text
    , optionsMailPort :: Maybe Int
    , optionsMailUser :: Maybe Text
    , optionsMailPassword :: Maybe Text
    , optionsMailDev :: Bool
    , optionsMailSES :: Bool
    , optionsApiHost :: String
    , optionsApiPort :: Int
    , optionsJwtSecret :: Text
    , optionsLogLevel :: Text
    }


data DbConnSpec = DbConnSpec
    { dbUser :: Text
    , dbPassword :: Text
    , dbDatabase :: Text
    }


loadOptions :: IO Options
loadOptions = OptEnvConf.runSettingsParser version "Run smart csv runner"
  where
    version = Data.Version.makeVersion [1, 0, 0]


instance HasParser Options where
    settingsParser =
        buildOptions
            <$> dbHostP
            <*> dbPortP
            <*> dbConnSpecP "listener" "DB_LISTENER"
            <*> dbConnSpecP "dequeuer" "DB_DEQUEUER"
            <*> dbConnSpecP "worker" "DB_WORKER"
            <*> dbConnSpecP "replica-csv" "DB_REPLICA_CSV"
            <*> dequeuerPoolSizeP
            <*> workerPoolSizeP
            <*> replicaCsvPoolSizeP
            <*> graphqlUrlP
            <*> portalUrlP
            <*> numRetriesP
            <*> mailHostP
            <*> mailPortP
            <*> mailUserP
            <*> mailPasswordP
            <*> mailDevP
            <*> mailSESP
            <*> apiHostP
            <*> apiPortP
            <*> jwtSecretP
            <*> logLevelP
      where
        buildOptions
            dbHost
            dbPort
            listenerSpec
            dequeuerSpec
            workerSpec
            replicaCsvSpec
            dequeuerPoolSize
            workerPoolSize
            replicaCsvPoolSize
            optionsGraphqlUrl
            optionsPortalUrl
            optionsNumRetries
            optionsMailHost
            optionsMailPort
            optionsMailUser
            optionsMailPassword
            optionsMailDev
            optionsMailSES
            optionsApiHost
            optionsApiPort
            optionsJwtSecret
            optionsLogLevel =
                let listenerSettings = mkDbSettings dbHost dbPort listenerSpec
                    dequeuerSettings = mkDbSettings dbHost dbPort dequeuerSpec
                    workerSettings = mkDbSettings dbHost dbPort workerSpec
                    replicaCsvSettings = mkDbSettings dbHost dbPort replicaCsvSpec
                 in Options
                        { optionsListenerConn = pure listenerSettings
                        , optionsPgPoolDequeuer = liftIO $ do
                            pool <- mkPool dequeuerPoolSize dequeuerSettings
                            pure (pool, dequeuerPoolSize)
                        , optionsPgPoolWorker = liftIO $ mkPool workerPoolSize workerSettings
                        , optionsPgPoolReplicaCSV = liftIO $ mkPool replicaCsvPoolSize replicaCsvSettings
                        , optionsGraphqlUrl
                        , optionsPortalUrl
                        , optionsNumRetries
                        , optionsMailHost
                        , optionsMailPort
                        , optionsMailUser
                        , optionsMailPassword
                        , optionsMailDev
                        , optionsMailSES
                        , optionsApiHost
                        , optionsApiPort
                        , optionsJwtSecret
                        , optionsLogLevel
                        }

        dbHostP =
            setting
                [ help "database host"
                , long "db-host"
                , value "postgres"
                , reader str
                , metavar "DB_HOST"
                , option
                , OptEnvConf.env "DB_HOST"
                ]

        dbPortP =
            setting
                [ help "database port"
                , long "db-port"
                , value 5432
                , reader auto
                , metavar "DB_PORT"
                , option
                , OptEnvConf.env "DB_PORT"
                ]

        requiredText optName envVar =
            setting
                [ help ("required env var: " <> envVar)
                , long optName
                , reader str
                , metavar envVar
                , option
                , OptEnvConf.env envVar
                ]

        dbConnSpecP role envPrefix =
            DbConnSpec
                <$> requiredText ("db-" <> role <> "-user") (envPrefix <> "_USER")
                <*> requiredText ("db-" <> role <> "-password") (envPrefix <> "_PASSWORD")
                <*> requiredText ("db-" <> role <> "-database") (envPrefix <> "_DATABASE")

        dequeuerPoolSizeP =
            setting
                [ help "dequeuer pool size"
                , long "db-dequeuer-pool-size"
                , value 8
                , reader auto
                , metavar "DB_DEQUEUER_POOL_SIZE"
                , option
                , OptEnvConf.env "DB_DEQUEUER_POOL_SIZE"
                ]

        workerPoolSizeP =
            setting
                [ help "worker pool size"
                , long "db-worker-pool-size"
                , value 8
                , reader auto
                , metavar "DB_WORKER_POOL_SIZE"
                , option
                , OptEnvConf.env "DB_WORKER_POOL_SIZE"
                ]

        replicaCsvPoolSizeP =
            setting
                [ help "replica csv pool size"
                , long "db-replica-csv-pool-size"
                , value 8
                , reader auto
                , metavar "DB_REPLICA_CSV_POOL_SIZE"
                , option
                , OptEnvConf.env "DB_REPLICA_CSV_POOL_SIZE"
                ]

        graphqlUrlP =
            setting
                [ help "graphql endpoint"
                , long "graphql-url"
                , value "http://localhost:8080/v1/graphql"
                , reader str
                , metavar "GRAPHQL_URL"
                , option
                , OptEnvConf.env "GRAPHQL_URL"
                ]

        portalUrlP =
            setting
                [ help "portal url"
                , long "portal-url"
                , value "http://localhost:3000"
                , reader str
                , metavar "PORTAL_URL"
                , option
                , OptEnvConf.env "PORTAL_URL"
                ]

        numRetriesP =
            setting
                [ help "number of retries for failed jobs"
                , long "num-retries"
                , value 12
                , reader auto
                , metavar "NUM_RETRIES"
                , option
                , OptEnvConf.env "NUM_RETRIES"
                ]

        mailHostP =
            optional
                ( setting
                    [ help "smtp host"
                    , long "mail-host"
                    , reader str
                    , metavar "MAIL_HOST"
                    , option
                    , OptEnvConf.env "MAIL_HOST"
                    ]
                )

        mailPortP =
            optional
                ( setting
                    [ help "smtp port"
                    , long "mail-port"
                    , reader auto
                    , metavar "MAIL_PORT"
                    , option
                    , OptEnvConf.env "MAIL_PORT"
                    ]
                )

        mailUserP =
            optional
                ( setting
                    [ help "smtp user"
                    , long "mail-user"
                    , reader str
                    , metavar "MAIL_USER"
                    , option
                    , OptEnvConf.env "MAIL_USER"
                    ]
                )

        mailPasswordP =
            optional
                ( setting
                    [ help "smtp password"
                    , long "mail-password"
                    , reader str
                    , metavar "MAIL_PASSWORD"
                    , option
                    , OptEnvConf.env "MAIL_PASSWORD"
                    ]
                )

        mailDevP =
            setting
                [ help "use development smtp flow"
                , switch True
                , long "mail-dev"
                , value True
                , reader auto
                , metavar "MAIL_DEV"
                , OptEnvConf.env "MAIL_DEV"
                ]

        mailSESP =
            setting
                [ help "use SES email flow"
                , switch True
                , long "mail-ses"
                , value False
                , reader auto
                , metavar "MAIL_SES"
                , OptEnvConf.env "MAIL_SES"
                ]

        apiHostP =
            setting
                [ help "API server host"
                , long "api-host"
                , value "0.0.0.0"
                , reader str
                , metavar "API_HOST"
                , option
                , OptEnvConf.env "API_HOST"
                ]

        apiPortP =
            setting
                [ help "API server port"
                , long "api-port"
                , value 8000
                , reader auto
                , metavar "API_PORT"
                , option
                , OptEnvConf.env "API_PORT"
                ]

        jwtSecretP =
            setting
                [ help "JWT secret"
                , long "jwt-secret"
                , value "supersecret"
                , reader str
                , metavar "JWT_SECRET"
                , option
                , OptEnvConf.env "JWT_SECRET"
                ]

        logLevelP =
            setting
                [ help "log level"
                , long "log-level"
                , value "info"
                , reader str
                , metavar "LOG_LEVEL"
                , option
                , OptEnvConf.env "LOG_LEVEL"
                ]


mkPool :: Int -> [Hasql.Connection.Setting.Setting] -> IO Pool.Pool
mkPool poolSize connSettings =
    Pool.acquire $
        Pool.Config.settings
            [ Pool.Config.size poolSize
            , Pool.Config.idlenessTimeout (5 * 60)
            , Pool.Config.staticConnectionSettings connSettings
            ]


mkDbSettings :: Text -> Int -> DbConnSpec -> [Hasql.Connection.Setting.Setting]
mkDbSettings host port spec =
    [ Hasql.Connection.Setting.connection
        ( Hasql.Connection.Setting.Connection.params
            [ Hasql.Connection.Setting.Connection.Param.host host
            , Hasql.Connection.Setting.Connection.Param.port (fromIntegral port)
            , Hasql.Connection.Setting.Connection.Param.user spec.dbUser
            , Hasql.Connection.Setting.Connection.Param.password spec.dbPassword
            , Hasql.Connection.Setting.Connection.Param.dbname spec.dbDatabase
            ]
        )
    , Hasql.Connection.Setting.usePreparedStatements False
    ]
