module SmartCsvRunner.Job (
    Job,
    JobEnv (..),
    retry,
    giveupS,
    retryS,
    UserException (..),
    StringyException (..),
    defaultTimeUntilNextAttempt,
    getJobId,
    PayloadId (..),
    Source,
    retry_,
) where

import Control.Exception.Annotated
import GHC.Stack (withFrozenCallStack)
import Hasql.Connection qualified
import Kronor.Db (HasNamedPool (..), HasPgPool (getPgPoolL), Pool)
import Kronor.Logger (HasLogEnv, HasRequestId (requestId), LoggerEnv, RequestId, logEnvL)
import Kronor.Tracer qualified
import RIO
import RIO.Process (ProcessContext)
import RIO.Text qualified as Text
import RIO.Time (DiffTime, secondsToDiffTime)
import SmartCsvRunner.AWS (HasAwsEnv (..))
import SmartCsvRunner.AWS qualified as AWS
import SmartCsvRunner.Job.Payload (PayloadId (..))


data JobEnv env = JobEnv
    { jobEnv :: env
    , jobLogFunc :: LoggerEnv -> LogFunc
    , jobLogEnv :: LoggerEnv
    , jobProcessContext :: ProcessContext
    , jobPgPool :: Pool
    , jobPgReadCSVPool :: Pool
    , jobTracing :: RequestId
    , jobId :: PayloadId
    , jobThreadConnection :: Hasql.Connection.Connection
    , jobFailedAttempts :: Natural
    , jobTracer :: Kronor.Tracer.Tracer
    , jobTopSpan :: Maybe Kronor.Tracer.Span
    , jobCurrentSpan :: Maybe Kronor.Tracer.Span
    , jobAwsEnv :: AWS.Env
    }


type Job env = RIO (JobEnv env)


instance HasLogFunc (JobEnv env) where
    logFuncL = lens (\JobEnv{..} -> jobLogFunc jobLogEnv) (\_ _ -> error "Setting logger function not supported")


instance HasLogEnv (JobEnv env) where
    logEnvL = lens jobLogEnv (\env logEnv -> env{jobLogEnv = logEnv})


instance HasAwsEnv (JobEnv env) where
    getAwsEnvL = lens (\JobEnv{..} -> jobAwsEnv) (\env newAwsEnv -> env{jobAwsEnv = newAwsEnv})


instance Kronor.Tracer.HasTracer (JobEnv env) where
    tracerL = lens jobTracer (\app tracer -> app{jobTracer = tracer})


instance Kronor.Tracer.HasTraceSpan (JobEnv env) where
    topTraceSpanL = lens jobTopSpan (\app s -> app{jobTopSpan = s})
    traceSpanL = lens jobCurrentSpan (\app s -> app{jobCurrentSpan = s})


instance HasRequestId (JobEnv env) where
    requestId = jobTracing


instance HasPgPool (JobEnv env) where
    getPgPoolL = lens jobPgPool (\app pool -> app{jobPgPool = pool})


instance HasNamedPool "csv-replica" (JobEnv env) where
    getPgNamedPoolL _ = lens jobPgReadCSVPool (\app pool -> app{jobPgReadCSVPool = pool})


type Source = Text


data StringyException = StringyException Source Text
    deriving stock (Show)


instance Exception StringyException where
    displayException (StringyException source desc) = Text.unpack $ source <> " " <> desc


data UserException
    = forall e. Exception e => RetryableException PayloadId DiffTime (Maybe Int) e
    | RetryableException_ PayloadId DiffTime (Maybe Int)
    | forall e. Exception e => NonRetryableException PayloadId e
deriving stock instance Show UserException
deriving anyclass instance Exception UserException


getJobId :: Job env PayloadId
getJobId = jobId <$> ask


retry :: HasCallStack => Exception e => Source -> DiffTime -> Utf8Builder -> e -> Job env a
retry src timeUntilNextAttempt t e = withFrozenCallStack do
    pId <- getJobId
    logErrorS src t
    throwWithCallStack $ RetryableException pId timeUntilNextAttempt Nothing e


retry_ :: HasCallStack => Source -> Maybe DiffTime -> Utf8Builder -> Job env a
retry_ src mTimeUntilNextAttempt t = withFrozenCallStack do
    pId <- getJobId
    failedAttempts <- jobFailedAttempts <$> ask
    logErrorS src t
    throwWithCallStack $
        RetryableException_
            pId
            ( case mTimeUntilNextAttempt of
                Nothing -> defaultTimeUntilNextAttempt failedAttempts
                Just n -> n
            )
            Nothing


retryS :: HasCallStack => Source -> DiffTime -> Utf8Builder -> Job env a
retryS src timeUntilNextAttempt t = withFrozenCallStack do
    pId <- getJobId
    throwWithCallStack $ RetryableException pId timeUntilNextAttempt Nothing (StringyException src (textDisplay t))


giveupS :: HasCallStack => Source -> Utf8Builder -> Job env a
giveupS src t = withFrozenCallStack do
    pId <- getJobId
    throwWithCallStack $ NonRetryableException pId $ StringyException src (textDisplay t)


defaultTimeUntilNextAttempt :: Natural -> DiffTime
defaultTimeUntilNextAttempt failedAttempts = secondsToDiffTime $ case failedAttempts of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 4
    4 -> 8
    5 -> 16
    6 -> 30
    7 -> 60
    8 -> 2 * 60
    9 -> 4 * 60
    10 -> 8 * 60
    11 -> 16 * 60
    12 -> 30 * 60
    _ -> 60 * 60
