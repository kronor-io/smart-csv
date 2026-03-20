{-# OPTIONS_GHC -Wno-deprecations #-}

module SmartCsvRunner.Dequeuer (
    withDequeue,
    startListening,
    checkIfShouldStop,
    streamJobsCount,
) where

import Control.Exception.Annotated (AnnotatedException (..))
import Control.Exception.Annotated.UnliftIO qualified as AnnException
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Function
import Data.String.Interpolate (iii)
import Hasql.Connection
import Hasql.Session qualified
import Hasql.TH
import Kronor.CircuitBreaker
import Kronor.Db qualified
import Kronor.Http qualified as Req
import Kronor.Logger (HasLogEnv)
import Network.HTTP.Client qualified as Http.Client
import RIO
import RIO.Time (DiffTime, diffTimeToPicoseconds)
import SmartCsvRunner.Dispatcher (withAddedContextFromAnnotations, withContextFromExceptionAnnotations)
import SmartCsvRunner.Job qualified as Job
import SmartCsvRunner.Job.Payload
import SmartCsvRunner.Job.Payload qualified as I
import Streamly.Data.Stream qualified as Streamly
import Streamly.Data.Unfold qualified as Streamly.Unfold
import Prelude


------------
-- PUBLIC --
------------

-- | A more general configurable version of 'withDequeue'. Unlike 'withDequeue' one
-- can specify the exception that causes a retry. Additionally, event handlers can be
-- specified to observe the internal behavior of the retry loop.
withDequeue ::
    forall env.
    (HasLogFunc env, Kronor.Logger.HasLogEnv env) =>
    -- | The connection for dequeuing the job. Will be in a transaction for the sudarion of this function
    Connection ->
    -- | Retry count
    Int ->
    -- | If True stop execution
    RIO env Bool ->
    -- | Continuation
    (I.Payload -> RIO env ()) ->
    RIO env ()
withDequeue conn retryCount shouldStop f = do
    let action env =
            withDequeuePayloadId retryCount (runRIO env . f)

    AnnException.catches
        do
            env <- ask
            liftIO $ doWork conn (runRIO env shouldStop) (action env)
        [ handleKronorJobException retryCount
        , handleHttpException retryCount
        , handleHttpSimpleException retryCount
        , handleIOError retryCount
        ]


startListening :: Vector Text -> Connection -> IO ()
startListening channels = I.runThrow listen
  where
    listen =
        Hasql.Session.statement
            channels
            $ Kronor.Db.makeUnprepared
                [resultlessStatement|
                select true::bool
                from (
                    select listen_on(channel)
                    from unnest($1::text[]) as channel
                ) a
                |]


streamJobsCount :: MonadIO m => Connection -> Streamly.Stream m ()
streamJobsCount conn = do
    let count =
            Streamly.unfoldrM
                ( \lastId -> do
                    (total, mostRecentId) <-
                        liftIO $
                            flip I.runThrow conn $
                                Hasql.Session.statement
                                    lastId
                                    ( Kronor.Db.makeUnprepared
                                        [singletonStatement|
                                select count(*)::int, coalesce(max(id), 0)::bigint from job_queue.get_job_batch($1::bigint, 10)
                            |]
                                    )
                    if total == 0
                        then return Nothing
                        else return (Just (total, mostRecentId))
                )
                0

    Streamly.unfoldMany
        Streamly.Unfold.replicateM
        (fmap (\i -> (fromIntegral i, pure ())) count)


checkIfShouldStop :: IO Bool -> IO Bool
checkIfShouldStop shouldStop = fix \recurse -> do
    threadDelay 1_000_000
    stop' <- shouldStop
    if stop'
        then do
            -- When we reach here the function finishes, so race ends with the result Right stop
            pure True
        else recurse


-------------
-- PRIVATE --
-------------

reQueueJobForCircuit :: Text -> I.PayloadId -> Int -> Hasql.Session.Session ()
reQueueJobForCircuit label pId attempts = do
    Hasql.Session.statement
        (coerce pId, max (fromIntegral attempts) 1, label)
        $ Kronor.Db.makeUnprepared
            [resultlessStatement|
                select id::bigint
                from job_queue.retry_job_circuit_closed(
                        id_ := $1::bigint,
                        label_ := $3::text,
                        attempt_ := $2::int
                    )
            |]


reQueueJob :: I.PayloadId -> Int -> DiffTime -> Hasql.Session.Session ()
reQueueJob pId attempts timeUntilNextAttempt = do
    let secondsUntilNextAttempt = fromInteger $ diffTimeToPicoseconds timeUntilNextAttempt `div` 10 ^ (12 :: Integer)
    Hasql.Session.statement
        (coerce pId, fromIntegral attempts, secondsUntilNextAttempt)
        $ Kronor.Db.makeUnprepared
            [resultlessStatement|
                select id::bigint
                from job_queue.retry_job(
                            id_ := $1::bigint,
                            attempt_ := $2::int,
                            at_ := now() + make_interval(secs => $3::bigint)
                        )
            |]


saveFailedJob :: I.PayloadId -> Hasql.Session.Session ()
saveFailedJob pId =
    Hasql.Session.statement
        (coerce pId)
        $ Kronor.Db.makeUnprepared
            [resultlessStatement|
            select true::bool from job_queue.mark_as_failed($1::bigint)
            |]


deleteJob :: I.PayloadId -> Hasql.Session.Session ()
deleteJob pId =
    Hasql.Session.statement
        (coerce pId)
        $ Kronor.Db.makeUnprepared
            [resultlessStatement|
            delete from job_queue.task_in_process
            where id = $1::bigint
            |]


withDequeuePayloadId ::
    Int ->
    (I.Payload -> IO b) ->
    Connection ->
    IO ()
withDequeuePayloadId retryCount f conn = do
    bracket
        ( do
            I.runThrow (Hasql.Session.sql "BEGIN") conn
            ex <- Hasql.Session.run I.dequeuePayload conn
            case ex of
                Left _ -> pure Nothing
                Right m_pay -> pure m_pay
        )
        ( \ex -> void $ case ex of
            Nothing -> do
                I.runThrow (Hasql.Session.sql "ROLLBACK") conn
            Just _ -> do
                I.runThrow (Hasql.Session.sql "COMMIT") conn
        )
        ( \ex -> do
            case ex of
                Nothing -> pure ()
                Just x -> void do
                    eRes <- flip I.runThrow conn $ do
                        b <- liftIO (AnnException.try $ f x)
                        let pid = I.pId x
                        case b of
                            Right _ -> (Right ()) <$ deleteJob pid
                            Left e -> do
                                case AnnException.check e of
                                    Just (AnnotatedException{exception = Job.NonRetryableException _ _}) -> saveFailedJob pid
                                    Just (AnnotatedException{exception = Job.RetryableException _ timeUntilNextAttempt mMaxAttempts _}) ->
                                        if I.pAttempts x < fromMaybe retryCount mMaxAttempts
                                            then reQueueJob pid (I.pAttempts x) timeUntilNextAttempt
                                            else saveFailedJob pid
                                    Just (AnnotatedException{exception = Job.RetryableException_ _ timeUntilNextAttempt mMaxAttempts}) ->
                                        if I.pAttempts x < fromMaybe retryCount mMaxAttempts
                                            then reQueueJob pid (I.pAttempts x) timeUntilNextAttempt
                                            else saveFailedJob pid
                                    Nothing ->
                                        case AnnException.check e of
                                            Just (AnnotatedException{exception = (CircuitBreakerClosed label)}) -> do
                                                reQueueJobForCircuit label pid (I.pAttempts x)
                                            Nothing -> do
                                                let timeUntilNextAttempt = Job.defaultTimeUntilNextAttempt $ fromIntegral $ I.pAttempts x
                                                if I.pAttempts x < retryCount
                                                    then reQueueJob pid (I.pAttempts x) timeUntilNextAttempt
                                                    else saveFailedJob pid

                                deleteJob pid
                                pure (Left e)
                    case eRes of
                        Left e -> liftIO (AnnException.throw e)
                        Right () -> pure ()
        )


doWork :: Connection -> IO Bool -> (Connection -> IO ()) -> IO ()
doWork conn shouldStop action = do
    stop <- shouldStop
    unless stop do
        action conn


handleKronorJobException ::
    (HasLogFunc env, Kronor.Logger.HasLogEnv env) =>
    Int ->
    AnnException.Handler (RIO env) ()
handleKronorJobException retryCount =
    AnnException.Handler
        ( \case
            AnnotatedException ann (Job.RetryableException pId timeUntilNextAttempt mMaxAttempts exception) -> do
                withAddedContextFromAnnotations ann do
                    logRetry
                        (fromMaybe retryCount mMaxAttempts)
                        timeUntilNextAttempt
                        (coerce pId)
                        exception
            AnnotatedException ann (Job.RetryableException_ pId timeUntilNextAttempt mMaxAttempts) -> do
                withAddedContextFromAnnotations ann do
                    logRetry_
                        (fromMaybe retryCount mMaxAttempts)
                        timeUntilNextAttempt
                        (coerce pId)
            AnnotatedException ann (Job.NonRetryableException pId exception) -> do
                withAddedContextFromAnnotations ann do
                    logDontRetry
                        (coerce pId)
                        exception
        )
  where
    logDontRetry ::
        HasLogFunc env =>
        Exception e =>
        Int64 ->
        e ->
        RIO env ()
    logDontRetry pId e = do
        logErrorS
            "kronor-worker:Dequeue"
            ( "Trying to mark the job with id: "
                <> displayShow pId
                <> " as failed since it threw a NonRetryableException. "
                <> fromString @Utf8Builder (displayException e)
            )


handleHttpException ::
    (HasLogFunc env, Kronor.Logger.HasLogEnv env) =>
    Int ->
    AnnException.Handler (RIO env) ()
handleHttpException retryCount =
    AnnException.Handler
        ( \(e :: AnnotatedException Req.HttpRequestException) -> do
            withContextFromExceptionAnnotations e do
                logErrorS
                    "kronor-worker:Dequeue"
                    [iii|Failed processing a job with Req.HttpException: #{e}; Max attempts: #{retryCount}|]
        )


handleHttpSimpleException ::
    (HasLogFunc env, Kronor.Logger.HasLogEnv env) =>
    Int ->
    AnnException.Handler (RIO env) ()
handleHttpSimpleException retryCount =
    AnnException.Handler
        ( \(e :: AnnotatedException Http.Client.HttpException) -> do
            withContextFromExceptionAnnotations e do
                logErrorS
                    "kronor-worker:Dequeue"
                    [iii|Failed processing a job with Req.HttpExceptionRequest: #{e}; Max attempts: #{retryCount}|]
        )


handleIOError ::
    (HasLogFunc env, Kronor.Logger.HasLogEnv env) =>
    Int ->
    AnnException.Handler (RIO env) ()
handleIOError retryCount =
    AnnException.Handler
        ( \(e :: AnnotatedException IOError) -> do
            withContextFromExceptionAnnotations e do
                logErrorS
                    "kronor-worker:Dequeue"
                    [iii|Failed processing job with exception: #{e}; Max attempts: #{retryCount}|]
        )


logRetry ::
    HasLogFunc env =>
    Exception e =>
    Int ->
    DiffTime ->
    Int64 ->
    e ->
    RIO env ()
logRetry retryCount timeUntilNextAttempt pId e = do
    logErrorS
        "kronor-worker:Dequeue"
        [iii|Failed processing a job with id: #{pId}, #{displayException e}; Retrying in #{show timeUntilNextAttempt}; Max attempts: #{retryCount}|]


logRetry_ ::
    HasLogFunc env =>
    Int ->
    DiffTime ->
    Int64 ->
    RIO env ()
logRetry_ retryCount timeUntilNextAttempt pId = do
    logErrorS
        "kronor-worker:Dequeue"
        [iii|Failed processing a job with id: #{pId}; Retrying in #{show timeUntilNextAttempt}; Max attempts: #{retryCount}|]
