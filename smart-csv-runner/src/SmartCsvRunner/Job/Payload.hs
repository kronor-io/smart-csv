module SmartCsvRunner.Job.Payload (
    Payload (..),
    PayloadId (..),
    dequeuePayload,
    runThrow,
    notifyPayload,
) where

import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Int
import Data.Time (DiffTime)
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.LibPQ.Notify
import Hasql.Connection
import Hasql.Session
import Hasql.TH
import Kronor.Db qualified
import RIO


newtype PayloadId = PayloadId {unPayloadId :: Int64}
    deriving stock (Show)


data Payload = Payload
    { pId :: PayloadId
    , pAttempts :: Int
    , pTimeInQueue :: DiffTime
    , pExpired :: Bool
    , pValue :: Aeson.Value
    }


dequeuePayload :: Session (Maybe Payload)
dequeuePayload = do
    let singleQuery =
            [maybeStatement|
            SELECT
                id::bigint,
                attempts::int,
                time_in_queue::interval,
                expired::bool,
                value::jsonb
            from job_queue.dequeue_payload(1)
            |]

    statement () $
        Kronor.Db.makeUnprepared
            (fmap (\(pId, pAttempts, pTimeInQueue, pExpired, pValue) -> Payload (coerce pId) (fromIntegral pAttempts) pTimeInQueue pExpired pValue) <$> singleQuery)


newtype QueryException = QueryException SessionError
    deriving stock (Show)


instance Exception QueryException


runThrow :: Session a -> Connection -> IO a
runThrow sess conn = either (throwIO . QueryException) pure =<< run sess conn


notifyPayload :: [(ByteString, ByteString -> a)] -> Connection -> IO a
notifyPayload channels conn = fix $ \restart -> do
    PQ.Notify{..} <- either throwIO pure =<< withLibPQConnection conn getNotification
    case notifyRelname `lookup` channels of
        Just channelFn -> pure (channelFn notifyExtra)
        Nothing -> restart
