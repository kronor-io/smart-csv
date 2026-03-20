module JobSchemas.GenerateSettlementCsv where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Kronor.Db.Types.Bigint (Bigint)
import Kronor.Tracer qualified
import RIO


newtype GenerateSettlementCSV = GenerateSettlementCSV Payload
    deriving stock (Eq, Show, Generic)


data Payload = Payload
    { shardId :: Bigint
    , reportId :: Int64
    , startDate :: UTCTime
    , endDate :: UTCTime
    , stateMachineId :: Int64
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)


instance Kronor.Tracer.HasTraceTags Payload where
    getTraceTags payload =
        [ ("shard.id", Kronor.Tracer.toAttribute payload.shardId)
        , ("report.id", Kronor.Tracer.toAttribute payload.reportId)
        , ("report.start_date", Kronor.Tracer.toAttribute $ tshow payload.startDate)
        , ("report.end_date", Kronor.Tracer.toAttribute $ tshow payload.endDate)
        ]
