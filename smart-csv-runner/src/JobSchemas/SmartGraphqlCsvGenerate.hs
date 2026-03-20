module JobSchemas.SmartGraphqlCsvGenerate where

import Data.Aeson qualified as Aeson
import Kronor.Db.Types.Bigint (Bigint)
import Kronor.Tracer qualified
import RIO


newtype SmartGraphqlCsvGenerate = SmartGraphqlCsvGenerate Payload
    deriving stock (Eq, Show, Generic)


data Payload = Payload
    { shardId :: Bigint
    , csvId :: Int64
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)


instance Kronor.Tracer.HasTraceTags Payload where
    getTraceTags payload =
        [ ("shard.id", Kronor.Tracer.toAttribute payload.shardId)
        , ("smart_csv.id", Kronor.Tracer.toAttribute payload.csvId)
        ]
