module Kronor.Db.Models.Shard (
    ShardId (..),
    shardToInt64,
) where

import Data.Aeson qualified as Aeson
import Kronor.Db.Types.Bigint (Bigint (..))
import Kronor.Tracer qualified
import RIO


newtype ShardId = ShardId Bigint
    deriving newtype (Eq, Show)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via Bigint


shardToInt64 :: ShardId -> Int64
shardToInt64 (ShardId (Bigint shardId)) = shardId


instance Kronor.Tracer.ToAttribute ShardId where
    toAttribute (ShardId b) = Kronor.Tracer.toAttribute (tshow b)
