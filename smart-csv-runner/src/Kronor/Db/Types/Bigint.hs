module Kronor.Db.Types.Bigint (Bigint (..), toInt64) where

import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Kronor.Tracer qualified
import RIO


newtype Bigint = Bigint Int64
    deriving newtype (Show, Eq, Read, Num, Ord, Enum, Real, Integral)


toInt64 :: Bigint -> Int64
toInt64 = coerce


instance Aeson.FromJSON Bigint where
    parseJSON v = Bigint <$> Aeson.parseJSON v


instance Aeson.ToJSON Bigint where
    toJSON (Bigint i) = Aeson.toJSON i


instance Kronor.Tracer.ToAttribute Bigint where
    toAttribute (Bigint i) = Kronor.Tracer.toAttribute i
