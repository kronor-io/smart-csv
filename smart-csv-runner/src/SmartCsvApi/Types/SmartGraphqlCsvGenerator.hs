module SmartCsvApi.Types.SmartGraphqlCsvGenerator
  ( SmartGraphqlCsvGeneratorInput (..),
    SmartGraphqlCsvGeneratorResult (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Kronor.Db.Types.Bigint (Bigint)
import RIO

-- | Input type for the smartGraphqlCsvGenerator mutation
data SmartGraphqlCsvGeneratorInput = SmartGraphqlCsvGeneratorInput
  { shardId :: Bigint,
    recipient :: Text,
    graphqlPaginationKey :: Text,
    graphqlQueryBody :: Text,
    graphqlQueryVariables :: Text,
    columnConfig :: Maybe Value,
    columnConfigName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)

-- | Result type for the smartGraphqlCsvGenerator mutation
data SmartGraphqlCsvGeneratorResult = SmartGraphqlCsvGeneratorResult
  { reportId :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
