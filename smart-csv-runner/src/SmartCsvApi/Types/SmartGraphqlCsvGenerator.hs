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
    orderBy :: Maybe Value,
    graphqlQueryBody :: Text,
    graphqlQueryVariables :: Text,
    columnConfig :: Maybe Value,
    columnConfigName :: Maybe Text,
    -- | Read the GraphQL schema to find the columns that print numbers, so that a
    -- number sent as a string (Hasura's stringified numeric types) still gets a
    -- decimal comma.  Off when absent.
    detectNumericColumns :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)

-- | Result type for the smartGraphqlCsvGenerator mutation
data SmartGraphqlCsvGeneratorResult = SmartGraphqlCsvGeneratorResult
  { reportId :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
