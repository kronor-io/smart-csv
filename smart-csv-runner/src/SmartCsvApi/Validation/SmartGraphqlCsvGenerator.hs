module SmartCsvApi.Validation.SmartGraphqlCsvGenerator
  ( SmartGraphqlCsvGenerator (..),
    validateSmartGraphqlCsvGeneratorInput,
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Data.Time (nominalDay)
import Kronor.Db.Models.Shard (ShardId (..))
import Kronor.Db.Types.Bigint (Bigint (..))
import Kronor.SmartCsv.Validation qualified as SmartCsvValidation
import RIO
import RIO.Text qualified as Text
import SmartCsvApi.Types.SmartGraphqlCsvGenerator (SmartGraphqlCsvGeneratorInput (..))

data SmartGraphqlCsvGenerator = SmartGraphqlCsvGenerator
  { shardId :: ShardId,
    recipient :: Text,
    graphqlPaginationKey :: JSON.Key,
    graphqlQueryBody :: Text,
    graphqlQueryVariables :: Value,
    columnConfig :: Maybe Value,
    columnConfigName :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Validate the SmartGraphqlCsvGeneratorInput
validateSmartGraphqlCsvGeneratorInput ::
  Int ->
  SmartGraphqlCsvGeneratorInput ->
  Either String SmartGraphqlCsvGenerator
validateSmartGraphqlCsvGeneratorInput maxRangeDays input = do
  -- Validate shard ID (must be positive)
  case input.shardId of
    Bigint n | n <= 0 -> Left "shardId must be a positive number"
    _ -> pure ()

  -- Validate recipient (must not be empty)
  when (Text.null input.recipient) $ Left "recipient email address must not be empty"

  let graphqlPaginationKey = JSON.fromText input.graphqlPaginationKey
      queryRootField = case SmartCsvValidation.validateGraphqlQueryBodyAndGetRootField input.graphqlQueryBody of
        Left validationError -> Left $ "Invalid GraphQL query body: " <> Text.unpack (Text.intercalate ", " $ toList validationError)
        Right rootField -> Right rootField

  _ <- queryRootField
  let maxRange = fromIntegral maxRangeDays * nominalDay

  -- Validate using SmartCsvValidation
  queryVariables <- case SmartCsvValidation.validateQueryVariables maxRange graphqlPaginationKey input.graphqlQueryVariables of
    Left validationError -> Left $ "Invalid GraphQL query variables: " <> Text.unpack validationError
    Right vars -> Right vars

  -- Cannot specify both inline config and named config
  when (isJust input.columnConfig && isJust input.columnConfigName)
    $ Left "Cannot specify both columnConfig and columnConfigName"

  return
    SmartGraphqlCsvGenerator
      { shardId = ShardId input.shardId,
        recipient = input.recipient,
        graphqlPaginationKey = graphqlPaginationKey,
        graphqlQueryBody = input.graphqlQueryBody,
        graphqlQueryVariables = queryVariables,
        columnConfig = input.columnConfig,
        columnConfigName = input.columnConfigName
      }
