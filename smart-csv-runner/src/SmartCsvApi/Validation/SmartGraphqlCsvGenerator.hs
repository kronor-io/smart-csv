module SmartCsvApi.Validation.SmartGraphqlCsvGenerator
  ( SmartGraphqlCsvGenerator (..),
    validateSmartGraphqlCsvGeneratorInput,
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Kronor.Db.Models.Shard (ShardId (..))
import Kronor.Db.Types.Bigint (Bigint (..))
import Kronor.SmartCsv.Pagination qualified as SmartCsvPagination
import Kronor.SmartCsv.Validation qualified as SmartCsvValidation
import RIO
import RIO.Text qualified as Text
import SmartCsvApi.Types.SmartGraphqlCsvGenerator (SmartGraphqlCsvGeneratorInput (..))

data SmartGraphqlCsvGenerator = SmartGraphqlCsvGenerator
  { shardId :: ShardId,
    recipient :: Text,
    graphqlPaginationKey :: JSON.Key,
    orderBy :: Maybe Value,
    graphqlQueryBody :: Text,
    graphqlQueryVariables :: Value,
    columnConfig :: Maybe Value,
    columnConfigName :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Validate the SmartGraphqlCsvGeneratorInput
validateSmartGraphqlCsvGeneratorInput ::
  SmartGraphqlCsvGeneratorInput ->
  Either String SmartGraphqlCsvGenerator
validateSmartGraphqlCsvGeneratorInput input = do
  -- Validate shard ID (must be positive)
  case input.shardId of
    Bigint n | n <= 0 -> Left "shardId must be a positive number"
    _ -> pure ()

  -- Validate recipient (must not be empty)
  when (Text.null input.recipient) $ Left "recipient email address must not be empty"

  let graphqlPaginationKey = JSON.fromText input.graphqlPaginationKey

  case SmartCsvValidation.validateGraphqlQueryBodyAndGetRootField input.graphqlQueryBody of
    Left validationError -> Left $ "Invalid GraphQL query body: " <> Text.unpack (Text.intercalate ", " $ toList validationError)
    Right _ -> pure ()

  -- Parse the query variables JSON (row/time limits bound the job at generation time)
  queryVariables <- case SmartCsvValidation.validateQueryVariables input.graphqlQueryVariables of
    Left validationError -> Left $ "Invalid GraphQL query variables: " <> Text.unpack validationError
    Right vars -> Right vars

  case input.orderBy of
    Nothing -> pure ()
    Just orderBy ->
      case SmartCsvPagination.parseOrderByFields orderBy of
        [] -> Left "orderBy must contain at least one ASC or DESC field"
        firstField : _
          | firstField.paginationFieldName == input.graphqlPaginationKey -> pure ()
          | otherwise -> Left "The first orderBy field must match graphqlPaginationKey"

  -- Cannot specify both inline config and named config
  when (isJust input.columnConfig && isJust input.columnConfigName)
    $ Left "Cannot specify both columnConfig and columnConfigName"

  return
    SmartGraphqlCsvGenerator
      { shardId = ShardId input.shardId,
        recipient = input.recipient,
        graphqlPaginationKey = graphqlPaginationKey,
        orderBy = input.orderBy,
        graphqlQueryBody = input.graphqlQueryBody,
        graphqlQueryVariables = queryVariables,
        columnConfig = input.columnConfig,
        columnConfigName = input.columnConfigName
      }
