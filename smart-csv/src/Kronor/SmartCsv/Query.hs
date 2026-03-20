module Kronor.SmartCsv.Query
  ( GenericQuery (..),
    ResponseError (..),
    buildRequestBody,
    decodeResponseRows,
    resolvePaginationKey,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Kronor.SmartCsv.Flatten (csvify)
import Kronor.SmartCsv.Pagination (setPaginationValues)
import RIO

data GenericQuery
  = GenericQuery
  { paginationKey :: Maybe Text,
    query :: Text,
    variables :: Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

data ResponseError
  = ResponseContainsError Text
  | ResponseMissingData
  | ResponseMissingRootData
  deriving stock (Eq, Show)

resolvePaginationKey :: GenericQuery -> Aeson.Key
resolvePaginationKey gq = maybe "createdAt" Aeson.Key.fromText gq.paginationKey

buildRequestBody :: Aeson.Key -> Int -> Maybe Text -> GenericQuery -> LByteString
buildRequestBody pKey batchSize mCursor gq =
  Aeson.encode gq {variables = setPaginationValues pKey batchSize mCursor gq.variables}

decodeResponseRows :: Map Text (Maybe Text) -> Text -> Map Text Csv.Field -> Aeson.Value -> Either ResponseError (Vector (Map Text Csv.Field))
decodeResponseRows colConfig root emptyCsvRow (Aeson.Object responseObj) =
  case Aeson.KeyMap.lookup "error" responseObj of
    Just (Aeson.String errMsg) -> Left (ResponseContainsError errMsg)
    _ -> case Aeson.KeyMap.lookup "data" responseObj of
      Nothing -> Left ResponseMissingData
      Just (Aeson.Object dataObj) -> case Aeson.KeyMap.lookup (Aeson.Key.fromText root) dataObj of
        Just (Aeson.Array arr) -> Right ((`Map.union` emptyCsvRow) . csvify colConfig root <$> arr)
        _ -> Left ResponseMissingRootData
      _ -> Left ResponseMissingData
decodeResponseRows _ _ _ _ = Left ResponseMissingData
