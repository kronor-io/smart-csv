module Kronor.SmartCsv.Pagination
  ( CursorError (..),
    extractCursor,
    inferHeaders,
    setPaginationValues,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Morpheus.Types.Internal.AST (RAW, Selection)
import Kronor.SmartCsv.Flatten (gatherSelectionNames)
import RIO

data CursorError
  = CursorKeyDeleted Text
  | CursorValueMissing Text
  deriving stock (Eq, Show)

inferHeaders :: Map Text (Maybe Text) -> Selection RAW -> [Text]
inferHeaders colConfig rootSelection =
  mapMaybe
    ( \key -> case Map.lookup key colConfig of
        Just Nothing -> Nothing
        Nothing -> Just key
        Just mappedKey -> mappedKey
    )
    (gatherSelectionNames rootSelection)

extractCursor :: Map Text (Maybe Text) -> Text -> Text -> Map Text Csv.Field -> Either CursorError Text
extractCursor colConfig root paginationKey row =
  let rawColumn = root <> "_" <> paginationKey
      selectedColumn =
        case Map.lookup rawColumn colConfig of
          Nothing -> Right rawColumn
          Just Nothing -> Left (CursorKeyDeleted rawColumn)
          Just (Just colName) -> Right colName
   in do
        cursorColumn <- selectedColumn
        case row Map.!? cursorColumn of
          Nothing -> Left (CursorValueMissing cursorColumn)
          Just cursor -> Right (decodeUtf8Lenient cursor)

setPaginationValues :: Aeson.Key -> Int -> Maybe Text -> Aeson.Value -> Aeson.Value
setPaginationValues pKey limit mPaginationValue (Aeson.Object obj) =
  Aeson.Object
    $ Aeson.KeyMap.insert "paginationCondition" modifier
    $ Aeson.KeyMap.insert "rowLimit" (Aeson.toJSON limit) obj
  where
    modifier =
      maybe
        (Aeson.object [])
        (\paginationValue -> Aeson.object [pKey Aeson..= Aeson.object ["_lt" Aeson..= paginationValue]])
        mPaginationValue
setPaginationValues _ _ _ val = val
