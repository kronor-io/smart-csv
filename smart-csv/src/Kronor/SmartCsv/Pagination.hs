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
import Data.Morpheus.Types.Internal.AST (RAW, Selection (..), SelectionContent (..), unpackName)
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, columnHeader)
import Kronor.SmartCsv.Flatten (gatherSelectionNames, selectionOutputName)
import RIO

data CursorError
  = CursorColumnMissing Text
  | CursorValueMissing Text
  deriving stock (Eq, Show)

inferHeaders :: ColumnConfig -> Selection RAW -> [Text]
inferHeaders colConfig rootSelection =
  map (`columnHeader` colConfig) (gatherSelectionNames rootSelection)

extractCursor :: ColumnConfig -> Selection RAW -> Text -> Map Text Csv.Field -> Either CursorError Text
extractCursor colConfig rootSelection paginationKey row = do
  columnId <- maybe (Left (CursorColumnMissing paginationKey)) Right (findColumnId paginationKey rootSelection)
  let cursorColumn = columnHeader columnId colConfig
  case row Map.!? cursorColumn of
    Nothing -> Left (CursorValueMissing cursorColumn)
    Just cursor -> Right (decodeUtf8Lenient cursor)

findColumnId :: Text -> Selection RAW -> Maybe Text
findColumnId targetField = go
  where
    go InlineFragment {} = Nothing
    go Spread {} = Nothing
    go sel@(Selection {})
      | unpackName sel.selectionName == targetField = Just (selectionOutputName sel)
      | otherwise =
          case sel.selectionContent of
            SelectionSet sss -> asum (go <$> toList sss)
            _ -> Nothing

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
