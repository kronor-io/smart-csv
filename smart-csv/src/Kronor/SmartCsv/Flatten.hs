{-# LANGUAGE ViewPatterns #-}

module Kronor.SmartCsv.Flatten
  ( csvify,
    gatherSelectionNames,
    resolveValue,
    selectionOutputName,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Morpheus.Types.Internal.AST (RAW, Selection (..), SelectionContent (..), unpackName)
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, scientific)
import Data.Text qualified as Text
import Data.Vector qualified
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, ColumnKey, OutputColumn (..), outputColumns)
import RIO

-- | Convert one GraphQL response row into flat CSV fields, keyed by column (see
-- 'ColumnKey'; headers need not be unique).  Each top-level key in the JSON
-- object prints the columns 'outputColumns' gives it: usually one, or one per
-- entry of its config's @columns@.  Scalar values are used directly; nested
-- objects and arrays are resolved using each column's data path.  Arrays are
-- serialised by rendering every element and joining them with commas.
csvify :: ColumnConfig -> Text -> Aeson.Value -> Map ColumnKey Csv.Field
csvify colConfig _ (Aeson.Object (Aeson.KeyMap.toMapText -> obj)) =
  Map.unions $ concatMap extractColumns (Map.toList obj)
  where
    withCommaDecimalSeparator :: String -> String
    withCommaDecimalSeparator = map $ \char -> if char == '.' then ',' else char

    truncateScientific :: Int -> Scientific -> Scientific
    truncateScientific decimals sc =
      let factor = scientific 1 decimals
          truncated = truncate (sc * factor) :: Integer
       in scientific truncated (negate decimals)

    formatNumeric :: OutputColumn -> Scientific -> Csv.Field
    formatNumeric column scientificValue =
      case column.decimalPlaces of
        Just decimals ->
          Csv.toField $ withCommaDecimalSeparator $ formatScientific Fixed (Just decimals) (truncateScientific decimals scientificValue)
        Nothing
          | truncateScientific 0 scientificValue == scientificValue -> Csv.toField (truncate scientificValue :: Integer)
          | otherwise -> Csv.toField $ withCommaDecimalSeparator $ formatScientific Fixed Nothing scientificValue

    renderLeaf :: OutputColumn -> Aeson.Value -> Maybe Csv.Field
    renderLeaf column (Aeson.String t) =
      case column.decimalPlaces of
        Just _ ->
          case readMaybe (Text.unpack t) :: Maybe Scientific of
            Just sc -> Just (formatNumeric column sc)
            Nothing -> Just (Csv.toField t)
        -- A number Hasura sent as a string keeps its own digits (the scale of the
        -- database column), only the decimal point changes.
        Nothing
          | column.numeric -> Just (Csv.toField (withCommaDecimalSeparator (Text.unpack t)))
          | otherwise -> Just (Csv.toField t)
    renderLeaf column (Aeson.Number sc) = Just (formatNumeric column sc)
    renderLeaf _ (Aeson.Bool b) = Just (if b then "True" else "False")
    renderLeaf _ Aeson.Null = Just mempty
    renderLeaf _ (Aeson.Array _) = Nothing
    renderLeaf _ (Aeson.Object _) = Nothing

    renderValue :: OutputColumn -> Aeson.Value -> Maybe Csv.Field
    renderValue column (Aeson.Array array) =
      let renderedItems = mapMaybe renderArrayItem (toList array)
       in Just (ByteString.Char8.intercalate ", " renderedItems)
      where
        renderArrayItem Aeson.Null = Nothing
        renderArrayItem value = do
          rendered <- renderValue column value
          if ByteString.Char8.null rendered then Nothing else Just rendered
    renderValue column value = resolveValue column.dataPath value >>= renderLeaf column

    extractColumns :: (Text, Aeson.Value) -> [Map ColumnKey Csv.Field]
    extractColumns (columnId, value) =
      [ Map.singleton column.key field
      | column <- toList (outputColumns colConfig columnId),
        Just field <- [renderValue column value]
      ]
csvify _ _ v = error ("csvify: expected a JSON object, but received: " <> show v)

-- | Follow a column's data path down to the scalar it prints.  An array resolves
-- through its first element; an object with a single key resolves to that key's
-- value once the path is used up.
resolveValue :: [Text] -> Aeson.Value -> Maybe Aeson.Value
resolveValue [] value@(Aeson.String _) = Just value
resolveValue [] value@(Aeson.Number _) = Just value
resolveValue [] value@(Aeson.Bool _) = Just value
resolveValue [] Aeson.Null = Just Aeson.Null
resolveValue [] (Aeson.Object obj') =
  case Aeson.KeyMap.toList obj' of
    [(_, value)] -> resolveValue [] value
    _ -> Nothing
resolveValue path (Aeson.Array array) = do
  value <- array Data.Vector.!? 0
  resolveValue path value
resolveValue (pathPart : rest) (Aeson.Object obj') = do
  value <- Aeson.KeyMap.lookup (Aeson.Key.fromText pathPart) obj'
  resolveValue rest value
resolveValue _ _ = Nothing

gatherSelectionNames :: Selection RAW -> [Text]
gatherSelectionNames = go
  where
    go :: Selection RAW -> [Text]
    go InlineFragment {} = mempty
    go Spread {} = mempty
    go sel@(Selection {}) =
      case sel.selectionContent of
        SelectionSet sss -> mapMaybe immediateSelectionName (toList sss)
        _ -> pure (selectionOutputName sel)

    immediateSelectionName :: Selection RAW -> Maybe Text
    immediateSelectionName InlineFragment {} = Nothing
    immediateSelectionName Spread {} = Nothing
    immediateSelectionName sel@(Selection {}) = Just (selectionOutputName sel)

selectionOutputName :: Selection RAW -> Text
selectionOutputName sel = maybe (unpackName sel.selectionName) unpackName sel.selectionAlias
