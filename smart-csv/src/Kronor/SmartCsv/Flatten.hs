{-# LANGUAGE ViewPatterns #-}

module Kronor.SmartCsv.Flatten
  ( csvify,
    gatherSelectionNames,
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
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, columnDataPath, columnDecimalPlaces, columnHeader)
import RIO

-- | Convert one GraphQL response row into a flat CSV field map.
-- Each top-level key in the JSON object becomes a column keyed by its CSV header
-- (resolved via 'columnHeader').  Scalar values are used directly; nested objects
-- and arrays are resolved using the 'dataPath' from the column config (see
-- 'columnDataPath').  Arrays are serialised by rendering every element and
-- joining them with commas.
csvify :: ColumnConfig -> Text -> Aeson.Value -> Map Text Csv.Field
csvify colConfig _ (Aeson.Object (Aeson.KeyMap.toMapText -> obj)) =
  Map.unions $ mapMaybe extractColumn (Map.toList obj)
  where
    withCommaDecimalSeparator :: String -> String
    withCommaDecimalSeparator = map $ \char -> if char == '.' then ',' else char

    truncateScientific :: Int -> Scientific -> Scientific
    truncateScientific decimals sc =
      let factor = scientific 1 decimals
          truncated = truncate (sc * factor) :: Integer
       in scientific truncated (negate decimals)

    singletonField :: Text -> Csv.Field -> Map Text Csv.Field
    singletonField columnId value =
      Map.singleton (columnHeader columnId colConfig) value

    formatNumeric :: Text -> Scientific -> Csv.Field
    formatNumeric columnId scientificValue =
      case columnDecimalPlaces columnId colConfig of
        Just decimals ->
          Csv.toField $ withCommaDecimalSeparator $ formatScientific Fixed (Just decimals) (truncateScientific decimals scientificValue)
        Nothing
          | truncateScientific 0 scientificValue == scientificValue -> Csv.toField (truncate scientificValue :: Integer)
          | otherwise -> Csv.toField $ withCommaDecimalSeparator $ formatScientific Fixed Nothing scientificValue

    renderLeaf :: Text -> Aeson.Value -> Maybe Csv.Field
    renderLeaf columnId (Aeson.String t) =
      case columnDecimalPlaces columnId colConfig of
        Just _ ->
          case readMaybe (Text.unpack t) :: Maybe Scientific of
            Just sc -> Just (formatNumeric columnId sc)
            Nothing -> Just (Csv.toField t)
        Nothing -> Just (Csv.toField t)
    renderLeaf columnId (Aeson.Number sc) = Just (formatNumeric columnId sc)
    renderLeaf _ (Aeson.Bool b) = Just (if b then "True" else "False")
    renderLeaf _ Aeson.Null = Just mempty
    renderLeaf _ (Aeson.Array _) = Nothing
    renderLeaf _ (Aeson.Object _) = Nothing

    renderValue :: Text -> [Text] -> Aeson.Value -> Maybe Csv.Field
    renderValue columnId path (Aeson.Array array) =
      let renderedItems = mapMaybe renderArrayItem (toList array)
       in Just (ByteString.Char8.intercalate ", " renderedItems)
      where
        renderArrayItem Aeson.Null = Nothing
        renderArrayItem value = do
          rendered <- renderValue columnId path value
          if ByteString.Char8.null rendered then Nothing else Just rendered
    renderValue columnId path value = resolveValue path value >>= renderLeaf columnId

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

    extractColumn :: (Text, Aeson.Value) -> Maybe (Map Text Csv.Field)
    extractColumn (columnId, value) = do
      fieldValue <- renderValue columnId (columnDataPath columnId colConfig) value
      pure (singletonField columnId fieldValue)
csvify _ _ v = error ("csvify: expected a JSON object, but received: " <> show v)

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
