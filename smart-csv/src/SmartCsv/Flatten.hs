{-# LANGUAGE ViewPatterns #-}

module SmartCsv.Flatten
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
import RIO
import RIO.List (headMaybe)
import SmartCsv.ColumnConfig (ColumnConfig, ColumnKey, OutputColumn (..), outputColumns)
import SmartCsv.CsvFormat (CsvFormat (..), escapeFormula, withDecimalSeparator)

-- | A cell's text, and whether it starts with text rather than a number or a
-- boolean (only text is escaped as a formula).
data Rendered = Rendered
  { field :: Csv.Field,
    startsWithText :: Bool
  }

-- | Convert one GraphQL response row into flat CSV fields, keyed by column (see
-- 'ColumnKey'; headers need not be unique).  Each top-level key in the JSON
-- object prints the columns 'outputColumns' gives it: usually one, or one per
-- entry of its config's @columns@.  Scalar values are used directly; nested
-- objects and arrays are resolved using each column's data path.  Arrays are
-- serialised by rendering every element and joining them with commas.  Numbers
-- use the format's decimal separator, and with 'escapeFormulas' a cell that
-- starts with text is escaped (see 'escapeFormula').
csvify :: CsvFormat -> ColumnConfig -> Text -> Aeson.Value -> Map ColumnKey Csv.Field
csvify csvFormat colConfig _ (Aeson.Object (Aeson.KeyMap.toMapText -> obj)) =
  Map.unions $ concatMap extractColumns (Map.toList obj)
  where
    withSeparator :: String -> String
    withSeparator = withDecimalSeparator csvFormat.decimalSeparator

    truncateScientific :: Int -> Scientific -> Scientific
    truncateScientific decimals sc =
      let factor = scientific 1 decimals
          truncated = truncate (sc * factor) :: Integer
       in scientific truncated (negate decimals)

    formatNumeric :: OutputColumn -> Scientific -> Csv.Field
    formatNumeric column scientificValue =
      case column.decimalPlaces of
        Just decimals ->
          Csv.toField $ withSeparator $ formatScientific Fixed (Just decimals) (truncateScientific decimals scientificValue)
        Nothing
          | truncateScientific 0 scientificValue == scientificValue -> Csv.toField (truncate scientificValue :: Integer)
          | otherwise -> Csv.toField $ withSeparator $ formatScientific Fixed Nothing scientificValue

    -- Text, or a number, boolean or nothing.
    textCell, otherCell :: Csv.Field -> Rendered
    textCell rendered = Rendered {field = rendered, startsWithText = True}
    otherCell rendered = Rendered {field = rendered, startsWithText = False}

    renderLeaf :: OutputColumn -> Aeson.Value -> Maybe Rendered
    renderLeaf column (Aeson.String t) =
      case column.decimalPlaces of
        Just _ ->
          case readMaybe (Text.unpack t) :: Maybe Scientific of
            Just sc -> Just (otherCell (formatNumeric column sc))
            Nothing -> Just (textCell (Csv.toField t))
        -- A number Hasura sent as a string keeps its own digits (the scale of the
        -- database column), only the decimal point changes.  A string that isn't
        -- a number is text, even in a numeric column.
        Nothing
          | column.numeric,
            Just (_ :: Scientific) <- readMaybe (Text.unpack t) ->
              Just (otherCell (Csv.toField (withSeparator (Text.unpack t))))
          | otherwise -> Just (textCell (Csv.toField t))
    renderLeaf column (Aeson.Number sc) = Just (otherCell (formatNumeric column sc))
    renderLeaf _ (Aeson.Bool b) = Just (otherCell (if b then "True" else "False"))
    renderLeaf _ Aeson.Null = Just (otherCell mempty)
    renderLeaf _ (Aeson.Array _) = Nothing
    renderLeaf _ (Aeson.Object _) = Nothing

    -- An array's cell starts with its first item, so that item decides whether
    -- the cell starts with text.
    renderValue :: OutputColumn -> Aeson.Value -> Maybe Rendered
    renderValue column (Aeson.Array array) =
      let renderedItems = mapMaybe renderArrayItem (toList array)
       in Just
            Rendered
              { field = ByteString.Char8.intercalate ", " ((.field) <$> renderedItems),
                startsWithText = maybe False (.startsWithText) (headMaybe renderedItems)
              }
      where
        renderArrayItem Aeson.Null = Nothing
        renderArrayItem value = do
          rendered <- renderValue column value
          if ByteString.Char8.null rendered.field then Nothing else Just rendered
    renderValue column value = resolveValue column.dataPath value >>= renderLeaf column

    cellField :: Rendered -> Csv.Field
    cellField rendered
      | csvFormat.escapeFormulas && rendered.startsWithText = escapeFormula rendered.field
      | otherwise = rendered.field

    extractColumns :: (Text, Aeson.Value) -> [Map ColumnKey Csv.Field]
    extractColumns (columnId, value) =
      [ Map.singleton column.key (cellField rendered)
      | column <- toList (outputColumns colConfig columnId),
        Just rendered <- [renderValue column value]
      ]
csvify _ _ _ v = error ("csvify: expected a JSON object, but received: " <> show v)

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
