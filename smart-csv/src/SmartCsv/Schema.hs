module SmartCsv.Schema
  ( SchemaTypes (..),
    introspectionRequestBody,
    parseIntrospectionResponse,
    numericColumns,
    markNumericColumns,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Morpheus.Types.Internal.AST (RAW, Selection (..), SelectionContent (..), unpackName)
import Data.Set qualified as Set
import RIO
import RIO.List (find)
import SmartCsv.ColumnConfig (ColumnConfig, ColumnKey (..), ColumnSettings (..), OutputColumn (..), defaultColumnSettings, outputColumns)
import SmartCsv.Flatten (selectionOutputName)

-- | The field types of every object type in the schema the export query runs
-- against, keyed by type name and then field name.  List and non-null wrappers are
-- stripped: a column prints each element of a list the same way, so only the named
-- type matters.
data SchemaTypes = SchemaTypes
  { queryTypeName :: Text,
    fieldTypes :: Map Text (Map Text Text)
  }
  deriving stock (Eq, Show)

-- | Scalars whose values carry a decimal point.  Hasura sends them as JSON strings
-- when HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES is on, so the value alone does not say
-- it is a number.  Integer scalars are left out: they have no decimal point to swap.
numericScalars :: Set Text
numericScalars = Set.fromList ["numeric", "float4", "float8", "Float"]

-- | One introspection request for the whole schema, as the export's own role sees it.
-- Four levels of 'ofType' reach the named type of @[T!]!@, the deepest wrapping a
-- Hasura field has.
introspectionRequestBody :: LByteString
introspectionRequestBody =
  Aeson.encode
    $ Aeson.object
      [ "query"
          Aeson..= ( "{ __schema { queryType { name } types { name fields(includeDeprecated: true) { name type { name ofType { name ofType { name ofType { name } } } } } } } }" ::
                       Text
                   )
      ]

parseIntrospectionResponse :: Aeson.Value -> Either String SchemaTypes
parseIntrospectionResponse = Aeson.Types.parseEither \value -> do
  response <- Aeson.parseJSON value
  schema <- response Aeson..: "data" >>= (Aeson..: "__schema")
  queryTypeName <- schema Aeson..: "queryType" >>= (Aeson..: "name")
  types <- schema Aeson..: "types"
  fieldTypes <- Map.fromList . catMaybes <$> traverse parseType types
  pure SchemaTypes {queryTypeName, fieldTypes}
  where
    parseType = Aeson.withObject "__Type" \typeObj -> do
      name <- typeObj Aeson..: "name"
      mFields <- typeObj Aeson..:? "fields"
      for mFields \fields -> (name,) . Map.fromList <$> traverse parseField fields

    parseField = Aeson.withObject "__Field" \fieldObj -> do
      name <- fieldObj Aeson..: "name"
      namedType <- fieldObj Aeson..: "type" >>= parseNamedType
      pure (name, namedType)

    parseNamedType = Aeson.withObject "__Type" \typeObj ->
      typeObj Aeson..:? "name" >>= \case
        Just name -> pure name
        Nothing -> typeObj Aeson..: "ofType" >>= parseNamedType

-- | The columns whose printed value is a numeric scalar: a selected field's only
-- column, or entries of its config's @columns@.  The leaf is found the way
-- 'SmartCsv.Flatten.csvify' finds it: follow the column's dataPath, then
-- keep descending while the selection has a single field.
numericColumns :: SchemaTypes -> ColumnConfig -> Selection RAW -> Set ColumnKey
numericColumns schema colConfig rootSelection =
  case (rootSelection, fieldType schema.queryTypeName rootSelection) of
    (Selection {selectionContent = SelectionSet columns}, Just rowType) ->
      Set.fromList
        [ output.key
        | column@Selection {} <- toList columns,
          output <- toList (outputColumns colConfig (selectionOutputName column)),
          Just leafType <- [leafTypeOf output.dataPath rowType column],
          leafType `Set.member` numericScalars
        ]
    _ -> mempty
  where
    fieldType :: Text -> Selection RAW -> Maybe Text
    fieldType parentType selection = do
      fields <- Map.lookup parentType schema.fieldTypes
      Map.lookup (unpackName selection.selectionName) fields

    leafTypeOf :: [Text] -> Text -> Selection RAW -> Maybe Text
    leafTypeOf path parentType selection = do
      ownType <- fieldType parentType selection
      case (selection.selectionContent, path) of
        (SelectionField, []) -> Just ownType
        (SelectionField, _) -> Nothing
        (SelectionSet children, step : rest) -> do
          child <- find ((== step) . selectionOutputName) (fieldSelections children)
          leafTypeOf rest ownType child
        (SelectionSet children, []) -> case fieldSelections children of
          [onlyChild] -> leafTypeOf [] ownType onlyChild
          _ -> Nothing

    fieldSelections :: (Foldable t) => t (Selection RAW) -> [Selection RAW]
    fieldSelections children = [child | child@Selection {} <- toList children]

-- | Flag the given columns as numeric, adding a default entry for a field the
-- config does not mention.  Each field's entries are gone through once, however
-- many of them are numeric.
markNumericColumns :: Set ColumnKey -> ColumnConfig -> ColumnConfig
markNumericColumns numeric colConfig =
  Map.foldlWithKey' markEntries (foldl' markField colConfig numericFields) numericEntries
  where
    numericFields = [columnId | ColumnKey columnId Nothing <- Set.toList numeric]
    numericEntries = Map.fromListWith Set.union [(columnId, Set.singleton index) | ColumnKey columnId (Just index) <- Set.toList numeric]

    markField config columnId = Map.alter (Just . setNumeric . fromMaybe defaultColumnSettings) columnId config

    markEntries config columnId indices = Map.adjust (markEntriesOf indices) columnId config

    markEntriesOf indices settings =
      settings {columns = NonEmpty.zipWith (markAt indices) (0 :| [1 ..]) <$> settings.columns}

    markAt indices index entry
      | index `Set.member` indices = setNumeric entry
      | otherwise = entry

    -- Built field by field: OutputColumn has a 'numeric' field too, which makes a
    -- record update ambiguous.
    setNumeric (ColumnSettings decimals header dataPath _ columns) =
      ColumnSettings decimals header dataPath True columns
