module Kronor.SmartCsv.Pagination
  ( CursorError (..),
    PaginationCursor (..),
    PaginationDirection (..),
    PaginationField (..),
    encodePaginationCursor,
    extractCursor,
    extractCursorValue,
    inferHeaders,
    parseOrderByFields,
    resolvePaginationFields,
    setPaginationValues,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Csv qualified as Csv
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Morpheus.Types.Internal.AST (RAW, Selection (..), SelectionContent (..), unpackName)
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, columnHeader)
import Kronor.SmartCsv.Flatten (gatherSelectionNames, selectionOutputName)
import RIO
import RIO.Text qualified as Text

data CursorError
  = CursorColumnMissing Text
  | CursorValueMissing Text
  deriving stock (Eq, Show)

data PaginationDirection = PaginationAsc | PaginationDesc
  deriving stock (Eq, Show)

data PaginationField = PaginationField
  { paginationFieldName :: Text,
    paginationFieldDirection :: PaginationDirection
  }
  deriving stock (Eq, Show)

newtype PaginationCursor = PaginationCursor
  { cursorValues :: Map Text Text
  }
  deriving stock (Eq, Show)

inferHeaders :: ColumnConfig -> Selection RAW -> [Text]
inferHeaders colConfig rootSelection =
  map (`columnHeader` colConfig) (gatherSelectionNames rootSelection)

extractCursor :: ColumnConfig -> Selection RAW -> NonEmpty PaginationField -> Map Text Csv.Field -> Either CursorError PaginationCursor
extractCursor colConfig rootSelection paginationFields row = do
  cursorEntries <-
    for (toList paginationFields) $ \PaginationField {paginationFieldName} -> do
      columnId <- maybe (Left (CursorColumnMissing paginationFieldName)) Right (findColumnId (paginationFieldPath paginationFieldName) rootSelection)
      let cursorColumn = columnHeader columnId colConfig
      case row Map.!? cursorColumn of
        Nothing -> Left (CursorValueMissing cursorColumn)
        Just cursor -> Right (paginationFieldName, decodeUtf8Lenient cursor)
  pure (PaginationCursor (Map.fromList cursorEntries))

extractCursorValue :: Selection RAW -> NonEmpty PaginationField -> Aeson.Value -> Either CursorError PaginationCursor
extractCursorValue rootSelection paginationFields rowValue = do
  cursorEntries <-
    for (toList paginationFields) $ \PaginationField {paginationFieldName} -> do
      responsePath <- maybe (Left (CursorColumnMissing paginationFieldName)) Right (findResponsePath (paginationFieldPath paginationFieldName) rootSelection)
      value <- maybe (Left (CursorValueMissing (Text.intercalate "." (toList responsePath)))) Right (lookupResponsePath responsePath rowValue)
      cursor <- maybe (Left (CursorValueMissing (Text.intercalate "." (toList responsePath)))) Right (cursorValueText value)
      Right (paginationFieldName, cursor)
  pure (PaginationCursor (Map.fromList cursorEntries))

encodePaginationCursor :: PaginationCursor -> Text
encodePaginationCursor = decodeUtf8Lenient . toStrictBytes . Aeson.encode . cursorValues

resolvePaginationFields :: Aeson.Key -> Aeson.Value -> NonEmpty PaginationField
resolvePaginationFields paginationKey queryVariables =
  case parsedOrderByFields of
    firstField : remainingFields
      | firstField.paginationFieldName == Aeson.Key.toText paginationKey -> firstField :| remainingFields
    _ -> fallbackPaginationFields
  where
    fallbackPaginationFields = PaginationField (Aeson.Key.toText paginationKey) PaginationDesc :| []

    parsedOrderByFields = case queryVariables of
      Aeson.Object obj ->
        case Aeson.KeyMap.lookup "orderBy" obj of
          Just orderBy -> parseOrderByFields orderBy
          _ -> []
      _ -> []

parseOrderByFields :: Aeson.Value -> [PaginationField]
parseOrderByFields (Aeson.Array orderByValues) = concatMap parseOrderByEntry (toList orderByValues)
  where
    parseOrderByEntry :: Aeson.Value -> [PaginationField]
    parseOrderByEntry (Aeson.Object orderObj) = concatMap (parseOrderByComponent []) (Aeson.KeyMap.toList orderObj)
    parseOrderByEntry _ = []

    parseOrderByComponent :: [Text] -> (Aeson.Key, Aeson.Value) -> [PaginationField]
    parseOrderByComponent path (fieldName, Aeson.String directionText) =
      maybeToList $ PaginationField (Text.intercalate "." (path <> [Aeson.Key.toText fieldName])) <$> parseDirection directionText
    parseOrderByComponent path (fieldName, Aeson.Object nestedOrderObj) =
      concatMap (parseOrderByComponent (path <> [Aeson.Key.toText fieldName])) (Aeson.KeyMap.toList nestedOrderObj)
    parseOrderByComponent _ _ = []

    parseDirection = \case
      directionText | Text.toUpper directionText == "ASC" -> Just PaginationAsc
      directionText | Text.toUpper directionText == "DESC" -> Just PaginationDesc
      _ -> Nothing
parseOrderByFields _ = []

paginationFieldPath :: Text -> NonEmpty Text
paginationFieldPath fieldName =
  case filter (not . Text.null) (Text.split (== '.') fieldName) of
    [] -> fieldName :| []
    pathPart : rest -> pathPart :| rest

findColumnId :: NonEmpty Text -> Selection RAW -> Maybe Text
findColumnId _ InlineFragment {} = Nothing
findColumnId _ Spread {} = Nothing
findColumnId targetPath sel@(Selection {}) =
  case sel.selectionContent of
    SelectionSet sss ->
      asum
        [ case child of
            Selection {}
              | unpackName child.selectionName == NonEmpty.head targetPath ->
                  case NonEmpty.nonEmpty (NonEmpty.tail targetPath) of
                    Nothing -> Just (selectionOutputName child)
                    Just remainingPath -> selectionOutputName child <$ findColumnId remainingPath child
            _ -> Nothing
          | child <- toList sss
        ]
    _ -> Nothing

findResponsePath :: NonEmpty Text -> Selection RAW -> Maybe (NonEmpty Text)
findResponsePath _ InlineFragment {} = Nothing
findResponsePath _ Spread {} = Nothing
findResponsePath targetPath sel@(Selection {}) =
  case sel.selectionContent of
    SelectionSet sss ->
      asum
        [ case child of
            Selection {}
              | unpackName child.selectionName == NonEmpty.head targetPath ->
                  case NonEmpty.nonEmpty (NonEmpty.tail targetPath) of
                    Nothing -> Just (selectionOutputName child :| [])
                    Just remainingPath -> (selectionOutputName child NonEmpty.<|) <$> findResponsePath remainingPath child
            _ -> Nothing
          | child <- toList sss
        ]
    _ -> Nothing

lookupResponsePath :: NonEmpty Text -> Aeson.Value -> Maybe Aeson.Value
lookupResponsePath (fieldName :| []) (Aeson.Object obj) = Aeson.KeyMap.lookup (Aeson.Key.fromText fieldName) obj
lookupResponsePath (fieldName :| nextField : rest) (Aeson.Object obj) = do
  nestedValue <- Aeson.KeyMap.lookup (Aeson.Key.fromText fieldName) obj
  lookupResponsePath (nextField :| rest) nestedValue
lookupResponsePath _ _ = Nothing

cursorValueText :: Aeson.Value -> Maybe Text
cursorValueText (Aeson.String text) = Just text
cursorValueText (Aeson.Number number) = Just (tshow number)
cursorValueText (Aeson.Bool boolValue) = Just (if boolValue then "true" else "false")
cursorValueText Aeson.Null = Nothing
cursorValueText (Aeson.Array _) = Nothing
cursorValueText (Aeson.Object _) = Nothing

setPaginationValues :: NonEmpty PaginationField -> Int -> Maybe PaginationCursor -> Aeson.Value -> Aeson.Value
setPaginationValues paginationFields limit mPaginationValue (Aeson.Object obj) =
  Aeson.Object
    $ Aeson.KeyMap.insert "paginationCondition" modifier
    $ Aeson.KeyMap.insert "rowLimit" (Aeson.toJSON limit) obj
  where
    modifier =
      maybe
        (Aeson.object [])
        (paginationCondition paginationFields)
        mPaginationValue
setPaginationValues _ _ _ val = val

paginationCondition :: NonEmpty PaginationField -> PaginationCursor -> Aeson.Value
paginationCondition (paginationField :| []) cursor =
  maybe (Aeson.object []) (comparisonExpression paginationField) (lookupCursorValue paginationField cursor)
paginationCondition (paginationField :| remainingFields) cursor =
  case lookupCursorValue paginationField cursor of
    Nothing -> Aeson.object []
    Just cursorValue ->
      Aeson.object
        [ "_or" Aeson..=
            [ comparisonExpression paginationField cursorValue,
              Aeson.object
                [ "_and" Aeson..=
                    [ equalityExpression paginationField cursorValue,
                      paginationCondition (NonEmpty.fromList remainingFields) cursor
                    ]
                ]
            ]
        ]

comparisonExpression :: PaginationField -> Text -> Aeson.Value
comparisonExpression paginationField cursorValue =
  nestedComparison (paginationFieldPath paginationField.paginationFieldName) (Aeson.object [comparisonOperator paginationField.paginationFieldDirection Aeson..= cursorValue])

equalityExpression :: PaginationField -> Text -> Aeson.Value
equalityExpression paginationField cursorValue =
  nestedComparison (paginationFieldPath paginationField.paginationFieldName) (Aeson.object ["_eq" Aeson..= cursorValue])

nestedComparison :: NonEmpty Text -> Aeson.Value -> Aeson.Value
nestedComparison (fieldName :| []) leaf = Aeson.object [Aeson.Key.fromText fieldName Aeson..= leaf]
nestedComparison (fieldName :| nextField : rest) leaf = Aeson.object [Aeson.Key.fromText fieldName Aeson..= nestedComparison (nextField :| rest) leaf]

comparisonOperator :: PaginationDirection -> Aeson.Key
comparisonOperator = \case
  PaginationAsc -> "_gt"
  PaginationDesc -> "_lt"

lookupCursorValue :: PaginationField -> PaginationCursor -> Maybe Text
lookupCursorValue paginationField cursor = cursor.cursorValues Map.!? paginationField.paginationFieldName
