module Kronor.SmartCsv.Pagination
  ( CursorError (..),
    PaginationCursor (..),
    PaginationDirection (..),
    PaginationField (..),
    encodePaginationCursor,
    extractCursor,
    inferHeaders,
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
      columnId <- maybe (Left (CursorColumnMissing paginationFieldName)) Right (findColumnId paginationFieldName rootSelection)
      let cursorColumn = columnHeader columnId colConfig
      case row Map.!? cursorColumn of
        Nothing -> Left (CursorValueMissing cursorColumn)
        Just cursor -> Right (paginationFieldName, decodeUtf8Lenient cursor)
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
          Just (Aeson.Array orderByValues) -> concatMap parseOrderByEntry (toList orderByValues)
          _ -> []
      _ -> []

    parseOrderByEntry :: Aeson.Value -> [PaginationField]
    parseOrderByEntry (Aeson.Object orderObj) = mapMaybe parseOrderByComponent (Aeson.KeyMap.toList orderObj)
    parseOrderByEntry _ = []

    parseOrderByComponent :: (Aeson.Key, Aeson.Value) -> Maybe PaginationField
    parseOrderByComponent (fieldName, Aeson.String directionText) =
      PaginationField (Aeson.Key.toText fieldName) <$> parseDirection directionText
    parseOrderByComponent _ = Nothing

    parseDirection = \case
      directionText | Text.toUpper directionText == "ASC" -> Just PaginationAsc
      directionText | Text.toUpper directionText == "DESC" -> Just PaginationDesc
      _ -> Nothing

findColumnId :: Text -> Selection RAW -> Maybe Text
findColumnId _ InlineFragment {} = Nothing
findColumnId _ Spread {} = Nothing
findColumnId targetField sel@(Selection {}) =
  case sel.selectionContent of
    SelectionSet sss ->
      asum
        [ case child of
            Selection {}
              | unpackName child.selectionName == targetField -> Just (selectionOutputName child)
            _ -> Nothing
          | child <- toList sss
        ]
    _ -> Nothing

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
                [ Aeson.Key.fromText paginationField.paginationFieldName Aeson..= Aeson.object ["_eq" Aeson..= cursorValue],
                  "_and" Aeson..= [paginationCondition (NonEmpty.fromList remainingFields) cursor]
                ]
            ]
        ]

comparisonExpression :: PaginationField -> Text -> Aeson.Value
comparisonExpression paginationField cursorValue =
  Aeson.object
    [ Aeson.Key.fromText paginationField.paginationFieldName Aeson..=
        Aeson.object [comparisonOperator paginationField.paginationFieldDirection Aeson..= cursorValue]
    ]

comparisonOperator :: PaginationDirection -> Aeson.Key
comparisonOperator = \case
  PaginationAsc -> "_gt"
  PaginationDesc -> "_lt"

lookupCursorValue :: PaginationField -> PaginationCursor -> Maybe Text
lookupCursorValue paginationField cursor = cursor.cursorValues Map.!? paginationField.paginationFieldName
