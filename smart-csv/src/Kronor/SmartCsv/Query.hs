module Kronor.SmartCsv.Query
  ( GenericQuery (..),
    DecodedResponsePage (..),
    ParsedResponseRow (..),
    ResponseError (..),
    ResponseRowsStep (..),
    buildRequestBody,
    decodeResponseChunk,
    decodeResponseChunkWith,
    decodedResponsePageBytes,
    decodeResponsePage,
    decodeResponsePageWith,
    decodeResponseRows,
    resolvePaginationFields,
    resolvePaginationKey,
    responsePageParser,
    responseRowsStartParser,
    responseRowsStepParser,
    responseRowsTailParser,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Aeson.Parser.Internal qualified as Aeson.Parser
import Data.Attoparsec.ByteString qualified as Atto
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LByteString
import Data.Csv qualified as Csv
import Data.Csv.Builder qualified as Csv.Builder
import Data.Map.Strict qualified as Map
import Kronor.SmartCsv.ColumnConfig (ColumnConfig)
import Kronor.SmartCsv.Flatten (csvify)
import Kronor.SmartCsv.Pagination (PaginationCursor, PaginationField)
import Kronor.SmartCsv.Pagination qualified as SmartCsvPagination
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

data ParsedResponseRow = ParsedResponseRow
  { encodedRow :: LByteString.ByteString,
    encodedRowBytes :: Int64,
    csvRow :: Map Text Csv.Field
  }

data ResponseRowsStep
  = ResponseRowsEnd
  | ResponseRowsRow ParsedResponseRow
  | ResponseRowsLast ParsedResponseRow

data DecodedResponsePage = DecodedResponsePage
  { encodedRows :: LByteString.ByteString,
    encodedRowBytes :: Int64,
    rowCount :: Int,
    lastRow :: Maybe (Map Text Csv.Field)
  }

decodedResponsePageBytes :: DecodedResponsePage -> LByteString
decodedResponsePageBytes = (.encodedRows)

resolvePaginationKey :: GenericQuery -> Aeson.Key
resolvePaginationKey gq = maybe "createdAt" Aeson.Key.fromText gq.paginationKey

resolvePaginationFields :: GenericQuery -> NonEmpty PaginationField
resolvePaginationFields gq = SmartCsvPagination.resolvePaginationFields (resolvePaginationKey gq) gq.variables

buildRequestBody :: NonEmpty PaginationField -> Int -> Maybe PaginationCursor -> GenericQuery -> LByteString
buildRequestBody paginationFields batchSize mCursor gq =
  Aeson.encode gq {variables = SmartCsvPagination.setPaginationValues paginationFields batchSize mCursor gq.variables}

decodeResponsePage :: ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> ByteString -> Either String (Either ResponseError DecodedResponsePage)
decodeResponsePage colConfig root emptyCsvRow header =
  Atto.parseOnly (responsePageParser colConfig root emptyCsvRow header)

decodeResponseChunk :: Int64 -> ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> ByteString -> Either String (Either ResponseError DecodedResponsePage)
decodeResponseChunk maxChunkBytes colConfig root emptyCsvRow header =
  Atto.parseOnly (responseChunkParser maxChunkBytes colConfig root emptyCsvRow header)

decodeResponsePageWith :: ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> IO ByteString -> IO (Either String (Either ResponseError DecodedResponsePage))
decodeResponsePageWith colConfig root emptyCsvRow header readChunk =
  fmap toParseResult (Atto.parseWith readChunk (responsePageParser colConfig root emptyCsvRow header) mempty)
  where
    toParseResult = \case
      Atto.Fail _ _ err -> Left err
      Atto.Done _ result -> Right result
      Atto.Partial _ -> Left "Unexpected end of GraphQL response"

decodeResponseChunkWith :: Int64 -> ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> IO ByteString -> IO (Either String (Either ResponseError DecodedResponsePage))
decodeResponseChunkWith maxChunkBytes colConfig root emptyCsvRow header readChunk =
  fmap toParseResult (Atto.parseWith readChunk (responseChunkParser maxChunkBytes colConfig root emptyCsvRow header) mempty)
  where
    toParseResult = \case
      Atto.Fail _ _ err -> Left err
      Atto.Done _ result -> Right result
      Atto.Partial _ -> Left "Unexpected end of GraphQL response"

responsePageParser :: ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> Atto.Parser (Either ResponseError DecodedResponsePage)
responsePageParser colConfig root emptyCsvRow header = do
  responseStart <- responseRowsStartParser root
  case responseStart of
    Left err -> pure (Left err)
    Right () -> Right <$> parseRows mempty 0 0 Nothing
  where
    parseRows :: LByteString.ByteString -> Int64 -> Int -> Maybe (Map Text Csv.Field) -> Atto.Parser DecodedResponsePage
    parseRows encoded encodedBytes count mLastRow = do
      rowStep <- responseRowsStepParser colConfig root emptyCsvRow header
      case rowStep of
        ResponseRowsEnd -> do
          responseRowsTailParser
          pure (mkDecodedResponsePage encoded encodedBytes count mLastRow)
        ResponseRowsRow parsedRow ->
          parseRows
            (encoded <> parsedRow.encodedRow)
            (encodedBytes + parsedRow.encodedRowBytes)
            (count + 1)
            (Just parsedRow.csvRow)
        ResponseRowsLast parsedRow -> do
          responseRowsTailParser
          pure
            ( mkDecodedResponsePage
                (encoded <> parsedRow.encodedRow)
                (encodedBytes + parsedRow.encodedRowBytes)
                (count + 1)
                (Just parsedRow.csvRow)
            )

    mkDecodedResponsePage :: LByteString.ByteString -> Int64 -> Int -> Maybe (Map Text Csv.Field) -> DecodedResponsePage
    mkDecodedResponsePage encoded encodedBytes count mLastRow =
      DecodedResponsePage
        { encodedRows = encoded,
          encodedRowBytes = encodedBytes,
          rowCount = count,
          lastRow = mLastRow
        }

decodeResponseRows :: ColumnConfig -> Text -> Map Text Csv.Field -> Aeson.Value -> Either ResponseError (Vector (Map Text Csv.Field))
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

responseRowsStartParser :: Text -> Atto.Parser (Either ResponseError ())
responseRowsStartParser root = do
  skipJsonSpace
  _ <- Atto.word8 123
  parseTopLevelMembers
  where
    parseTopLevelMembers = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 125 -> do
          _ <- Atto.anyWord8
          pure (Left ResponseMissingData)
        _ -> do
          key <- parseObjectKey
          case key of
            "error" -> do
              mErrMsg <- parseErrorField
              pure $ Left $ maybe ResponseMissingData ResponseContainsError mErrMsg
            "data" -> parseDataField
            _ -> do
              _ <- Aeson.Parser.json'
              done <- consumeObjectDelimiter
              if done
                then do
                  _ <- Atto.word8 125
                  skipJsonSpace
                  Atto.endOfInput
                  pure (Left ResponseMissingData)
                else parseTopLevelMembers

    parseDataField = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 123 -> do
          _ <- Atto.anyWord8
          parseDataMembers
        _ -> do
          _ <- Aeson.Parser.json'
          pure (Left ResponseMissingData)

    parseDataMembers = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 125 -> do
          _ <- Atto.anyWord8
          skipRemainingObjectTail
          pure (Left ResponseMissingRootData)
        _ -> do
          key <- parseObjectKey
          if key == root
            then parseRootField
            else do
              _ <- Aeson.Parser.json'
              done <- consumeObjectDelimiter
              if done
                then do
                  _ <- Atto.word8 125
                  skipRemainingObjectTail
                  pure (Left ResponseMissingRootData)
                else parseDataMembers

    parseRootField = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 91 -> Atto.anyWord8 $> Right ()
        _ -> do
          _ <- Aeson.Parser.json'
          pure (Left ResponseMissingRootData)

    skipRemainingObjectTail = do
      skipRemainingObjectMembers
      _ <- Atto.word8 125
      skipJsonSpace
      Atto.endOfInput

responseRowsStepParser :: ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> Atto.Parser ResponseRowsStep
responseRowsStepParser colConfig root emptyCsvRow header = do
  skipJsonSpace
  next <- Atto.peekWord8
  case next of
    Just 93 -> do
      _ <- Atto.anyWord8
      pure ResponseRowsEnd
    _ -> do
      rowValue <- Aeson.Parser.json'
      let row = (`Map.union` emptyCsvRow) (csvify colConfig root rowValue)
          encodedRowBuilder = Csv.Builder.encodeNamedRecordWith Csv.defaultEncodeOptions header row
          encodedRow = Builder.toLazyByteString encodedRowBuilder
          parsedRow = ParsedResponseRow {encodedRow, encodedRowBytes = LByteString.length encodedRow, csvRow = row}
      done <- consumeArrayDelimiter
      pure $ if done then ResponseRowsLast parsedRow else ResponseRowsRow parsedRow

responseRowsTailParser :: Atto.Parser ()
responseRowsTailParser = do
  skipRemainingObjectMembers
  _ <- Atto.word8 125
  skipRemainingObjectMembers
  _ <- Atto.word8 125
  skipJsonSpace
  Atto.endOfInput

parseErrorField :: Atto.Parser (Maybe Text)
parseErrorField = do
  value <- Aeson.Parser.json'
  pure $ case value of
    Aeson.String errMsg -> Just errMsg
    _ -> Nothing

parseObjectKey :: Atto.Parser Text
parseObjectKey = do
  key <- Aeson.Parser.jstring
  skipJsonSpace
  _ <- Atto.word8 58
  pure key

consumeArrayDelimiter :: Atto.Parser Bool
consumeArrayDelimiter = do
  skipJsonSpace
  next <- Atto.peekWord8
  case next of
    Just 44 -> Atto.anyWord8 $> False
    Just 93 -> Atto.anyWord8 $> True
    _ -> fail "Expected ',' or ']' in response array"

consumeObjectDelimiter :: Atto.Parser Bool
consumeObjectDelimiter = do
  skipJsonSpace
  next <- Atto.peekWord8
  case next of
    Just 44 -> Atto.anyWord8 $> False
    Just 125 -> pure True
    _ -> fail "Expected ',' or '}' in response object"

skipRemainingObjectMembers :: Atto.Parser ()
skipRemainingObjectMembers = do
  done <- consumeObjectDelimiter
  unless done do
    _ <- parseObjectKey
    _ <- Aeson.Parser.json'
    skipRemainingObjectMembers

skipJsonSpace :: Atto.Parser ()
skipJsonSpace = Atto.skipWhile isJsonWhitespace

isJsonWhitespace :: Word8 -> Bool
isJsonWhitespace = \case
  9 -> True
  10 -> True
  13 -> True
  32 -> True
  _ -> False

responseChunkParser :: Int64 -> ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> Atto.Parser (Either ResponseError DecodedResponsePage)
responseChunkParser maxChunkBytes colConfig root emptyCsvRow header = do
  skipJsonSpace
  _ <- Atto.word8 123
  parseTopLevelMembers Nothing
  where
    parseTopLevelMembers mError = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 125 -> do
          _ <- Atto.anyWord8
          pure $ maybe (Left ResponseMissingData) (Left . ResponseContainsError) mError
        _ -> do
          key <- parseObjectKey
          case key of
            "error" -> do
              mErrMsg <- parseErrorField
              done <- consumeObjectDelimiter
              case mErrMsg <|> mError of
                Just errMsg -> pure (Left (ResponseContainsError errMsg))
                Nothing ->
                  if done
                    then pure (Left ResponseMissingData)
                    else parseTopLevelMembers Nothing
            "data" -> parseDataField
            _ -> do
              _ <- Aeson.Parser.json'
              done <- consumeObjectDelimiter
              if done
                then pure $ maybe (Left ResponseMissingData) (Left . ResponseContainsError) mError
                else parseTopLevelMembers mError

    parseDataField = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 123 -> do
          _ <- Atto.anyWord8
          parseDataMembers
        _ -> do
          _ <- Aeson.Parser.json'
          pure (Left ResponseMissingData)

    parseDataMembers = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 125 -> do
          _ <- Atto.anyWord8
          pure (Left ResponseMissingRootData)
        _ -> do
          key <- parseObjectKey
          if key == root
            then parseRootField
            else do
              _ <- Aeson.Parser.json'
              done <- consumeObjectDelimiter
              if done
                then pure (Left ResponseMissingRootData)
                else parseDataMembers

    parseRootField = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 91 -> do
          _ <- Atto.anyWord8
          Right <$> parseRows mempty 0 0 Nothing
        _ -> do
          _ <- Aeson.Parser.json'
          pure (Left ResponseMissingRootData)

    parseRows encoded encodedBytes count mLastRow = do
      skipJsonSpace
      next <- Atto.peekWord8
      case next of
        Just 93 -> do
          _ <- Atto.anyWord8
          pure (mkDecodedResponsePage encoded encodedBytes count mLastRow)
        _ -> do
          rowValue <- Aeson.Parser.json'
          let row = (`Map.union` emptyCsvRow) (csvify colConfig root rowValue)
              encodedRowBuilder = Csv.Builder.encodeNamedRecordWith Csv.defaultEncodeOptions header row
              encodedRow = Builder.toLazyByteString encodedRowBuilder
          done <- consumeArrayDelimiter
          let encoded' = encoded <> encodedRow
              encodedBytes' = encodedBytes + LByteString.length encodedRow
              count' = count + 1
              mLastRow' = Just row
          if encodedBytes' >= maxChunkBytes || done
            then pure (mkDecodedResponsePage encoded' encodedBytes' count' mLastRow')
            else parseRows encoded' encodedBytes' count' mLastRow'

    mkDecodedResponsePage encoded encodedBytes count mLastRow =
      DecodedResponsePage
        { encodedRows = encoded,
          encodedRowBytes = encodedBytes,
          rowCount = count,
          lastRow = mLastRow
        }
