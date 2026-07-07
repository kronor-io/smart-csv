module Kronor.SmartCsv.Query
  ( GenericQuery (..),
    DecodedResponsePage (..),
    ResponseError (..),
    buildRequestBody,
    decodeResponseChunk,
    decodeResponseChunkWith,
    decodedResponsePageBytes,
    decodeResponsePage,
    decodeResponsePageWith,
    decodeResponseRows,
    resolvePaginationFields,
    resolvePaginationKey,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import Data.Aeson.Decoding.Tokens (Lit (..), Number (..), TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LByteString
import Data.Csv qualified as Csv
import Data.Csv.Builder qualified as Csv.Builder
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Vector qualified
import Kronor.SmartCsv.ColumnConfig (ColumnConfig)
import Kronor.SmartCsv.Flatten (csvify)
import Kronor.SmartCsv.Pagination (PaginationCursor, PaginationField)
import Kronor.SmartCsv.Pagination qualified as SmartCsvPagination
import RIO

data GenericQuery
  = GenericQuery
  { paginationKey :: Maybe Text,
    orderBy :: Maybe Aeson.Value,
    query :: Text,
    variables :: Aeson.Value
  }
  deriving stock (Generic)

instance Aeson.ToJSON GenericQuery where
  toJSON GenericQuery {paginationKey, query, variables} =
    Aeson.object
      [ "paginationKey" Aeson..= paginationKey,
        "query" Aeson..= query,
        "variables" Aeson..= variables
      ]

data ResponseError
  = ResponseContainsError Text
  | ResponseMissingData
  | ResponseMissingRootData
  deriving stock (Eq, Show)

data DecodedResponsePage = DecodedResponsePage
  { encodedRows :: LByteString.ByteString,
    encodedRowBytes :: Int64,
    rowCount :: Int,
    lastRow :: Maybe (Map Text Csv.Field),
    lastRawRow :: Maybe Aeson.Value
  }

decodedResponsePageBytes :: DecodedResponsePage -> LByteString
decodedResponsePageBytes = (.encodedRows)

resolvePaginationKey :: GenericQuery -> Aeson.Key
resolvePaginationKey gq = maybe "createdAt" Aeson.Key.fromText gq.paginationKey

resolvePaginationFields :: GenericQuery -> NonEmpty PaginationField
resolvePaginationFields gq = SmartCsvPagination.resolvePaginationFields (resolvePaginationKey gq) (applyOrderBy gq.orderBy gq.variables)

buildRequestBody :: NonEmpty PaginationField -> Int -> Maybe PaginationCursor -> GenericQuery -> LByteString
buildRequestBody paginationFields batchSize mCursor gq =
  Aeson.encode gq {variables = applyOrderBy gq.orderBy (SmartCsvPagination.setPaginationValues paginationFields batchSize mCursor gq.variables)}

applyOrderBy :: Maybe Aeson.Value -> Aeson.Value -> Aeson.Value
applyOrderBy Nothing queryVariables = queryVariables
applyOrderBy (Just orderBy) (Aeson.Object obj) = Aeson.Object (Aeson.KeyMap.insert "orderBy" orderBy obj)
applyOrderBy (Just _) queryVariables = queryVariables

-- | Decode a full GraphQL response (all rows) from a strict 'ByteString'.
decodeResponsePage :: ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> ByteString -> Either String (Either ResponseError DecodedResponsePage)
decodeResponsePage colConfig root emptyCsvRow header =
  tokenDecodePage Nothing colConfig root emptyCsvRow header . LByteString.fromStrict

-- | Decode rows until the encoded CSV reaches @maxChunkBytes@ (or the row array
-- ends), from a strict 'ByteString'.
decodeResponseChunk :: Int64 -> ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> ByteString -> Either String (Either ResponseError DecodedResponsePage)
decodeResponseChunk maxChunkBytes colConfig root emptyCsvRow header =
  tokenDecodePage (Just maxChunkBytes) colConfig root emptyCsvRow header . LByteString.fromStrict

-- | Streaming variant of 'decodeResponsePage' that pulls the response from a
-- chunk reader (e.g. an HTTP body reader).  The body is read into a strict
-- 'ByteString' while the connection is open, then decoded purely.
decodeResponsePageWith :: ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> IO ByteString -> IO (Either String (Either ResponseError DecodedResponsePage))
decodeResponsePageWith colConfig root emptyCsvRow header readChunk =
  decodeResponsePage colConfig root emptyCsvRow header <$> readAllChunks readChunk

-- | Streaming variant of 'decodeResponseChunk' that pulls the response from a
-- chunk reader (e.g. an HTTP body reader).  The body is read into a strict
-- 'ByteString' while the connection is open, then decoded purely.
decodeResponseChunkWith :: Int64 -> ColumnConfig -> Text -> Map Text Csv.Field -> Vector Csv.Name -> IO ByteString -> IO (Either String (Either ResponseError DecodedResponsePage))
decodeResponseChunkWith maxChunkBytes colConfig root emptyCsvRow header readChunk =
  decodeResponseChunk maxChunkBytes colConfig root emptyCsvRow header <$> readAllChunks readChunk

-- | Fully consume a chunk reader into a single strict 'ByteString'.
readAllChunks :: IO ByteString -> IO ByteString
readAllChunks readChunk = go []
  where
    go acc = do
      chunk <- readChunk
      if ByteString.null chunk
        then pure (ByteString.concat (reverse acc))
        else go (chunk : acc)

-- | Walk the GraphQL response token stream, flattening each row of
-- @data.<root>@ into a CSV record.  Streaming: only one row's 'Aeson.Value' is
-- built at a time before it is encoded and discarded.  With @Just maxChunkBytes@
-- it stops once the encoded CSV reaches that size.
tokenDecodePage ::
  Maybe Int64 ->
  ColumnConfig ->
  Text ->
  Map Text Csv.Field ->
  Vector Csv.Name ->
  LByteString ->
  Either String (Either ResponseError DecodedResponsePage)
tokenDecodePage mMaxChunkBytes colConfig root emptyCsvRow header lbs =
  case lbsToTokens lbs of
    TkErr err -> Left err
    TkRecordOpen responseObj -> topLevel responseObj
    _ -> Right (Left ResponseMissingData)
  where
    rootKey = Aeson.Key.fromText root

    topLevel :: TkRecord k String -> Either String (Either ResponseError DecodedResponsePage)
    topLevel = \case
      TkRecordErr err -> Left err
      TkRecordEnd _ -> Right (Left ResponseMissingData)
      TkPair key valueTokens
        | key == "error" -> case valueTokens of
            TkText message _ -> Right (Left (ResponseContainsError message))
            _ -> skipValue valueTokens >>= topLevel
        | key == "data" -> case valueTokens of
            TkErr err -> Left err
            TkRecordOpen dataObj -> dataLevel dataObj
            _ -> Right (Left ResponseMissingData)
        | otherwise -> skipValue valueTokens >>= topLevel

    dataLevel :: TkRecord k String -> Either String (Either ResponseError DecodedResponsePage)
    dataLevel = \case
      TkRecordErr err -> Left err
      TkRecordEnd _ -> Right (Left ResponseMissingRootData)
      TkPair key valueTokens
        | key == rootKey -> case valueTokens of
            TkErr err -> Left err
            TkArrayOpen rowsArray -> foldRows rowsArray
            _ -> Right (Left ResponseMissingRootData)
        | otherwise -> skipValue valueTokens >>= dataLevel

    foldRows :: TkArray k String -> Either String (Either ResponseError DecodedResponsePage)
    foldRows = go [] 0 0 Nothing Nothing
      where
        go !revEncoded !encodedBytes !count !mLastRow !mLastRawRow = \case
          TkArrayErr err -> Left err
          TkArrayEnd _ -> Right (Right (mkPage revEncoded encodedBytes count mLastRow mLastRawRow))
          TkItem rowTokens -> do
            (rowValue, rest) <- tokensToValue rowTokens
            let row = csvify colConfig root rowValue `Map.union` emptyCsvRow
                encodedRow = Builder.toLazyByteString (Csv.Builder.encodeNamedRecordWith Csv.defaultEncodeOptions header row)
                revEncoded' = encodedRow : revEncoded
                encodedBytes' = encodedBytes + LByteString.length encodedRow
                count' = count + 1
            case mMaxChunkBytes of
              Just maxChunkBytes
                | encodedBytes' >= maxChunkBytes ->
                    Right (Right (mkPage revEncoded' encodedBytes' count' (Just row) (Just rowValue)))
              _ -> go revEncoded' encodedBytes' count' (Just row) (Just rowValue) rest

    mkPage revEncoded encodedBytes count mLastRow mLastRawRow =
      DecodedResponsePage
        { encodedRows = LByteString.concat (reverse revEncoded),
          encodedRowBytes = encodedBytes,
          rowCount = count,
          lastRow = mLastRow,
          lastRawRow = mLastRawRow
        }

-- | Consume one value's tokens without building it, returning the continuation.
skipValue :: Tokens k String -> Either String k
skipValue = \case
  TkLit _ k -> Right k
  TkText _ k -> Right k
  TkNumber _ k -> Right k
  TkArrayOpen arr -> skipArray arr
  TkRecordOpen rec -> skipRecord rec
  TkErr err -> Left err
  where
    skipArray = \case
      TkItem toks -> skipValue toks >>= skipArray
      TkArrayEnd k -> Right k
      TkArrayErr err -> Left err
    skipRecord = \case
      TkPair _ toks -> skipValue toks >>= skipRecord
      TkRecordEnd k -> Right k
      TkRecordErr err -> Left err

-- | Build one 'Aeson.Value' from a token stream, returning the continuation.
tokensToValue :: Tokens k String -> Either String (Aeson.Value, k)
tokensToValue = \case
  TkLit LitNull k -> Right (Aeson.Null, k)
  TkLit LitTrue k -> Right (Aeson.Bool True, k)
  TkLit LitFalse k -> Right (Aeson.Bool False, k)
  TkText text k -> Right (Aeson.String text, k)
  TkNumber number k -> Right (Aeson.Number (numberToScientific number), k)
  TkArrayOpen arr -> do
    (values, k) <- arrayValues arr
    Right (Aeson.Array (Data.Vector.fromList values), k)
  TkRecordOpen rec -> do
    (pairs, k) <- recordPairs rec
    Right (Aeson.Object (Aeson.KeyMap.fromList pairs), k)
  TkErr err -> Left err
  where
    arrayValues = \case
      TkItem toks -> do
        (value, rest) <- tokensToValue toks
        (values, k) <- arrayValues rest
        Right (value : values, k)
      TkArrayEnd k -> Right ([], k)
      TkArrayErr err -> Left err
    recordPairs = \case
      TkPair key toks -> do
        (value, rest) <- tokensToValue toks
        (pairs, k) <- recordPairs rest
        Right ((key, value) : pairs, k)
      TkRecordEnd k -> Right ([], k)
      TkRecordErr err -> Left err

numberToScientific :: Number -> Scientific
numberToScientific = \case
  NumInteger i -> fromInteger i
  NumDecimal s -> s
  NumScientific s -> s

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
