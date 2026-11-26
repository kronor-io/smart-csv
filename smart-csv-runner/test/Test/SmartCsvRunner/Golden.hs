-- | Byte-for-byte checks of the files the runner writes today, CSV and Excel,
-- for requests shaped like the ones deployments send.  They pin the current
-- output so that a change to the export (a new engine, a dependency bump) shows
-- up as a diff here instead of in someone's spreadsheet.
--
-- The expected files live in @test/golden@.  To record them again after an
-- intended change, run the integration tests with @SMART_CSV_ACCEPT_GOLDEN=1@
-- and review the diff.
module Test.SmartCsvRunner.Golden
  ( goldenTests,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Hasql.Pool qualified
import Hasql.Session qualified
import Hasql.TH (singletonStatement)
import Network.HTTP.Simple qualified as HTTP
import RIO
import RIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removePathForcibly)
import RIO.FilePath (makeRelative, takeDirectory, (</>))
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import Test.SmartCsvRunner.Integration (extractReportId, fetchCsvLink, mkPool, postHasuraMetadata, postRestApiWithAuth, runSession, testShardId, waitFor)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)

goldenTests :: TestTree
goldenTests =
  withResource setupFixture cleanupFixture \getPool ->
    testGroup
      "Integration golden files"
      [ testCase "CSV without a column config" (getPool >>= \pool -> goldenExport pool "plain" Csv plainRequest),
        testCase "CSV with headers, decimal places, data paths and a nested orderBy" (getPool >>= \pool -> goldenExport pool "config" Csv configRequest),
        testCase "Excel with headers, decimal places, data paths and a nested orderBy" (getPool >>= \pool -> goldenExport pool "config" Xlsx configRequest),
        testCase "CSV with numeric columns detected from the schema" (getPool >>= \pool -> goldenExport pool "detect-numeric" Csv detectRequest),
        testCase "Excel with numeric columns detected from the schema" (getPool >>= \pool -> goldenExport pool "detect-numeric" Xlsx detectRequest)
      ]

data Format = Csv | Xlsx

formatName :: Format -> Text
formatName = \case
  Csv -> "csv"
  Xlsx -> "xlsx"

-- ──────────────────────────────────────────────────────────────
-- Requests
-- ──────────────────────────────────────────────────────────────

-- | Every scalar type, a single-key object and an array, with no config: raw
-- headers and the default rendering.  Two orders share a createdAt, so the id
-- breaks the tie.
plainRequest :: Aeson.Value
plainRequest =
  request
    "createdAt"
    [Aeson.object ["createdAt" Aeson..= ("DESC" :: Text)], Aeson.object ["id" Aeson..= ("DESC" :: Text)]]
    "id reference amount rate quantity big paid note createdAt customer { name } lines(orderBy: {id: ASC}) { sku }"
    []

-- | Headers, decimal places, a data path into an object with several fields and
-- into an array, ordered by a field of a related object.
configRequest :: Aeson.Value
configRequest =
  request
    "customer.name"
    [Aeson.object ["customer" Aeson..= Aeson.object ["name" Aeson..= ("ASC" :: Text)]], Aeson.object ["id" Aeson..= ("ASC" :: Text)]]
    "id reference amount rate customer { name email } lines(orderBy: {id: ASC}) { price }"
    [ "columnConfig"
        Aeson..= Aeson.object
          [ "id" Aeson..= Aeson.object ["header" Aeson..= ("Order ID" :: Text)],
            "reference" Aeson..= Aeson.object ["header" Aeson..= ("Reference" :: Text)],
            "amount" Aeson..= Aeson.object ["header" Aeson..= ("Amount" :: Text), "decimalPlaces" Aeson..= (2 :: Int)],
            "rate" Aeson..= Aeson.object ["header" Aeson..= ("Rate" :: Text), "decimalPlaces" Aeson..= (3 :: Int)],
            "customer" Aeson..= Aeson.object ["header" Aeson..= ("Customer Email" :: Text), "dataPath" Aeson..= ("email" :: Text)],
            "lines" Aeson..= Aeson.object ["header" Aeson..= ("Line Prices" :: Text), "dataPath" Aeson..= ("price" :: Text), "decimalPlaces" Aeson..= (2 :: Int)]
          ]
    ]

-- | Numbers Hasura sends as strings, printed with a decimal comma because the
-- schema says they are numeric.
detectRequest :: Aeson.Value
detectRequest =
  request
    "createdAt"
    [Aeson.object ["createdAt" Aeson..= ("DESC" :: Text)], Aeson.object ["id" Aeson..= ("DESC" :: Text)]]
    "id amount rate quantity big createdAt customer { balance }"
    ["detectNumericColumns" Aeson..= True]

request :: Text -> [Aeson.Value] -> Text -> [(Aeson.Key, Aeson.Value)] -> Aeson.Value
request paginationKey orderBy fields extra =
  Aeson.object
    $ [ "shardId" Aeson..= testShardId,
        "recipient" Aeson..= ("golden@example.com" :: Text),
        "graphqlPaginationKey" Aeson..= paginationKey,
        "orderBy" Aeson..= orderBy,
        "graphqlQueryBody"
          Aeson..= ( "query Q($rowLimit: Int!, $paginationCondition: SmartCsvGoldenOrderBoolExp!, $conditions: SmartCsvGoldenOrderBoolExp!, $orderBy: [SmartCsvGoldenOrderOrderBy!]) \
                     \{ smartCsvGoldenOrder(limit: $rowLimit, where: {_and: [$paginationCondition, $conditions]}, orderBy: $orderBy) { "
                       <> fields
                       <> " } }"
                   ),
        "graphqlQueryVariables" Aeson..= ("{\"conditions\":{\"createdAt\":{\"_gte\":\"2026-01-01T00:00:00Z\",\"_lt\":\"2026-02-01T00:00:00Z\"}}}" :: Text)
      ]
      <> extra

-- ──────────────────────────────────────────────────────────────
-- Running an export and comparing its file
-- ──────────────────────────────────────────────────────────────

goldenExport :: Hasql.Pool.Pool -> Text -> Format -> Aeson.Value -> IO ()
goldenExport pool name format requestBody = do
  let body = case requestBody of
        Aeson.Object fields -> Aeson.Object (KeyMap.insert "format" (Aeson.String (formatName format)) fields)
        other -> other
  response <- postRestApiWithAuth "/api/v1/csv/generate" body
  let status = HTTP.getResponseStatusCode response
  when (status /= 200)
    $ assertFailure [i|Expected HTTP 200 but got #{status}: #{decodeUtf8Lenient (LBS.toStrict (HTTP.getResponseBody response))}|]
  reportId <- extractReportId response
  -- Done once the link is there; a failed report stops the wait with its message.
  waitFor 60 $ do
    result <-
      Hasql.Pool.use pool
        $ Hasql.Session.statement
          reportId
          [singletonStatement|
            select status::text, link::text?, err_message::text?
            from smart_csv.generated_csv
            where id = $1::bigint
          |]
    case result of
      Right ("ERROR", _, mMessage) -> assertFailure [i|The export failed: #{fromMaybe "" mMessage}|] >> pure False
      Right (_, mLink, _) -> pure (isJust mLink)
      Left err -> assertFailure (show err) >> pure False
  fileLink <- fetchCsvLink pool reportId
  fileRequest <- HTTP.parseRequest (Text.unpack fileLink)
  bytes <- HTTP.getResponseBody <$> HTTP.httpBS fileRequest
  let goldenName = Text.unpack name <> "." <> Text.unpack (formatName format)
  case format of
    Csv -> compareFile ("test/golden" </> goldenName) bytes
    -- The ZIP's compressed bytes depend on the zlib build, so compare what it
    -- holds: every entry, after Info-ZIP's unzip has checked the archive.
    Xlsx -> withSystemTempDirectory "smart-csv-golden" \directory -> do
      let xlsxPath = directory </> "export.xlsx"
          extracted = directory </> "entries"
      writeFileBinary xlsxPath bytes
      runUnzip ["-tq", xlsxPath]
      runUnzip ["-q", xlsxPath, "-d", extracted]
      compareDirectory ("test/golden" </> goldenName) extracted

runUnzip :: [String] -> IO ()
runUnzip arguments = do
  (exitCode, out, err) <- readProcessWithExitCode "unzip" arguments ""
  unless (exitCode == ExitSuccess)
    $ assertFailure [i|unzip #{unwords arguments} failed (#{exitCode}): #{out}#{err}|]

acceptGolden :: IO Bool
acceptGolden = isJust <$> lookupEnv "SMART_CSV_ACCEPT_GOLDEN"

compareFile :: FilePath -> ByteString -> IO ()
compareFile goldenPath actual = do
  accept <- acceptGolden
  exists <- doesFileExist goldenPath
  if accept
    then createDirectoryIfMissing True (takeDirectory goldenPath) >> writeFileBinary goldenPath actual
    else
      if not exists
        then assertFailure [i|#{goldenPath} is missing; run with SMART_CSV_ACCEPT_GOLDEN=1 to record it|]
        else do
          expected <- readFileBinary goldenPath
          unless (expected == actual) $ assertFailure (describeDifference goldenPath expected actual)

compareDirectory :: FilePath -> FilePath -> IO ()
compareDirectory goldenDir actualDir = do
  accept <- acceptGolden
  when accept $ removePathForcibly goldenDir
  actualFiles <- filesUnder actualDir
  expectedFiles <- do
    exists <- doesDirectoryExist goldenDir
    if exists then filesUnder goldenDir else pure []
  unless (accept || expectedFiles == actualFiles)
    $ assertFailure [i|#{goldenDir} holds #{expectedFiles}, but the file holds #{actualFiles}|]
  for_ actualFiles \path -> readFileBinary (actualDir </> path) >>= compareFile (goldenDir </> path)

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = List.sort . map (makeRelative root) <$> go root
  where
    go path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then concat <$> (listDirectory path >>= traverse (go . (path </>)))
        else pure [path]

-- | Where the files first differ, with some context on both sides.
describeDifference :: FilePath -> ByteString -> ByteString -> String
describeDifference goldenPath expected actual =
  let offset = length (takeWhile id (ByteString.zipWith (==) expected actual))
      around bytes = decodeUtf8Lenient (ByteString.take 160 (ByteString.drop (max 0 (offset - 40)) bytes))
   in [i|The file differs from #{goldenPath} at byte #{offset} (expected #{ByteString.length expected} bytes, got #{ByteString.length actual}).
  expected: #{show (around expected)}
  got:      #{show (around actual)}
Run with SMART_CSV_ACCEPT_GOLDEN=1 to record the new output if the change is intended.|]

-- ──────────────────────────────────────────────────────────────
-- Fixture
-- ──────────────────────────────────────────────────────────────

goldenTables :: [Text]
goldenTables = ["golden_line", "golden_order", "golden_customer"]

setupFixture :: IO Hasql.Pool.Pool
setupFixture = do
  pool <- mkPool
  dropFixture pool
  runSession pool $ Hasql.Session.sql (encodeUtf8 fixtureSql)
  for_ (reverse goldenTables) \table -> do
    hasura "pg_track_table" ["table" Aeson..= tableRef table]
    hasura
      "pg_create_select_permission"
      [ "table" Aeson..= tableRef table,
        "role" Aeson..= ("smart-csv" :: Text),
        "permission"
          Aeson..= Aeson.object
            [ "columns" Aeson..= ("*" :: Text),
              "filter" Aeson..= Aeson.object ["shard_id" Aeson..= Aeson.object ["_eq" Aeson..= ("x-hasura-shard-id" :: Text)]],
              "allow_aggregations" Aeson..= False
            ]
      ]
  hasura
    "pg_create_object_relationship"
    [ "table" Aeson..= tableRef "golden_order",
      "name" Aeson..= ("customer" :: Text),
      "using" Aeson..= Aeson.object ["foreign_key_constraint_on" Aeson..= ("customer_id" :: Text)]
    ]
  hasura
    "pg_create_array_relationship"
    [ "table" Aeson..= tableRef "golden_order",
      "name" Aeson..= ("lines" :: Text),
      "using" Aeson..= Aeson.object ["foreign_key_constraint_on" Aeson..= Aeson.object ["table" Aeson..= tableRef "golden_line", "column" Aeson..= ("order_id" :: Text)]]
    ]
  threadDelay 500_000 -- let Hasura reload its schema
  pure pool
  where
    tableRef :: Text -> Aeson.Value
    tableRef table = Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= table]

    hasura :: Text -> [(Aeson.Key, Aeson.Value)] -> IO ()
    hasura operation args = do
      response <- postHasuraMetadata (Aeson.object ["type" Aeson..= operation, "args" Aeson..= Aeson.object (("source" Aeson..= ("default" :: Text)) : args)])
      let status = HTTP.getResponseStatusCode response
      unless (status == 200)
        $ assertFailure [i|Hasura #{operation} failed (#{status}): #{decodeUtf8Lenient (LBS.toStrict (HTTP.getResponseBody response))}|]

cleanupFixture :: Hasql.Pool.Pool -> IO ()
cleanupFixture pool = dropFixture pool >> Hasql.Pool.release pool

dropFixture :: Hasql.Pool.Pool -> IO ()
dropFixture pool = do
  for_ goldenTables \table ->
    void
      $ postHasuraMetadata
      $ Aeson.object
        [ "type" Aeson..= ("pg_untrack_table" :: Text),
          "args"
            Aeson..= Aeson.object
              [ "source" Aeson..= ("default" :: Text),
                "table" Aeson..= Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= table],
                "cascade" Aeson..= True
              ]
        ]
  void $ Hasql.Pool.use pool $ Hasql.Session.sql "DROP TABLE IF EXISTS smart_csv.golden_line, smart_csv.golden_order, smart_csv.golden_customer"

-- | Values that tend to break files: separators, quotes and line breaks in text,
-- a leading formula character, non-ASCII names, an empty string next to nulls,
-- negative and very large numbers, a bigint past 2^53, tiny and long floats.
fixtureSql :: Text
fixtureSql =
  "CREATE TABLE smart_csv.golden_customer (\
  \  id int PRIMARY KEY,\
  \  shard_id bigint NOT NULL,\
  \  name text NOT NULL,\
  \  email text NOT NULL,\
  \  balance numeric(12,2) NOT NULL\
  \);\
  \CREATE TABLE smart_csv.golden_order (\
  \  id bigint PRIMARY KEY,\
  \  shard_id bigint NOT NULL,\
  \  customer_id int NOT NULL REFERENCES smart_csv.golden_customer (id),\
  \  reference text NOT NULL,\
  \  amount numeric(12,2) NOT NULL,\
  \  rate float8,\
  \  quantity int,\
  \  big bigint,\
  \  paid boolean,\
  \  note text,\
  \  created_at timestamptz NOT NULL\
  \);\
  \CREATE TABLE smart_csv.golden_line (\
  \  id int PRIMARY KEY,\
  \  shard_id bigint NOT NULL,\
  \  order_id bigint NOT NULL REFERENCES smart_csv.golden_order (id),\
  \  sku text NOT NULL,\
  \  price numeric(12,2) NOT NULL\
  \);\
  \INSERT INTO smart_csv.golden_customer VALUES\
  \  (1, 1, 'Ada Lovelace', 'ada@example.com', 100.00),\
  \  (2, 1, 'Bob, Jr.', 'bob@example.com', -5.50),\
  \  (3, 1, 'Åsa Ödegård', 'asa@example.se', 0.10);\
  \INSERT INTO smart_csv.golden_order VALUES\
  \  (1, 1, 1, 'A-1', 10.50, 0.1, 3, 9007199254740993, true, 'plain', '2026-01-15T10:00:00Z'),\
  \  (2, 1, 2, 'B, with comma', -1234.56, 2.5, 0, 42, false, NULL, '2026-01-15T10:00:00Z'),\
  \  (3, 1, 3, 'quote \"inside\"', 0.00, 0.0000001, -7, -1, true, E'line one\\nline two', '2026-01-15T11:30:00Z'),\
  \  (4, 1, 1, '=SUM(A1:A2)', 99999999.99, 123456.789, 1, 0, false, '', '2026-01-16T09:15:00Z'),\
  \  (5, 1, 2, '+46 70 123', 1.10, NULL, NULL, NULL, NULL, ';semicolon', '2026-01-17T00:00:00Z');\
  \INSERT INTO smart_csv.golden_line VALUES\
  \  (1, 1, 1, 'SKU-1', 5.25),\
  \  (2, 1, 1, 'SKU-2', 5.25),\
  \  (3, 1, 3, 'SKU-3', 0.10),\
  \  (4, 1, 4, 'SKU-4', 99999999.99),\
  \  (5, 1, 4, 'SKU-5', -0.01);"
