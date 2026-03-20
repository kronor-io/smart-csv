module Test.SmartCsvRunner.Integration
  ( integrationTests,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as CSV
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified
import Hasql.TH (resultlessStatement, singletonStatement)
import Network.HTTP.Simple qualified as HTTP
import RIO
import Data.Char (chr)
import Data.Text qualified as Text
import RIO.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, secondsToNominalDiffTime)
import RIO.Vector qualified as Vec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- | Port the smart-csv-runner REST API listens on.
apiPort :: Int
apiPort = 8000

-- | Shard ID for test data. Must match an organization/shard that exists or
-- is irrelevant (the test creates its own table).
testShardId :: Int64
testShardId = 1

integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration (requires docker-compose stack + running smart-csv-runner)"
    [ testCase "REST API rejects invalid payload" testRejectsInvalidPayload,
      testCase "REST API generates CSV end-to-end" testGeneratesCsvEndToEnd,
      testCase "REST API generates CSV with inline columnConfig" testGeneratesCsvWithInlineColumnConfig,
      testCase "REST API generates CSV with named columnConfigName" testGeneratesCsvWithNamedColumnConfig
    ]

-- ──────────────────────────────────────────────────────────────
-- Test: validation rejects invalid payload
-- ──────────────────────────────────────────────────────────────

testRejectsInvalidPayload :: IO ()
testRejectsInvalidPayload = do
  now <- getCurrentTime
  let oneHour = addUTCTime (secondsToNominalDiffTime 3600) now
  let input =
        Aeson.object
          [ "shardId" Aeson..= (0 :: Int),
            "recipient" Aeson..= ("test@example.com" :: Text),
            "graphqlPaginationKey" Aeson..= ("createdAt" :: Text),
            "graphqlQueryBody" Aeson..= testTableQueryBody,
            "graphqlQueryVariables" Aeson..= mkQueryVariables now oneHour
          ]
  resp <- postRestApi "/api/v1/csv/generate" input
  HTTP.getResponseStatusCode resp @?= 400

-- ──────────────────────────────────────────────────────────────
-- Test: happy-path end-to-end CSV generation
-- ──────────────────────────────────────────────────────────────

testGeneratesCsvEndToEnd :: IO ()
testGeneratesCsvEndToEnd = do
  pool <- mkPool
  now <- getCurrentTime
  let oneHour = addUTCTime (secondsToNominalDiffTime 3600) now

  -- 0. Cleanup from any previous run
  cleanup pool
  deleteAllMailhogMessages

  -- 1. Create test table
  runSession pool $ do
    Hasql.Session.sql "DROP TABLE IF EXISTS smart_csv.e2e_test_data"
    Hasql.Session.sql
      "CREATE TABLE smart_csv.e2e_test_data (\
      \  id bigserial PRIMARY KEY,\
      \  shard_id bigint NOT NULL,\
      \  amount bigint NOT NULL,\
      \  reference text NOT NULL,\
      \  created_at timestamptz NOT NULL DEFAULT now()\
      \)"

  -- 2. Track in Hasura + grant select permission
  void
    $ postHasuraMetadata
    $ Aeson.object
      [ "type" Aeson..= ("pg_track_table" :: Text),
        "args"
          Aeson..= Aeson.object
            [ "source" Aeson..= ("default" :: Text),
              "table" Aeson..= Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= ("e2e_test_data" :: Text)]
            ]
      ]

  void
    $ postHasuraMetadata
    $ Aeson.object
      [ "type" Aeson..= ("pg_create_select_permission" :: Text),
        "args"
          Aeson..= Aeson.object
            [ "source" Aeson..= ("default" :: Text),
              "table" Aeson..= Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= ("e2e_test_data" :: Text)],
              "role" Aeson..= ("smart-csv" :: Text),
              "permission"
                Aeson..= Aeson.object
                  [ "columns" Aeson..= (["id", "shard_id", "amount", "reference", "created_at"] :: [Text]),
                    "filter" Aeson..= Aeson.object ["shard_id" Aeson..= Aeson.object ["_eq" Aeson..= ("x-hasura-shard-id" :: Text)]]
                  ]
            ]
      ]

  threadDelay 500_000 -- wait for Hasura schema reload

  -- 3. Insert test data
  runSession pool
    $ Hasql.Session.statement
      testShardId
      [resultlessStatement|
                INSERT INTO smart_csv.e2e_test_data (shard_id, amount, reference, created_at)
                VALUES ($1::bigint, 100, 'ref-aaa', now()),
                       ($1::bigint, 200, 'ref-bbb', now()),
                       ($1::bigint, 300, 'ref-ccc', now())
            |]

  -- 4. POST to REST API
  let restInput =
        Aeson.object
          [ "shardId" Aeson..= testShardId,
            "recipient" Aeson..= ("test@example.com" :: Text),
            "graphqlPaginationKey" Aeson..= ("createdAt" :: Text),
            "graphqlQueryBody" Aeson..= testTableQueryBody,
            "graphqlQueryVariables" Aeson..= mkQueryVariables now oneHour
          ]

  resp <- postRestApiWithAuth pool "/api/v1/csv/generate" restInput
  let status = HTTP.getResponseStatusCode resp
  when (status /= 200)
    $ assertFailure [i|Expected HTTP 200 but got #{status}: #{decodeUtf8Lenient (LBS.toStrict (HTTP.getResponseBody resp))}|]

  -- Extract reportId
  let respBody = HTTP.getResponseBody resp
  reportId <- case Aeson.decode respBody >>= \(obj :: Aeson.Value) -> obj ^? key "reportId" . _Integer of
    Just rid -> pure (fromInteger rid :: Int64)
    Nothing -> assertFailure [i|No reportId in response: #{decodeUtf8Lenient (LBS.toStrict respBody)}|] >> error "unreachable"

  -- 5. Wait for CSV link (poll up to 30 seconds)
  waitFor 30 $ do
    result <-
      Hasql.Pool.use pool
        $ Hasql.Session.statement
          reportId
          [singletonStatement|
                    select exists(
                        select 1
                        from smart_csv.generated_csv
                        where id = $1::bigint and link is not null
                    )::bool
                |]
    pure $ result == Right True

  -- 6. Fetch download link
  csvLink <- do
    result <-
      Hasql.Pool.use pool
        $ Hasql.Session.statement
          reportId
          [singletonStatement|
                    select link::text
                    from smart_csv.generated_csv
                    where id = $1::bigint
                |]
    case result of
      Right lnk -> pure lnk
      Left err -> assertFailure (show err) >> error "unreachable"

  -- 7. Download CSV and verify
  csvRequest <- HTTP.parseRequest (Text.unpack csvLink)
  csvBytes <- HTTP.getResponseBody <$> HTTP.httpBS csvRequest
  csvText <- case decodeUtf8' csvBytes of
    Right t -> pure t
    Left err -> assertFailure ("CSV decode error: " <> show err) >> error "unreachable"

  let parsed = CSV.decodeByName (LBS.fromStrict (encodeUtf8 csvText))
  rows <- case parsed of
    Right (_, rs) -> pure (Vec.toList rs :: [Map.Map Text Text])
    Left err -> assertFailure [i|CSV parse error: #{err}\n#{csvText}|] >> error "unreachable"

  length rows @?= 3

  let refCol = "smartCsvE2eTestData_reference"
      amtCol = "smartCsvE2eTestData_amount"
      refAmounts = Map.fromList [(r, a) | row <- rows, Just r <- [Map.lookup refCol row], Just a <- [Map.lookup amtCol row]]
  -- Hasura returns bigint as a number, CSV may have ".0" suffix
  let stripDecimal t = fromMaybe t (Text.stripSuffix ".0" t)
      refAmountsNorm = Map.map stripDecimal refAmounts
  Map.lookup "ref-aaa" refAmountsNorm @?= Just "100"
  Map.lookup "ref-bbb" refAmountsNorm @?= Just "200"
  Map.lookup "ref-ccc" refAmountsNorm @?= Just "300"

  -- 8. Verify completion email was sent with a valid download link
  assertEmailSentWithValidLink "test@example.com" csvLink

  -- 9. Cleanup
  cleanup pool

-- ──────────────────────────────────────────────────────────────
-- Test: end-to-end CSV generation with inline columnConfig
-- ──────────────────────────────────────────────────────────────

testGeneratesCsvWithInlineColumnConfig :: IO ()
testGeneratesCsvWithInlineColumnConfig = do
  pool <- mkPool
  now <- getCurrentTime
  let oneHour = addUTCTime (secondsToNominalDiffTime 3600) now

  -- 0. Cleanup from any previous run
  cleanup pool
  deleteAllMailhogMessages

  -- 1. Create test table + track + insert data
  setupTestTable pool

  threadDelay 500_000 -- wait for Hasura schema reload
  runSession pool
    $ Hasql.Session.statement
      testShardId
      [resultlessStatement|
                INSERT INTO smart_csv.e2e_test_data (shard_id, amount, reference, created_at)
                VALUES ($1::bigint, 100, 'ref-aaa', now()),
                       ($1::bigint, 200, 'ref-bbb', now()),
                       ($1::bigint, 300, 'ref-ccc', now())
            |]

  -- 2. POST with inline columnConfig that renames columns
  let inlineConfig =
        Aeson.object
          [ "smartCsvE2eTestData_reference" Aeson..= ("Reference" :: Text),
            "smartCsvE2eTestData_amount" Aeson..= ("Amount" :: Text),
            "smartCsvE2eTestData_createdAt" Aeson..= ("Created" :: Text)
          ]
  let restInput =
        Aeson.object
          [ "shardId" Aeson..= testShardId,
            "recipient" Aeson..= ("test@example.com" :: Text),
            "graphqlPaginationKey" Aeson..= ("createdAt" :: Text),
            "graphqlQueryBody" Aeson..= testTableQueryBody,
            "graphqlQueryVariables" Aeson..= mkQueryVariables now oneHour,
            "columnConfig" Aeson..= inlineConfig
          ]

  resp <- postRestApiWithAuth pool "/api/v1/csv/generate" restInput
  let status = HTTP.getResponseStatusCode resp
  when (status /= 200)
    $ assertFailure [i|Expected HTTP 200 but got #{status}: #{decodeUtf8Lenient (LBS.toStrict (HTTP.getResponseBody resp))}|]

  reportId <- extractReportId resp

  -- 3. Wait for CSV link
  waitFor 30 $ do
    result <-
      Hasql.Pool.use pool
        $ Hasql.Session.statement
          reportId
          [singletonStatement|
                    select exists(
                        select 1
                        from smart_csv.generated_csv
                        where id = $1::bigint and link is not null
                    )::bool
                |]
    pure $ result == Right True

  -- 4. Download and verify CSV uses renamed columns
  csvLink <- fetchCsvLink pool reportId
  rows <- downloadAndParseCsv csvLink

  length rows @?= 3

  -- Verify the renamed column headers are used
  let refCol = "Reference"
      amtCol = "Amount"
      refAmounts = Map.fromList [(r, a) | row <- rows, Just r <- [Map.lookup refCol row], Just a <- [Map.lookup amtCol row]]
  let stripDecimal t = fromMaybe t (Text.stripSuffix ".0" t)
      refAmountsNorm = Map.map stripDecimal refAmounts
  Map.lookup "ref-aaa" refAmountsNorm @?= Just "100"
  Map.lookup "ref-bbb" refAmountsNorm @?= Just "200"
  Map.lookup "ref-ccc" refAmountsNorm @?= Just "300"

  -- Verify raw column names are NOT present
  let allHeaders = concatMap Map.keys rows
  assertNotElem "smartCsvE2eTestData_reference" allHeaders
  assertNotElem "smartCsvE2eTestData_amount" allHeaders

  -- 5. Verify completion email was sent with a valid download link
  assertEmailSentWithValidLink "test@example.com" csvLink

  -- 6. Cleanup
  cleanup pool

-- ──────────────────────────────────────────────────────────────
-- Test: end-to-end CSV generation with named columnConfigName
-- ──────────────────────────────────────────────────────────────

testGeneratesCsvWithNamedColumnConfig :: IO ()
testGeneratesCsvWithNamedColumnConfig = do
  pool <- mkPool
  now <- getCurrentTime
  let oneHour = addUTCTime (secondsToNominalDiffTime 3600) now

  -- 0. Cleanup from any previous run
  cleanup pool
  cleanupColumnConfigPreset pool
  deleteAllMailhogMessages

  -- 1. Create test table + track + insert data
  setupTestTable pool

  threadDelay 500_000 -- wait for Hasura schema reload
  runSession pool
    $ Hasql.Session.statement
      testShardId
      [resultlessStatement|
                INSERT INTO smart_csv.e2e_test_data (shard_id, amount, reference, created_at)
                VALUES ($1::bigint, 100, 'ref-aaa', now()),
                       ($1::bigint, 200, 'ref-bbb', now()),
                       ($1::bigint, 300, 'ref-ccc', now())
            |]

  -- 2. Insert a named column config preset
  runSession pool
    $ Hasql.Session.sql
      "INSERT INTO smart_csv.column_config (name, config) \
      \VALUES ('e2e_test_preset', '{\"smartCsvE2eTestData_reference\": \"Ref\", \"smartCsvE2eTestData_amount\": \"Amt\", \"smartCsvE2eTestData_createdAt\": \"Date\"}'::jsonb) \
      \ON CONFLICT (name) DO UPDATE SET config = EXCLUDED.config"

  -- 3. POST referencing the named preset
  let restInput =
        Aeson.object
          [ "shardId" Aeson..= testShardId,
            "recipient" Aeson..= ("test@example.com" :: Text),
            "graphqlPaginationKey" Aeson..= ("createdAt" :: Text),
            "graphqlQueryBody" Aeson..= testTableQueryBody,
            "graphqlQueryVariables" Aeson..= mkQueryVariables now oneHour,
            "columnConfigName" Aeson..= ("e2e_test_preset" :: Text)
          ]

  resp <- postRestApiWithAuth pool "/api/v1/csv/generate" restInput
  let status = HTTP.getResponseStatusCode resp
  when (status /= 200)
    $ assertFailure [i|Expected HTTP 200 but got #{status}: #{decodeUtf8Lenient (LBS.toStrict (HTTP.getResponseBody resp))}|]

  reportId <- extractReportId resp

  -- 4. Wait for CSV link
  waitFor 30 $ do
    result <-
      Hasql.Pool.use pool
        $ Hasql.Session.statement
          reportId
          [singletonStatement|
                    select exists(
                        select 1
                        from smart_csv.generated_csv
                        where id = $1::bigint and link is not null
                    )::bool
                |]
    pure $ result == Right True

  -- 5. Download and verify CSV uses preset column names
  csvLink <- fetchCsvLink pool reportId
  rows <- downloadAndParseCsv csvLink

  length rows @?= 3

  let refCol = "Ref"
      amtCol = "Amt"
      refAmounts = Map.fromList [(r, a) | row <- rows, Just r <- [Map.lookup refCol row], Just a <- [Map.lookup amtCol row]]
  let stripDecimal t = fromMaybe t (Text.stripSuffix ".0" t)
      refAmountsNorm = Map.map stripDecimal refAmounts
  Map.lookup "ref-aaa" refAmountsNorm @?= Just "100"
  Map.lookup "ref-bbb" refAmountsNorm @?= Just "200"
  Map.lookup "ref-ccc" refAmountsNorm @?= Just "300"

  -- Verify raw column names are NOT present
  let allHeaders = concatMap Map.keys rows
  assertNotElem "smartCsvE2eTestData_reference" allHeaders
  assertNotElem "smartCsvE2eTestData_amount" allHeaders

  -- 6. Verify completion email was sent with a valid download link
  assertEmailSentWithValidLink "test@example.com" csvLink

  -- 7. Cleanup
  cleanupColumnConfigPreset pool
  cleanup pool

-- ──────────────────────────────────────────────────────────────
-- Helpers
-- ──────────────────────────────────────────────────────────────

mkPool :: IO Hasql.Pool.Pool
mkPool =
  Hasql.Pool.acquire
    $ Hasql.Pool.Config.settings
      [ Hasql.Pool.Config.size 2,
        Hasql.Pool.Config.acquisitionTimeout 10,
        Hasql.Pool.Config.staticConnectionSettings
          [ Hasql.Connection.Setting.connection
              (Hasql.Connection.Setting.Connection.string "postgresql://smart_csv:smart_csv@127.0.0.1:5432/smart_csv"),
            Hasql.Connection.Setting.usePreparedStatements False
          ]
      ]

runSession :: Hasql.Pool.Pool -> Hasql.Session.Session () -> IO ()
runSession pool session = do
  result <- Hasql.Pool.use pool session
  case result of
    Right () -> pure ()
    Left err -> assertFailure (show err)

postRestApi :: Text -> Aeson.Value -> IO (HTTP.Response LBS.ByteString)
postRestApi path body = do
  req <- HTTP.parseRequest (Text.unpack [i|http://127.0.0.1:#{apiPort}#{path}|])
  HTTP.httpLBS
    $ HTTP.setRequestMethod "POST"
    $ HTTP.setRequestBodyJSON body
    $ HTTP.setRequestHeader "Content-Type" ["application/json"] req

postRestApiWithAuth :: Hasql.Pool.Pool -> Text -> Aeson.Value -> IO (HTTP.Response LBS.ByteString)
postRestApiWithAuth pool path body = do
  token <- signTestToken pool
  req <- HTTP.parseRequest (Text.unpack [i|http://127.0.0.1:#{apiPort}#{path}|])
  HTTP.httpLBS
    $ HTTP.setRequestMethod "POST"
    $ HTTP.setRequestBodyJSON body
    $ HTTP.setRequestHeader "Content-Type" ["application/json"]
    $ HTTP.setRequestHeader "Authorization" [encodeUtf8 ("Bearer " <> token)] req

-- | Sign a test JWT using the database's pgjwt extension (same key Hasura uses).
signTestToken :: Hasql.Pool.Pool -> IO Text
signTestToken pool = do
  result <-
    Hasql.Pool.use pool
      $ Hasql.Session.statement
        (Just ("test@example.com" :: Text), Just testClaims, Just ("application" :: Text), Just ("test-tid" :: Text))
        [singletonStatement|
            select
                sign(
                    (json_build_object(
                        'https://hasura.io/jwt/claims', $2::jsonb?,
                        'iat', (select extract(epoch from now())),
                        'exp', (select extract(epoch from now() + interval '1 hour')),
                        'tid', $4::text?,
                        'ttype', $3::text?,
                        'tname', null,
                        'associated_email', $1::text?
                    )::jsonb)::json,
                    current_setting('graphql.jwt_secret')
                )::text
        |]
  case result of
    Right token -> pure token
    Left err -> error (show err)
  where
    testClaims :: Aeson.Value
    testClaims =
      Aeson.object
        [ "x-hasura-default-role" Aeson..= ("smart-csv" :: Text),
          "x-hasura-allowed-roles" Aeson..= (["smart-csv"] :: [Text]),
          "x-hasura-shard-id" Aeson..= ("1" :: Text),
          "x-hasura-user" Aeson..= ("test@example.com" :: Text)
        ]

postHasuraMetadata :: Aeson.Value -> IO (HTTP.Response LBS.ByteString)
postHasuraMetadata body = do
  req <- HTTP.parseRequest "http://127.0.0.1:8080/v1/metadata"
  HTTP.httpLBS
    $ HTTP.setRequestMethod "POST"
    $ HTTP.setRequestBodyJSON body
    $ HTTP.setRequestHeader "Content-Type" ["application/json"]
    $ HTTP.setRequestHeader "x-hasura-admin-secret" ["admin"] req

-- | Poll a condition up to N seconds, failing if it never becomes true.
waitFor :: Int -> IO Bool -> IO ()
waitFor maxSeconds check = go 0
  where
    go n
      | n >= maxSeconds * 10 = assertFailure [i|Timed out after #{maxSeconds}s waiting for condition|]
      | otherwise = do
          result <- check
          if result
            then pure ()
            else threadDelay 100_000 >> go (n + 1)

-- | GraphQL query targeting smart_csv.e2e_test_data.
-- Hasura auto-generates root field smartCsvE2eTestData.
testTableQueryBody :: Text
testTableQueryBody =
  "query Q($rowLimit: Int!, $paginationCondition: SmartCsvE2eTestDataBoolExp!, $conditions: SmartCsvE2eTestDataBoolExp!) {\
  \ smartCsvE2eTestData(limit: $rowLimit, where: {_and: [$paginationCondition, $conditions]}) {\
  \   amount\
  \   createdAt\
  \   reference\
  \ }\
  \}"

mkQueryVariables :: UTCTime -> UTCTime -> Text
mkQueryVariables fromTime toTime =
  let fmt t = Text.pack (formatTime defaultTimeLocale "%FT%TZ" t)
   in [i|{"conditions":{"createdAt":{"_gte":"#{fmt fromTime}","_lte":"#{fmt toTime}"}}}|]

-- | Shared setup: create test table, track in Hasura, grant permissions.
setupTestTable :: Hasql.Pool.Pool -> IO ()
setupTestTable pool = do
  runSession pool $ do
    Hasql.Session.sql "DROP TABLE IF EXISTS smart_csv.e2e_test_data"
    Hasql.Session.sql
      "CREATE TABLE smart_csv.e2e_test_data (\
      \  id bigserial PRIMARY KEY,\
      \  shard_id bigint NOT NULL,\
      \  amount bigint NOT NULL,\
      \  reference text NOT NULL,\
      \  created_at timestamptz NOT NULL DEFAULT now()\
      \)"

  void
    $ postHasuraMetadata
    $ Aeson.object
      [ "type" Aeson..= ("pg_track_table" :: Text),
        "args"
          Aeson..= Aeson.object
            [ "source" Aeson..= ("default" :: Text),
              "table" Aeson..= Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= ("e2e_test_data" :: Text)]
            ]
      ]

  void
    $ postHasuraMetadata
    $ Aeson.object
      [ "type" Aeson..= ("pg_create_select_permission" :: Text),
        "args"
          Aeson..= Aeson.object
            [ "source" Aeson..= ("default" :: Text),
              "table" Aeson..= Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= ("e2e_test_data" :: Text)],
              "role" Aeson..= ("smart-csv" :: Text),
              "permission"
                Aeson..= Aeson.object
                  [ "columns" Aeson..= (["id", "shard_id", "amount", "reference", "created_at"] :: [Text]),
                    "filter" Aeson..= Aeson.object ["shard_id" Aeson..= Aeson.object ["_eq" Aeson..= ("x-hasura-shard-id" :: Text)]]
                  ]
            ]
      ]

-- | Extract reportId from a REST API response.
extractReportId :: HTTP.Response LBS.ByteString -> IO Int64
extractReportId resp = do
  let respBody = HTTP.getResponseBody resp
  case Aeson.decode respBody >>= \(obj :: Aeson.Value) -> obj ^? key "reportId" . _Integer of
    Just rid -> pure (fromInteger rid :: Int64)
    Nothing -> assertFailure [i|No reportId in response: #{decodeUtf8Lenient (LBS.toStrict respBody)}|] >> error "unreachable"

-- | Fetch the CSV download link from the database.
fetchCsvLink :: Hasql.Pool.Pool -> Int64 -> IO Text
fetchCsvLink pool reportId = do
  result <-
    Hasql.Pool.use pool
      $ Hasql.Session.statement
        reportId
        [singletonStatement|
                  select link::text
                  from smart_csv.generated_csv
                  where id = $1::bigint
              |]
  case result of
    Right lnk -> pure lnk
    Left err -> assertFailure (show err) >> error "unreachable"

-- | Download a CSV file and parse it into rows.
downloadAndParseCsv :: Text -> IO [Map.Map Text Text]
downloadAndParseCsv csvLink = do
  csvRequest <- HTTP.parseRequest (Text.unpack csvLink)
  csvBytes <- HTTP.getResponseBody <$> HTTP.httpBS csvRequest
  csvText <- case decodeUtf8' csvBytes of
    Right t -> pure t
    Left err -> assertFailure ("CSV decode error: " <> show err) >> error "unreachable"
  let parsed = CSV.decodeByName (LBS.fromStrict (encodeUtf8 csvText))
  case parsed of
    Right (_, rs) -> pure (Vec.toList rs)
    Left err -> assertFailure [i|CSV parse error: #{err}\n#{csvText}|] >> error "unreachable"

-- | Assert that an element is NOT in a list.
assertNotElem :: (Eq a, Show a) => a -> [a] -> IO ()
assertNotElem x xs =
  when (x `elem` xs)
    $ assertFailure [i|Expected #{show x} to NOT be in list, but it was present|]

-- ──────────────────────────────────────────────────────────────
-- MailHog helpers
-- ──────────────────────────────────────────────────────────────

-- | MailHog API port (web UI / REST API).
mailhogApiPort :: Int
mailhogApiPort = 8025

-- | Delete all messages in MailHog so tests start clean.
deleteAllMailhogMessages :: IO ()
deleteAllMailhogMessages = do
  req <- HTTP.parseRequest [i|http://127.0.0.1:#{mailhogApiPort}/api/v1/messages|]
  void $ HTTP.httpNoBody (HTTP.setRequestMethod "DELETE" req)

-- | Fetch messages from MailHog sent to a specific recipient.
-- Returns a list of MailHog message JSON objects.
fetchMailhogMessages :: Text -> IO [Aeson.Value]
fetchMailhogMessages recipient = do
  req <- HTTP.parseRequest (Text.unpack [i|http://127.0.0.1:#{mailhogApiPort}/api/v2/search?kind=to&query=#{recipient}|])
  resp <- HTTP.httpLBS req
  let body = HTTP.getResponseBody resp
  case Aeson.decode body of
    Just (obj :: Aeson.Value) -> do
      let items = obj ^.. key "items" . values
      pure items
    Nothing -> assertFailure [i|Failed to parse MailHog response: #{decodeUtf8Lenient (LBS.toStrict body)}|] >> error "unreachable"

-- | Assert that MailHog received an email to the given recipient with
-- the expected subject and a valid download link in the HTML body.
assertEmailSentWithValidLink :: Text -> Text -> IO ()
assertEmailSentWithValidLink recipient csvLink = do
  -- Wait for the email to arrive (up to 10 seconds)
  waitFor 10 $ do
    msgs <- fetchMailhogMessages recipient
    pure (not (null msgs))

  msgs <- fetchMailhogMessages recipient
  msg <- case msgs of
    (m : _) -> pure m
    [] -> assertFailure [i|No emails found in MailHog for #{recipient}|] >> error "unreachable"

  -- Verify subject
  let mSubject = msg ^? key "Content" . key "Headers" . key "Subject" . nth 0 . _String
  mSubject @?= Just "Your CSV file is ready for download"

  -- Verify the HTML body contains the download link and expected text.
  -- MailHog returns quoted-printable encoded body (= becomes =3D, line wraps with =\n),
  -- so we decode it before comparing.
  let mBody = msg ^? key "Content" . key "Body" . _String
  case mBody of
    Nothing -> assertFailure "Email body is missing"
    Just body -> do
      let decodedBody = decodeQuotedPrintable body
      assertBool
        [i|Email body should contain a link with href matching the CSV link.\nExpected href containing: #{csvLink}\nBody: #{decodedBody}|]
        (csvLink `Text.isInfixOf` decodedBody)
      assertBool
        [i|Email body should contain 'Download CSV' link text.\nBody: #{decodedBody}|]
        ("Download CSV" `Text.isInfixOf` decodedBody)

-- | Decode quoted-printable encoding (=XX hex codes, =\r\n soft line breaks).
decodeQuotedPrintable :: Text -> Text
decodeQuotedPrintable = Text.replace "=\r\n" "" . Text.replace "=\n" "" . go
  where
    go t = case Text.breakOn "=" t of
      (before, rest)
        | Text.null rest -> before
        | Text.length rest >= 3
        , Text.index rest 1 /= '\n' && Text.index rest 1 /= '\r' ->
            let hex = Text.take 2 (Text.drop 1 rest)
                decoded = case readMaybe ("0x" <> Text.unpack hex) :: Maybe Int of
                  Just c -> Text.singleton (chr c)
                  Nothing -> "=" <> hex
             in before <> decoded <> go (Text.drop 3 rest)
        | otherwise -> before <> "=" <> go (Text.drop 1 rest)


-- | Shared cleanup: untrack table from Hasura and drop it.
cleanup :: Hasql.Pool.Pool -> IO ()
cleanup pool = do
  void
    $ postHasuraMetadata
    $ Aeson.object
      [ "type" Aeson..= ("pg_untrack_table" :: Text),
        "args"
          Aeson..= Aeson.object
            [ "source" Aeson..= ("default" :: Text),
              "table" Aeson..= Aeson.object ["schema" Aeson..= ("smart_csv" :: Text), "name" Aeson..= ("e2e_test_data" :: Text)],
              "cascade" Aeson..= True
            ]
      ]
  void
    $ Hasql.Pool.use pool
    $ Hasql.Session.sql "DROP TABLE IF EXISTS smart_csv.e2e_test_data"

-- | Clean up the e2e_test_preset: null out FK references then delete the row.
cleanupColumnConfigPreset :: Hasql.Pool.Pool -> IO ()
cleanupColumnConfigPreset pool = do
  void
    $ Hasql.Pool.use pool
    $ Hasql.Session.sql "UPDATE smart_csv.smart_graphql_csv_generator SET column_config_name = NULL WHERE column_config_name = 'e2e_test_preset'"
  void
    $ Hasql.Pool.use pool
    $ Hasql.Session.sql "DELETE FROM smart_csv.column_config WHERE name = 'e2e_test_preset'"
