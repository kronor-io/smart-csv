module Test.Kronor.SmartCsv (main) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..), RAW, Selection)
import Data.Vector qualified as Vector
import Kronor.SmartCsv.ColumnConfig (columnConfig, parseColumnConfig)
import Kronor.SmartCsv.ErrorHandling (ErrorAction (..), classifyCursorError, classifyJsonDecodeError, classifyResponseError, classifyTokenClaimsError)
import Kronor.SmartCsv.Flatten (csvify, gatherSelectionNames)
import Kronor.SmartCsv.Notification (CompletionEmail (..), EnqueueMeta (..), defaultEnqueueMeta, mkCompletionEmail)
import Kronor.SmartCsv.Pagination (CursorError (..), extractCursor, inferHeaders)
import Kronor.SmartCsv.Query (GenericQuery (..), ResponseError (..), buildRequestBody, decodeResponseRows, resolvePaginationKey)
import Kronor.SmartCsv.TokenClaims (ParsedTokenClaims (..), TokenClaimsError (..), parseTokenClaims)
import Kronor.SmartCsv.Validation qualified as SmartCsvValidation
import RIO
import RIO.List (headMaybe)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "SmartCsv"
    [ testCase "csvify flattens nested object and applies configured headers" testCsvify,
      testCase "gatherSelectionNames returns prefixed leaf fields" testGatherSelectionNames,
      testCase "extractCursor resolves configured pagination column aliases" testExtractCursorWithAlias,
      testCase "extractCursor falls back to raw key when no mapping exists" testExtractCursorFallback,
      testCase "resolvePaginationKey uses default createdAt" testResolvePaginationKeyDefault,
      testCase "buildRequestBody injects pagination variables" testBuildRequestBody,
      testCase "decodeResponseRows extracts csv rows from graphql data" testDecodeResponseRows,
      testCase "decodeResponseRows returns missing root error when root is absent" testDecodeResponseRowsMissingRoot,
      testCase "parseTokenClaims extracts expected optional fields" testParseTokenClaims,
      testCase "parseTokenClaims rejects non-object claims payload" testParseTokenClaimsInvalid,
      testCase "mkCompletionEmail with url uses download template" testCompletionEmailWithUrl,
      testCase "mkCompletionEmail without url uses no-data template" testCompletionEmailNoData,
      testCase "defaultEnqueueMeta preserves SendEmail constants" testDefaultEnqueueMeta,
      testCase "classifyResponseError with error message returns retryable action" testClassifyResponseErrorRetry,
      testCase "classifyResponseError with missing data returns non-retryable action" testClassifyResponseErrorMissingData,
      testCase "classifyResponseError with missing root returns non-retryable action" testClassifyResponseErrorMissingRoot,
      testCase "classifyCursorError with deleted key returns non-retryable action" testClassifyCursorErrorDeleted,
      testCase "classifyCursorError with missing value returns non-retryable action" testClassifyCursorErrorMissing,
      testCase "classifyTokenClaimsError returns non-retryable action" testClassifyTokenClaimsError,
      testCase "classifyJsonDecodeError returns retryable action" testClassifyJsonDecodeError,
      testCase "validateGraphqlQueryBody accepts valid query" testValidateGraphqlQueryBodyValid,
      testCase "validateGraphqlQueryBody rejects missing paginationCondition" testValidateGraphqlQueryBodyMissingPaginationCondition,
      testCase "validateQueryVariables rejects too-wide range" testValidateQueryVariablesTooWide,
      testCase "validateQueryVariables accepts bounded range" testValidateQueryVariablesValid,
      testCase "parseColumnConfig converts JSON object to column config map" testParseColumnConfig,
      testCase "csvify with empty config uses raw field paths (pass-through)" testCsvifyPassThrough,
      testCase "csvify suppresses fields mapped to null" testCsvifySuppression,
      testCase "inferHeaders with empty config returns raw field paths" testInferHeadersPassThrough,
      testCase "inferHeaders with custom config renames and suppresses" testInferHeadersCustomConfig,
      testCase "extractCursor returns error when pagination key is suppressed" testExtractCursorSuppressed,
      testCase "decodeResponseRows with empty config uses raw field paths" testDecodeResponseRowsPassThrough
    ]

testCsvify :: IO ()
testCsvify = do
  let row =
        Aeson.object
          [ ("waitToken", Aeson.String "wt_123"),
            ("attempts", Aeson.Array (Vector.fromList [Aeson.object [("cardType", Aeson.String "VISA")]])),
            ("customer", Aeson.object [("email", Aeson.String "user@example.com"), ("name", Aeson.String "Ada")])
          ]
      result = csvify columnConfig "paymentRequests" row
  Map.lookup "Payment Request ID" result @?= Just "wt_123"
  Map.lookup "Card Type" result @?= Just "VISA"
  Map.lookup "Customer Email" result @?= Just "user@example.com"
  Map.lookup "Customer Name" result @?= Just "Ada"

testGatherSelectionNames :: IO ()
testGatherSelectionNames = do
  root <- parseRootSelection gqlQueryText
  gatherSelectionNames root
    @?= [ "paymentRequests_waitToken",
          "paymentRequests_customer_email",
          "paymentRequests_attempts_cardType"
        ]

testExtractCursorWithAlias :: IO ()
testExtractCursorWithAlias = do
  let row = Map.fromList [("Placed At", "2026-03-16T13:10:02Z")]
  extractCursor columnConfig "paymentRequests" "createdAt" row @?= Right "2026-03-16T13:10:02Z"

testExtractCursorFallback :: IO ()
testExtractCursorFallback = do
  let row = Map.fromList [("paymentRequests_internalCursor", "cursor_001")]
  extractCursor mempty "paymentRequests" "internalCursor" row @?= Right "cursor_001"

testResolvePaginationKeyDefault :: IO ()
testResolvePaginationKeyDefault = do
  let gq = GenericQuery {paginationKey = Nothing, query = "query {}", variables = Aeson.object []}
  resolvePaginationKey gq @?= "createdAt"

testBuildRequestBody :: IO ()
testBuildRequestBody = do
  let gq =
        GenericQuery
          { paginationKey = Just "createdAt",
            query = "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { paymentRequests { waitToken } }",
            variables = Aeson.object [("existingVar", Aeson.String "kept")]
          }
      payload = buildRequestBody "createdAt" 100 (Just "cursor_123") gq
  case Aeson.eitherDecode payload of
    Left err -> assertFailure err
    Right (val :: Aeson.Value) -> do
      val
        @?= Aeson.object
          [ ("paginationKey", Aeson.String "createdAt"),
            ("query", Aeson.String gq.query),
            ( "variables",
              Aeson.object
                [ ("existingVar", Aeson.String "kept"),
                  ("rowLimit", Aeson.Number 100),
                  ("paginationCondition", Aeson.object [("createdAt", Aeson.object [("_lt", Aeson.String "cursor_123")])])
                ]
            )
          ]

testDecodeResponseRows :: IO ()
testDecodeResponseRows = do
  let response =
        Aeson.object
          [ ( "data",
              Aeson.object
                [ ( "paymentRequests",
                    Aeson.Array
                      ( Vector.fromList
                          [ Aeson.object
                              [ ("waitToken", Aeson.String "wt_777"),
                                ("createdAt", Aeson.String "2026-03-16T14:22:00Z")
                              ]
                          ]
                      )
                  )
                ]
            )
          ]
      emptyRow = Map.fromList [("Payment Request ID", mempty), ("Placed At", mempty)]
  decodeResponseRows columnConfig "paymentRequests" emptyRow response
    @?= Right (Vector.fromList [Map.fromList [("Payment Request ID", "wt_777"), ("Placed At", "2026-03-16T14:22:00Z")]])

testDecodeResponseRowsMissingRoot :: IO ()
testDecodeResponseRowsMissingRoot = do
  let response = Aeson.object [("data", Aeson.object [("otherRoot", Aeson.Array Vector.empty)])]
  decodeResponseRows mempty "paymentRequests" mempty response @?= Left ResponseMissingRootData

testParseTokenClaims :: IO ()
testParseTokenClaims = do
  let claims =
        Aeson.object
          [ ("associated_email", Aeson.String "ops@kronor.io"),
            ("https://hasura.io/jwt/claims", Aeson.object [("x-hasura-role", Aeson.String "merchant")]),
            ("ttype", Aeson.String "portal"),
            ("tid", Aeson.String "token-123")
          ]
  parseTokenClaims claims
    @?= Right
      ParsedTokenClaims
        { associatedEmail = Just "ops@kronor.io",
          hasuraClaims = Just (Aeson.object [("x-hasura-role", Aeson.String "merchant")]),
          tokenType = Just "portal",
          tokenId = Just "token-123"
        }

testParseTokenClaimsInvalid :: IO ()
testParseTokenClaimsInvalid =
  parseTokenClaims Aeson.Null @?= Left TokenClaimsNotObject

testCompletionEmailWithUrl :: IO ()
testCompletionEmailWithUrl =
  mkCompletionEmail (Just "https://signed.example.com/report.csv")
    @?= CompletionEmail
      { subject = "Your CSV file is ready for download",
        htmlBody =
          "<html>\n"
            <> "  <body>\n"
            <> "    <p>Your requested CSV file is ready for download:</p>\n"
            <> "    <a href=\"https://signed.example.com/report.csv\">Download CSV</a>\n"
            <> "  </body>\n"
            <> "</html>\n"
      }

testCompletionEmailNoData :: IO ()
testCompletionEmailNoData =
  mkCompletionEmail Nothing
    @?= CompletionEmail
      { subject = "Your CSV file contained no data",
        htmlBody =
          "<html>\n"
            <> "  <body>\n"
            <> "    <p>Your requested CSV file was not produced because it contained no data.</p>\n"
            <> "  </body>\n"
            <> "</html>\n"
      }

testDefaultEnqueueMeta :: IO ()
testDefaultEnqueueMeta =
  defaultEnqueueMeta
    @?= EnqueueMeta
      { tag = "SendEmail",
        caller = "worker_sendCsvDoneEmail",
        requestId = "portal_sendEmailNoReply",
        priority = 5000
      }

parseRootSelection :: Text -> IO (Selection RAW)
parseRootSelection q =
  case parseRequest (GQLRequest {query = q, operationName = Nothing, variables = Nothing}) of
    Failure errs -> assertFailure ("query parse failed: " <> show errs) >> error "unreachable"
    Success executableDocument _warnings ->
      case headMaybe (toList executableDocument.operation.operationSelection) of
        Nothing -> assertFailure "empty selection set" >> error "unreachable"
        Just root -> pure root

gqlQueryText :: Text
gqlQueryText =
  "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { \
  \  paymentRequests(limit: $rowLimit, where: $paginationCondition) { \
  \    waitToken \
  \    customer { email } \
  \    attempts { cardType } \
  \  } \
  \}"

testClassifyResponseErrorRetry :: IO ()
testClassifyResponseErrorRetry =
  classifyResponseError (ResponseContainsError "Field not found: customer")
    @?= Retry "Field not found: customer"

testClassifyResponseErrorMissingData :: IO ()
testClassifyResponseErrorMissingData =
  classifyResponseError ResponseMissingData
    @?= Giveup "GraphQL response does not contain data."

testClassifyResponseErrorMissingRoot :: IO ()
testClassifyResponseErrorMissingRoot =
  classifyResponseError ResponseMissingRootData
    @?= Giveup "GraphQL response does not contain expected root query data."

testClassifyCursorErrorDeleted :: IO ()
testClassifyCursorErrorDeleted =
  classifyCursorError (CursorKeyDeleted "paymentRequests_createdAt")
    @?= Giveup "Cursor key is marked to be deleted: paymentRequests_createdAt"

testClassifyCursorErrorMissing :: IO ()
testClassifyCursorErrorMissing =
  classifyCursorError (CursorValueMissing "Placed At")
    @?= Giveup "If you see this that means you need to look in columnConfig for missing pagination key column mapping (Placed At)"

testClassifyTokenClaimsError :: IO ()
testClassifyTokenClaimsError =
  classifyTokenClaimsError TokenClaimsNotObject
    @?= Giveup "Token claims are not in a valid json format."

testClassifyJsonDecodeError :: IO ()
testClassifyJsonDecodeError =
  classifyJsonDecodeError "trailing junk"
    @?= Retry "trailing junk"

testValidateGraphqlQueryBodyValid :: IO ()
testValidateGraphqlQueryBodyValid =
  SmartCsvValidation.validateGraphqlQueryBody gqlQueryText @?= Right ()

testValidateGraphqlQueryBodyMissingPaginationCondition :: IO ()
testValidateGraphqlQueryBodyMissingPaginationCondition =
  SmartCsvValidation.validateGraphqlQueryBody "query ($rowLimit: Int!) { paymentRequests(limit: $rowLimit) { waitToken } }"
    @?= Left (pure "The query must define a paginationCondition variable.")

testValidateQueryVariablesTooWide :: IO ()
testValidateQueryVariablesTooWide =
  SmartCsvValidation.validateQueryVariables "createdAt" "{\"conditions\":{\"createdAt\":{\"_gte\":\"2026-01-01T00:00:00Z\",\"_lt\":\"2026-03-01T00:00:00Z\"}}}"
    @?= Left "The createdAt range is too wide."

testValidateQueryVariablesValid :: IO ()
testValidateQueryVariablesValid =
  case SmartCsvValidation.validateQueryVariables "createdAt" "{\"conditions\":{\"createdAt\":{\"_gte\":\"2026-03-01T00:00:00Z\",\"_lt\":\"2026-03-15T00:00:00Z\"}}}" of
    Left err -> assertFailure (show err)
    Right _ -> pure ()

testParseColumnConfig :: IO ()
testParseColumnConfig = do
  let json =
        Aeson.object
          [ ("field_a", Aeson.String "Column A"),
            ("field_b", Aeson.Null)
          ]
  parseColumnConfig json
    @?= Map.fromList
      [ ("field_a", Just "Column A"),
        ("field_b", Nothing)
      ]
  parseColumnConfig Aeson.Null @?= Map.empty

testCsvifyPassThrough :: IO ()
testCsvifyPassThrough = do
  let row =
        Aeson.object
          [ ("waitToken", Aeson.String "wt_123"),
            ("customer", Aeson.object [("email", Aeson.String "user@example.com")])
          ]
      result = csvify mempty "orders" row
  -- With empty config, raw prefixed paths are used as keys
  Map.lookup "orders_waitToken" result @?= Just "wt_123"
  Map.lookup "orders_customer_email" result @?= Just "user@example.com"

testCsvifySuppression :: IO ()
testCsvifySuppression = do
  let config =
        Map.fromList
          [ ("orders_waitToken", Just "Order ID"),
            ("orders_customer_email", Nothing) -- suppress this field
          ]
      row =
        Aeson.object
          [ ("waitToken", Aeson.String "wt_123"),
            ("customer", Aeson.object [("email", Aeson.String "user@example.com")])
          ]
      result = csvify config "orders" row
  Map.lookup "Order ID" result @?= Just "wt_123"
  -- Suppressed field should not appear at all
  Map.member "orders_customer_email" result @?= False
  Map.member "Customer Email" result @?= False

testInferHeadersPassThrough :: IO ()
testInferHeadersPassThrough = do
  root <- parseRootSelection gqlQueryText
  inferHeaders mempty root
    @?= [ "paymentRequests_waitToken",
          "paymentRequests_customer_email",
          "paymentRequests_attempts_cardType"
        ]

testInferHeadersCustomConfig :: IO ()
testInferHeadersCustomConfig = do
  let config =
        Map.fromList
          [ ("paymentRequests_waitToken", Just "Order ID"),
            ("paymentRequests_customer_email", Nothing) -- suppress
            -- paymentRequests_attempts_cardType not in config -> pass-through
          ]
  root <- parseRootSelection gqlQueryText
  inferHeaders config root
    @?= [ "Order ID",
          -- customer_email is suppressed, so absent from list
          "paymentRequests_attempts_cardType"
        ]

testExtractCursorSuppressed :: IO ()
testExtractCursorSuppressed = do
  let config = Map.fromList [("paymentRequests_createdAt", Nothing)]
      row = Map.fromList [("Placed At", "2026-03-16T13:10:02Z")]
  extractCursor config "paymentRequests" "createdAt" row
    @?= Left (CursorKeyDeleted "paymentRequests_createdAt")

testDecodeResponseRowsPassThrough :: IO ()
testDecodeResponseRowsPassThrough = do
  let response =
        Aeson.object
          [ ( "data",
              Aeson.object
                [ ( "orders",
                    Aeson.Array
                      ( Vector.fromList
                          [ Aeson.object
                              [ ("reference", Aeson.String "ref_001"),
                                ("amount", Aeson.Number 1500)
                              ]
                          ]
                      )
                  )
                ]
            )
          ]
      emptyRow = Map.fromList [("orders_reference", mempty), ("orders_amount", mempty)]
  -- With empty config, raw field paths appear in output
  decodeResponseRows mempty "orders" emptyRow response
    @?= Right (Vector.fromList [Map.fromList [("orders_reference", "ref_001"), ("orders_amount", "1500.0")]])
