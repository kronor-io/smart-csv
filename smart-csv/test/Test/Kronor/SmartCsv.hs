module Test.Kronor.SmartCsv (main) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..), RAW, Selection)
import Data.Vector qualified as Vector
import Kronor.SmartCsv.ColumnConfig (ColumnConfig, ColumnSettings (..), parseColumnConfig)
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
    [ testCase "csvify extracts nested values via configured data paths" testCsvify,
      testCase "gatherSelectionNames returns direct selected fields" testGatherSelectionNames,
      testCase "extractCursor resolves configured pagination column aliases" testExtractCursorWithAlias,
      testCase "extractCursor falls back to field name when no alias exists" testExtractCursorFallback,
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
      testCase "classifyCursorError with missing selection returns non-retryable action" testClassifyCursorErrorMissingColumn,
      testCase "classifyCursorError with missing value returns non-retryable action" testClassifyCursorErrorMissing,
      testCase "classifyTokenClaimsError returns non-retryable action" testClassifyTokenClaimsError,
      testCase "classifyJsonDecodeError returns retryable action" testClassifyJsonDecodeError,
      testCase "validateGraphqlQueryBody accepts valid query" testValidateGraphqlQueryBodyValid,
      testCase "validateGraphqlQueryBody rejects missing paginationCondition" testValidateGraphqlQueryBodyMissingPaginationCondition,
      testCase "validateQueryVariables rejects too-wide range" testValidateQueryVariablesTooWide,
      testCase "validateQueryVariables accepts bounded range" testValidateQueryVariablesValid,
      testCase "parseColumnConfig converts JSON object to column config map" testParseColumnConfig,
      testCase "csvify with empty config auto-extracts scalar from single-key objects" testCsvifyPassThrough,
      testCase "csvify with empty config leaves multi-key objects blank" testCsvifyMultiKeyObject,
      testCase "csvify serializes arrays as comma-separated values" testCsvifyArrayValues,
      testCase "csvify ignores null values inside arrays" testCsvifyArrayIgnoresNulls,
      testCase "csvify ignores arrays of objects with only null selected fields" testCsvifyArrayObjectNullFields,
      testCase "csvify applies custom decimal places per column" testCsvifyCustomDecimalPlaces,
      testCase "csvify keeps numeric values as-is when decimalPlaces is unset" testCsvifyNumericPreservesOriginal,
      testCase "inferHeaders with empty config returns alias ids" testInferHeadersPassThrough,
      testCase "inferHeaders with custom config renames aliased columns" testInferHeadersCustomConfig,
      testCase "extractCursor returns error when pagination field is not selected" testExtractCursorMissingSelection,
      testCase "extractCursor ignores nested fields named like the pagination key" testExtractCursorIgnoresNestedField,
      testCase "decodeResponseRows with empty config uses alias ids" testDecodeResponseRowsPassThrough
    ]

testCsvify :: IO ()
testCsvify = do
  let row =
        Aeson.object
          [ ("payment_request_id", Aeson.String "wt_123"),
            ( "attempts",
              Aeson.Array
                ( Vector.fromList
                    [ Aeson.object
                        [("payment", Aeson.object [("cardType", Aeson.String "VISA")])]
                    ]
                )
            ),
            ( "customer",
              Aeson.object
                [ ("profile", Aeson.object [("email", Aeson.String "user@example.com"), ("name", Aeson.String "Ada")])
                ]
            )
          ]
      result = csvify columnConfig "paymentRequests" row
  Map.lookup "Payment Request ID" result @?= Just "wt_123"
  Map.lookup "Card Type" result @?= Just "VISA"
  Map.lookup "Customer Email" result @?= Just "user@example.com"

testGatherSelectionNames :: IO ()
testGatherSelectionNames = do
  root <- parseRootSelection gqlQueryText
  gatherSelectionNames root
    @?= [ "payment_request_id",
          "placed_at",
          "customer",
          "attempts"
        ]

testExtractCursorWithAlias :: IO ()
testExtractCursorWithAlias = do
  root <- parseRootSelection gqlQueryText
  let row = Map.fromList [("Placed At", "2026-03-16T13:10:02Z")]
  extractCursor columnConfig root "createdAt" row @?= Right "2026-03-16T13:10:02Z"

testExtractCursorFallback :: IO ()
testExtractCursorFallback = do
  root <- parseRootSelection fallbackCursorQueryText
  let row = Map.fromList [("internalCursor", "cursor_001")]
  extractCursor mempty root "internalCursor" row @?= Right "cursor_001"

testResolvePaginationKeyDefault :: IO ()
testResolvePaginationKeyDefault = do
  let gq = GenericQuery {paginationKey = Nothing, query = "query {}", variables = Aeson.object []}
  resolvePaginationKey gq @?= "createdAt"

testBuildRequestBody :: IO ()
testBuildRequestBody = do
  let gq =
        GenericQuery
          { paginationKey = Just "createdAt",
            query = "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { paymentRequests { payment_request_id: waitToken } }",
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
                              [ ("payment_request_id", Aeson.String "wt_777"),
                                ("placed_at", Aeson.String "2026-03-16T14:22:00Z")
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

-- | Sample column config used across several tests.
columnConfig :: ColumnConfig
columnConfig =
  Map.fromList
    [ ("payment_request_id", ColumnSettings Nothing (Just "Payment Request ID") Nothing),
      ("transaction_id", ColumnSettings Nothing (Just "Transaction ID") Nothing),
      ("merchant_id", ColumnSettings Nothing (Just "Merchant ID") Nothing),
      ("placed_at", ColumnSettings Nothing (Just "Placed At") Nothing),
      ("reference", ColumnSettings Nothing (Just "Reference") Nothing),
      ("payment_method", ColumnSettings Nothing (Just "Payment Method") Nothing),
      ("attempts", ColumnSettings Nothing (Just "Card Type") (Just "payment.cardType")),
      ("customer", ColumnSettings Nothing (Just "Customer Email") (Just "profile.email")),
      ("latest_status", ColumnSettings Nothing (Just "Latest Status") Nothing),
      ("currency", ColumnSettings Nothing (Just "Currency") Nothing),
      ("amount", ColumnSettings Nothing (Just "Amount") Nothing)
    ]

gqlQueryText :: Text
gqlQueryText =
  "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { \
  \  paymentRequests(limit: $rowLimit, where: $paginationCondition) { \
  \    payment_request_id: waitToken \
  \    placed_at: createdAt \
  \    customer { profile { email name } } \
  \    attempts { payment { cardType } } \
  \  } \
  \}"

fallbackCursorQueryText :: Text
fallbackCursorQueryText =
  "query { orders { internalCursor } }"

missingCursorQueryText :: Text
missingCursorQueryText =
  "query { paymentRequests { payment_request_id: waitToken } }"

nestedCursorQueryText :: Text
nestedCursorQueryText =
  "query { paymentRequests { payment_request_id: waitToken customer { createdAt } } }"

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

testClassifyCursorErrorMissingColumn :: IO ()
testClassifyCursorErrorMissingColumn =
  classifyCursorError (CursorColumnMissing "createdAt")
    @?= Giveup "GraphQL query does not select pagination field: createdAt"

testClassifyCursorErrorMissing :: IO ()
testClassifyCursorErrorMissing =
  classifyCursorError (CursorValueMissing "Placed At")
    @?= Giveup "GraphQL response is missing the pagination cursor column: Placed At"

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
  SmartCsvValidation.validateGraphqlQueryBody "query ($rowLimit: Int!) { paymentRequests(limit: $rowLimit) { payment_request_id: waitToken } }"
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
          [ ( "field_a",
              Aeson.object
                [ ("header", Aeson.String "Column A"),
                  ("decimalPlaces", Aeson.toJSON (3 :: Int)),
                  ("dataPath", Aeson.String "customer.email")
                ]
            ),
            ("field_b", Aeson.object [])
          ]
  parseColumnConfig json
    @?= Map.fromList
      [ ("field_a", ColumnSettings (Just 3) (Just "Column A") (Just "customer.email")),
        ("field_b", ColumnSettings Nothing Nothing Nothing)
      ]
  parseColumnConfig Aeson.Null @?= Map.empty

testCsvifyPassThrough :: IO ()
testCsvifyPassThrough = do
  let row =
        Aeson.object
          [ ("payment_request_id", Aeson.String "wt_123"),
            ("customer", Aeson.object [("profile", Aeson.object [("email", Aeson.String "user@example.com")])]),
            ("tags", Aeson.Array (Vector.fromList [Aeson.object [("label", Aeson.String "vip")]]))
          ]
      result = csvify mempty "orders" row
  Map.lookup "payment_request_id" result @?= Just "wt_123"
  Map.lookup "customer" result @?= Just "user@example.com"
  Map.lookup "tags" result @?= Just "vip"

testCsvifyMultiKeyObject :: IO ()
testCsvifyMultiKeyObject = do
  let row =
        Aeson.object
          [ ("customer", Aeson.object [("email", Aeson.String "user@example.com"), ("name", Aeson.String "Ada")]) ]
      result = csvify mempty "orders" row
  Map.lookup "customer" result @?= Nothing

testCsvifyArrayValues :: IO ()
testCsvifyArrayValues = do
  let config =
        Map.fromList
          [ ("amounts", ColumnSettings (Just 1) (Just "Amounts") Nothing),
            ("attempts", ColumnSettings Nothing (Just "Card Types") (Just "payment.cardType"))
          ]
      row =
        Aeson.object
          [ ("amounts", Aeson.Array (Vector.fromList [Aeson.Number 12.34, Aeson.Number 56.78])),
            ( "attempts",
              Aeson.Array
                ( Vector.fromList
                    [ Aeson.object [("payment", Aeson.object [("cardType", Aeson.String "VISA")])],
                      Aeson.object [("payment", Aeson.object [("cardType", Aeson.String "MASTERCARD")])]
                    ]
                )
            )
          ]
      result = csvify config "orders" row
  Map.lookup "Amounts" result @?= Just "12.3,56.7"
  Map.lookup "Card Types" result @?= Just "VISA,MASTERCARD"

testCsvifyArrayIgnoresNulls :: IO ()
testCsvifyArrayIgnoresNulls = do
  let config =
        Map.fromList
          [ ("amounts", ColumnSettings (Just 1) (Just "Amounts") Nothing),
            ("attempts", ColumnSettings Nothing (Just "Card Types") (Just "payment.cardType"))
          ]
      row =
        Aeson.object
          [ ("amounts", Aeson.Array (Vector.fromList [Aeson.Null, Aeson.Number 12.34, Aeson.Null, Aeson.Number 56.78])),
            ( "attempts",
              Aeson.Array
                ( Vector.fromList
                    [ Aeson.Null,
                      Aeson.object [("payment", Aeson.object [("cardType", Aeson.String "VISA")])],
                      Aeson.Null,
                      Aeson.object [("payment", Aeson.object [("cardType", Aeson.String "MASTERCARD")])]
                    ]
                )
            )
          ]
      result = csvify config "orders" row
  Map.lookup "Amounts" result @?= Just "12.3,56.7"
  Map.lookup "Card Types" result @?= Just "VISA,MASTERCARD"

testCsvifyArrayObjectNullFields :: IO ()
testCsvifyArrayObjectNullFields = do
  let config =
        Map.fromList
          [("attempts", ColumnSettings Nothing (Just "Card Types") (Just "payment.cardType"))]
      row =
        Aeson.object
          [ ( "attempts",
              Aeson.Array
                ( Vector.fromList
                    [ Aeson.object [("payment", Aeson.object [("cardType", Aeson.Null)])],
                      Aeson.object [("payment", Aeson.object [("cardType", Aeson.Null)])],
                      Aeson.object [("payment", Aeson.object [("cardType", Aeson.Null)])]
                    ]
                )
            )
          ]
      result = csvify config "orders" row
  Map.lookup "Card Types" result @?= Just ""

testCsvifyCustomDecimalPlaces :: IO ()
testCsvifyCustomDecimalPlaces = do
  let config =
        Map.fromList
          [ ("amount", ColumnSettings (Just 3) (Just "Amount") Nothing),
            ("exchange_rate", ColumnSettings (Just 4) (Just "Rate") Nothing)
          ]
      row =
        Aeson.object
          [ ("amount", Aeson.Number 12.3456),
            ("exchange_rate", Aeson.String "1.23456")
          ]
      result = csvify config "orders" row
  Map.lookup "Amount" result @?= Just "12.345"
  Map.lookup "Rate" result @?= Just "1.2345"

testCsvifyNumericPreservesOriginal :: IO ()
testCsvifyNumericPreservesOriginal = do
  let config =
        Map.fromList
          [ ("price", ColumnSettings Nothing (Just "Price") Nothing),
            ("quantity", ColumnSettings Nothing (Just "Quantity") Nothing),
            ("discount", ColumnSettings Nothing (Just "Discount") Nothing)
          ]
      row =
        Aeson.object
          [ ("price", Aeson.Number 123.456),
            ("quantity", Aeson.String "50.10"),
            ("discount", Aeson.Number 10)
          ]
      result = csvify config "orders" row
  Map.lookup "Price" result @?= Just "123.456"
  Map.lookup "Quantity" result @?= Just "50.10"
  Map.lookup "Discount" result @?= Just "10"

testInferHeadersPassThrough :: IO ()
testInferHeadersPassThrough = do
  root <- parseRootSelection gqlQueryText
  inferHeaders mempty root
    @?= [ "payment_request_id",
          "placed_at",
          "customer",
          "attempts"
        ]

testInferHeadersCustomConfig :: IO ()
testInferHeadersCustomConfig = do
  let config =
        Map.fromList
          [ ("payment_request_id", ColumnSettings Nothing (Just "Order ID") Nothing),
            ("placed_at", ColumnSettings Nothing (Just "Placed At") Nothing),
            ("customer", ColumnSettings Nothing (Just "Customer Email") (Just "profile.email")),
            ("attempts", ColumnSettings Nothing (Just "Card Type") (Just "payment.cardType"))
          ]
  root <- parseRootSelection gqlQueryText
  inferHeaders config root
    @?= [ "Order ID",
          "Placed At",
          "Customer Email",
          "Card Type"
        ]

testExtractCursorMissingSelection :: IO ()
testExtractCursorMissingSelection = do
  root <- parseRootSelection missingCursorQueryText
  let row = Map.fromList [("Placed At", "2026-03-16T13:10:02Z")]
  extractCursor columnConfig root "createdAt" row
    @?= Left (CursorColumnMissing "createdAt")

testExtractCursorIgnoresNestedField :: IO ()
testExtractCursorIgnoresNestedField = do
  root <- parseRootSelection nestedCursorQueryText
  let row = Map.fromList [("createdAt", "2026-03-16T13:10:02Z")]
  extractCursor mempty root "createdAt" row
    @?= Left (CursorColumnMissing "createdAt")

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
                              [ ("reference_col", Aeson.String "ref_001"),
                                ("amount_col", Aeson.Number 1500)
                              ]
                          ]
                      )
                  )
                ]
            )
          ]
      emptyRow = Map.fromList [("reference_col", mempty), ("amount_col", mempty)]
  decodeResponseRows mempty "orders" emptyRow response
    @?= Right (Vector.fromList [Map.fromList [("reference_col", "ref_001"), ("amount_col", "1500")]])
