module Test.SmartCsv (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Morpheus.Core (parseRequest)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Types.IO (GQLRequest (..))
import Data.Morpheus.Types.Internal.AST (ExecutableDocument (..), Operation (..), RAW, Selection)
import Data.Vector qualified as Vector
import SmartCsv.ColumnConfig (ColumnConfig, ColumnKey (..), ColumnSettings (..), defaultColumnSettings, parseColumnConfig)
-- OutputColumn's fields, for .header and .key: unqualified they would make record
-- updates of ColumnSettings ambiguous.
import RIO
import RIO.ByteString.Lazy qualified as LByteString
import RIO.List (headMaybe)
import RIO.Set qualified as Set
import SmartCsv.ColumnConfig qualified as ColumnConfig
import SmartCsv.ErrorHandling (ErrorAction (..), classifyCursorError, classifyHttpStatusError, classifyJsonDecodeError, classifyResponseError, classifyTokenClaimsError)
import SmartCsv.Flatten (csvify, gatherSelectionNames)
import SmartCsv.Notification (CompletionEmail (..), EnqueueMeta (..), defaultEnqueueMeta, mkCompletionEmail)
import SmartCsv.Pagination (CursorError (..), PaginationCursor (..), PaginationDirection (..), PaginationField (..), extractCursorValue, inferColumns, inferHeaders)
import SmartCsv.Query (DecodedResponsePage (..), GenericQuery (..), ResponseError (..), buildRequestBody, csvRowBytes, decodeResponseChunk, decodeResponsePage, decodeResponseRows, decodedResponsePageBytes, resolvePaginationFields, resolvePaginationKey)
import SmartCsv.Schema (SchemaTypes (..), introspectionRequestBody, markNumericColumns, numericColumns, parseIntrospectionResponse)
import SmartCsv.TokenClaims (HasuraRoleError (..), ParsedTokenClaims (..), TokenClaimsError (..), parseTokenClaims, selectHasuraRole)
import SmartCsv.Validation qualified as SmartCsvValidation
import Test.SmartCsv.Xlsx (byHeader)
import Test.SmartCsv.Xlsx qualified
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
      testCase "extractCursorValue reads an aliased pagination field" testExtractCursorWithAlias,
      testCase "extractCursorValue captures composite order cursor values" testExtractCursorCompositeOrder,
      testCase "extractCursorValue captures nested cursor values from raw rows" testExtractCursorValueNested,
      testCase "extractCursorValue renders an integral number without a decimal point" testExtractCursorValueIntegral,
      testCase "extractCursorValue keeps the fraction of a non-integral number" testExtractCursorValueFractional,
      testCase "extractCursorValue falls back to field name when no alias exists" testExtractCursorFallback,
      testCase "resolvePaginationKey uses default createdAt" testResolvePaginationKeyDefault,
      testCase "resolvePaginationFields uses orderBy when it matches pagination key" testResolvePaginationFieldsUsesOrderBy,
      testCase "resolvePaginationFields uses explicit nested orderBy" testResolvePaginationFieldsNestedOrderBy,
      testCase "buildRequestBody injects pagination variables" testBuildRequestBody,
      testCase "buildRequestBody injects lexicographic pagination conditions" testBuildRequestBodyComposite,
      testCase "buildRequestBody injects explicit orderBy" testBuildRequestBodyExplicitOrderBy,
      testCase "buildRequestBody injects nested pagination conditions" testBuildRequestBodyNested,
      testCase "decodeResponseRows extracts csv rows from graphql data" testDecodeResponseRows,
      testCase "decodeResponseChunk stops after reaching chunk target" testDecodeResponseChunk,
      testCase "decodeResponsePage streams rows into csv bytes" testDecodeResponsePage,
      testCase "decodeResponsePage returns top-level error" testDecodeResponsePageError,
      testCase "decodeResponseRows returns missing root error when root is absent" testDecodeResponseRowsMissingRoot,
      testCase "parseTokenClaims extracts expected optional fields" testParseTokenClaims,
      testCase "parseTokenClaims rejects non-object claims payload" testParseTokenClaimsInvalid,
      testCase "selectHasuraRole narrows the claims to an allowed role" testSelectHasuraRole,
      testCase "selectHasuraRole matches role claim names ignoring case" testSelectHasuraRoleClaimCase,
      testCase "selectHasuraRole rejects a role the token is not allowed" testSelectHasuraRoleNotAllowed,
      testCase "selectHasuraRole rejects claims without Hasura claims" testSelectHasuraRoleMissingClaims,
      testCase "selectHasuraRole rejects Hasura claims that are not an object" testSelectHasuraRoleClaimsNotObject,
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
      testCase "classifyHttpStatusError with server error returns retryable action" testClassifyHttpStatusErrorServerError,
      testCase "classifyHttpStatusError with throttling returns retryable action" testClassifyHttpStatusErrorThrottled,
      testCase "classifyHttpStatusError with client error returns non-retryable action" testClassifyHttpStatusErrorClientError,
      testCase "validateGraphqlQueryBody accepts valid query" testValidateGraphqlQueryBodyValid,
      testCase "validateGraphqlQueryBody rejects missing paginationCondition" testValidateGraphqlQueryBodyMissingPaginationCondition,
      testCase "validateGraphqlQueryBodyAndGetRootField returns root field name" testValidateGraphqlQueryBodyAndGetRootFieldValid,
      testCase "validateGraphqlQueryBodyAndGetRootField ignores root alias" testValidateGraphqlQueryBodyAndGetRootFieldAlias,
      testCase "validateGraphqlQueryBodyAndGetRootField rejects non-field root selection" testValidateGraphqlQueryBodyAndGetRootFieldRejectsNonFieldRoot,
      testCase "validateQueryVariables rejects invalid JSON" testValidateQueryVariablesInvalidJson,
      testCase "validateQueryVariables rejects non-object JSON" testValidateQueryVariablesNonObject,
      testCase "validateQueryVariables accepts valid JSON" testValidateQueryVariablesValid,
      testCase "parseColumnConfig converts JSON object to column config map" testParseColumnConfig,
      testCase "csvify with empty config auto-extracts scalar from single-key objects" testCsvifyPassThrough,
      testCase "csvify with empty config leaves multi-key objects blank" testCsvifyMultiKeyObject,
      testCase "csvify serializes arrays as comma-separated values" testCsvifyArrayValues,
      testCase "csvify ignores null values inside arrays" testCsvifyArrayIgnoresNulls,
      testCase "csvify ignores arrays of objects with only null selected fields" testCsvifyArrayObjectNullFields,
      testCase "csvify applies custom decimal places with comma decimals" testCsvifyCustomDecimalPlaces,
      testCase "csvify uses comma decimals when decimalPlaces is unset" testCsvifyNumericPreservesOriginal,
      testCase "csvify uses comma decimals for a numeric column sent as a string" testCsvifyNumericStringColumn,
      testCase "parseColumnConfig reads the entries of columns and ignores the field's own settings" testParseColumnConfigColumns,
      testCase "parseColumnConfig leaves out an entry whose columns is empty" testParseColumnConfigEmptyColumns,
      testCase "csvify prints a field with columns as one column per entry" testCsvifyColumns,
      testCase "csvify prints each entry of columns like a column of its own" testCsvifyColumnsLikeOwnColumns,
      testCase "csvify applies columns to every element of an array" testCsvifyColumnsArray,
      testCase "csvRowBytes keeps each column's value when two share a header" testCsvRowBytesSharedHeader,
      testCase "introspectionRequestBody is a valid GraphQL query" testIntrospectionRequestBodyParses,
      testCase "parseIntrospectionResponse keeps the named type of each field" testParseIntrospectionResponse,
      testCase "numericColumns finds the columns that print a numeric scalar" testNumericColumns,
      testCase "markNumericColumns flags configured and unconfigured columns" testMarkNumericColumns,
      testCase "numericColumns checks each entry of columns on its own" testNumericColumnsEntries,
      testCase "markNumericColumns flags one entry of columns" testMarkNumericColumnsEntry,
      testCase "markNumericColumns flags several entries of one field" testMarkNumericColumnsEntries,
      testCase "inferHeaders with empty config returns alias ids" testInferHeadersPassThrough,
      testCase "inferHeaders with custom config renames aliased columns" testInferHeadersCustomConfig,
      testCase "inferHeaders puts the entries of columns in the field's place" testInferHeadersColumns,
      testCase "extractCursorValue returns error when pagination field is not selected" testExtractCursorMissingSelection,
      testCase "extractCursorValue reads the cursor whatever columns the file prints" testExtractCursorIgnoresColumns,
      testCase "decodeResponseRows with empty config uses alias ids" testDecodeResponseRowsPassThrough,
      Test.SmartCsv.Xlsx.tests
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
      result = csvifyByHeader columnConfig "paymentRequests" row
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
  let row = Aeson.object [("placed_at", Aeson.String "2026-03-16T13:10:02Z")]
  extractCursorValue root singleFieldPaginationFields row
    @?= Right (PaginationCursor (Map.fromList [("createdAt", "2026-03-16T13:10:02Z")]))

testExtractCursorCompositeOrder :: IO ()
testExtractCursorCompositeOrder = do
  root <- parseRootSelection compositeCursorQueryText
  let row =
        Aeson.object
          [ ("createdAt", Aeson.String "2026-05-12T08:07:32.542661+02:00"),
            ("date", Aeson.String "2026-05-11"),
            ("transactionNo", Aeson.String "CST000000000016390")
          ]
  extractCursorValue root compositePaginationFields row
    @?= Right
      ( PaginationCursor
          ( Map.fromList
              [ ("createdAt", "2026-05-12T08:07:32.542661+02:00"),
                ("date", "2026-05-11"),
                ("transactionNo", "CST000000000016390")
              ]
          )
      )

testExtractCursorValueNested :: IO ()
testExtractCursorValueNested = do
  root <- parseRootSelection nestedCursorQueryText
  let row =
        Aeson.object
          [ ("payment_request_id", Aeson.String "wt_123"),
            ("customer", Aeson.object [("createdAt", Aeson.String "2026-03-16T13:10:02Z"), ("email", Aeson.String "ada@example.com")])
          ]
  extractCursorValue root (PaginationField "customer.createdAt" PaginationDesc :| []) row
    @?= Right (PaginationCursor (Map.fromList [("customer.createdAt", "2026-03-16T13:10:02Z")]))

-- Hasura sends an integer key as a JSON number, and the cursor value goes back to it
-- as the text of a comparison: "2160.0" fails with "invalid input syntax for type
-- integer" and the whole export dies.
testExtractCursorValueIntegral :: IO ()
testExtractCursorValueIntegral = do
  root <- parseRootSelection integralCursorQueryText
  let row = Aeson.object [("id", Aeson.Number 2160)]
  extractCursorValue root (PaginationField "id" PaginationDesc :| []) row
    @?= Right (PaginationCursor (Map.fromList [("id", "2160")]))

testExtractCursorValueFractional :: IO ()
testExtractCursorValueFractional = do
  root <- parseRootSelection integralCursorQueryText
  let row = Aeson.object [("id", Aeson.Number 1.5)]
  extractCursorValue root (PaginationField "id" PaginationDesc :| []) row
    @?= Right (PaginationCursor (Map.fromList [("id", "1.5")]))

testExtractCursorFallback :: IO ()
testExtractCursorFallback = do
  root <- parseRootSelection fallbackCursorQueryText
  let row = Aeson.object [("internalCursor", Aeson.String "cursor_001")]
  extractCursorValue root fallbackPaginationFields row
    @?= Right (PaginationCursor (Map.fromList [("internalCursor", "cursor_001")]))

testResolvePaginationKeyDefault :: IO ()
testResolvePaginationKeyDefault = do
  let gq = GenericQuery {paginationKey = Nothing, orderBy = Nothing, query = "query {}", variables = Aeson.object []}
  resolvePaginationKey gq @?= "createdAt"

testResolvePaginationFieldsUsesOrderBy :: IO ()
testResolvePaginationFieldsUsesOrderBy = do
  let gq =
        GenericQuery
          { paginationKey = Just "createdAt",
            orderBy = Nothing,
            query = "query {}",
            variables =
              Aeson.object
                [ ( "orderBy",
                    Aeson.Array
                      ( Vector.fromList
                          [ Aeson.object [("createdAt", Aeson.String "DESC")],
                            Aeson.object [("date", Aeson.String "DESC")],
                            Aeson.object [("transactionNo", Aeson.String "DESC")]
                          ]
                      )
                  )
                ]
          }
  resolvePaginationFields gq @?= compositePaginationFields

testResolvePaginationFieldsNestedOrderBy :: IO ()
testResolvePaginationFieldsNestedOrderBy = do
  let orderBy =
        Aeson.Array
          ( Vector.fromList
              [ Aeson.object [("customer", Aeson.object [("createdAt", Aeson.String "DESC")])],
                Aeson.object [("transactionNo", Aeson.String "DESC")]
              ]
          )
      gq =
        GenericQuery
          { paginationKey = Just "customer.createdAt",
            orderBy = Just orderBy,
            query = "query {}",
            variables = Aeson.object []
          }
  resolvePaginationFields gq @?= nestedPaginationFields

testBuildRequestBody :: IO ()
testBuildRequestBody = do
  let gq =
        GenericQuery
          { paginationKey = Just "createdAt",
            orderBy = Nothing,
            query = "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { paymentRequests { payment_request_id: waitToken } }",
            variables = Aeson.object [("existingVar", Aeson.String "kept")]
          }
      payload = buildRequestBody singleFieldPaginationFields 100 (Just (PaginationCursor (Map.fromList [("createdAt", "cursor_123")]))) gq
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

testBuildRequestBodyComposite :: IO ()
testBuildRequestBodyComposite = do
  let gq =
        GenericQuery
          { paginationKey = Just "createdAt",
            orderBy = Nothing,
            query = "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { CashSettlement { createdAt date transactionNo } }",
            variables =
              Aeson.object
                [ ( "orderBy",
                    Aeson.Array
                      ( Vector.fromList
                          [ Aeson.object [("createdAt", Aeson.String "DESC")],
                            Aeson.object [("date", Aeson.String "DESC")],
                            Aeson.object [("transactionNo", Aeson.String "DESC")]
                          ]
                      )
                  )
                ]
          }
      payload =
        buildRequestBody
          compositePaginationFields
          250
          ( Just
              ( PaginationCursor
                  ( Map.fromList
                      [ ("createdAt", "2026-05-12T08:07:32.542661+02:00"),
                        ("date", "2026-05-11"),
                        ("transactionNo", "CST000000000016390")
                      ]
                  )
              )
          )
          gq
  case Aeson.eitherDecode payload of
    Left err -> assertFailure err
    Right (val :: Aeson.Value) ->
      val
        @?= Aeson.object
          [ ("paginationKey", Aeson.String "createdAt"),
            ("query", Aeson.String gq.query),
            ( "variables",
              Aeson.object
                [ ( "orderBy",
                    Aeson.Array
                      ( Vector.fromList
                          [ Aeson.object [("createdAt", Aeson.String "DESC")],
                            Aeson.object [("date", Aeson.String "DESC")],
                            Aeson.object [("transactionNo", Aeson.String "DESC")]
                          ]
                      )
                  ),
                  ("rowLimit", Aeson.Number 250),
                  ( "paginationCondition",
                    Aeson.object
                      [ ( "_or",
                          Aeson.toJSON
                            [ Aeson.object [("createdAt", Aeson.object [("_lt", Aeson.String "2026-05-12T08:07:32.542661+02:00")])],
                              Aeson.object
                                [ ( "_and",
                                    Aeson.toJSON
                                      [ Aeson.object [("createdAt", Aeson.object [("_eq", Aeson.String "2026-05-12T08:07:32.542661+02:00")])],
                                        Aeson.object
                                          [ ( "_or",
                                              Aeson.toJSON
                                                [ Aeson.object [("date", Aeson.object [("_lt", Aeson.String "2026-05-11")])],
                                                  Aeson.object
                                                    [ ( "_and",
                                                        Aeson.toJSON
                                                          [ Aeson.object [("date", Aeson.object [("_eq", Aeson.String "2026-05-11")])],
                                                            Aeson.object [("transactionNo", Aeson.object [("_lt", Aeson.String "CST000000000016390")])]
                                                          ]
                                                      )
                                                    ]
                                                ]
                                            )
                                          ]
                                      ]
                                  )
                                ]
                            ]
                        )
                      ]
                  )
                ]
            )
          ]

testBuildRequestBodyExplicitOrderBy :: IO ()
testBuildRequestBodyExplicitOrderBy = do
  let orderBy = Aeson.Array (Vector.fromList [Aeson.object [("createdAt", Aeson.String "DESC")]])
      gq =
        GenericQuery
          { paginationKey = Just "createdAt",
            orderBy = Just orderBy,
            query = "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!, $orderBy: [paymentRequests_order_by!]) { paymentRequests { payment_request_id: waitToken } }",
            variables = Aeson.object [("orderBy", Aeson.Array Vector.empty)]
          }
      payload = buildRequestBody singleFieldPaginationFields 100 Nothing gq
  case Aeson.eitherDecode payload of
    Left err -> assertFailure err
    Right (val :: Aeson.Value) ->
      val
        @?= Aeson.object
          [ ("paginationKey", Aeson.String "createdAt"),
            ("query", Aeson.String gq.query),
            ( "variables",
              Aeson.object
                [ ("orderBy", orderBy),
                  ("rowLimit", Aeson.Number 100),
                  ("paginationCondition", Aeson.object [])
                ]
            )
          ]

testBuildRequestBodyNested :: IO ()
testBuildRequestBodyNested = do
  let orderBy =
        Aeson.Array
          ( Vector.fromList
              [ Aeson.object [("customer", Aeson.object [("createdAt", Aeson.String "DESC")])],
                Aeson.object [("transactionNo", Aeson.String "DESC")]
              ]
          )
      gq =
        GenericQuery
          { paginationKey = Just "customer.createdAt",
            orderBy = Just orderBy,
            query = "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!, $orderBy: [paymentRequests_order_by!]) { paymentRequests { customer { createdAt } transactionNo } }",
            variables = Aeson.object []
          }
      payload =
        buildRequestBody
          nestedPaginationFields
          100
          ( Just
              ( PaginationCursor
                  ( Map.fromList
                      [ ("customer.createdAt", "2026-03-16T13:10:02Z"),
                        ("transactionNo", "tx_123")
                      ]
                  )
              )
          )
          gq
  case Aeson.eitherDecode payload of
    Left err -> assertFailure err
    Right (val :: Aeson.Value) ->
      val
        @?= Aeson.object
          [ ("paginationKey", Aeson.String "customer.createdAt"),
            ("query", Aeson.String gq.query),
            ( "variables",
              Aeson.object
                [ ("orderBy", orderBy),
                  ("rowLimit", Aeson.Number 100),
                  ( "paginationCondition",
                    Aeson.object
                      [ ( "_or",
                          Aeson.toJSON
                            [ Aeson.object [("customer", Aeson.object [("createdAt", Aeson.object [("_lt", Aeson.String "2026-03-16T13:10:02Z")])])],
                              Aeson.object
                                [ ( "_and",
                                    Aeson.toJSON
                                      [ Aeson.object [("customer", Aeson.object [("createdAt", Aeson.object [("_eq", Aeson.String "2026-03-16T13:10:02Z")])])],
                                        Aeson.object [("transactionNo", Aeson.object [("_lt", Aeson.String "tx_123")])]
                                      ]
                                  )
                                ]
                            ]
                        )
                      ]
                  )
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
  fmap (byHeader columnConfig) <$> decodeResponseRows columnConfig "paymentRequests" paymentRequestKeys response
    @?= Right (Vector.fromList [Map.fromList [("Payment Request ID", "wt_777"), ("Placed At", "2026-03-16T14:22:00Z")]])

testDecodeResponsePage :: IO ()
testDecodeResponsePage = do
  let response =
        ByteString.Char8.pack
          "{\"data\":{\"paymentRequests\":[{\"payment_request_id\":\"wt_777\",\"placed_at\":\"2026-03-16T14:22:00Z\"},{\"payment_request_id\":\"wt_778\",\"placed_at\":\"2026-03-16T14:23:00Z\"}]}}"
  case decodeResponsePage columnConfig "paymentRequests" paymentRequestKeys response of
    Left err -> assertFailure err
    Right (Left responseErr) -> assertFailure (show responseErr)
    Right (Right decodedPage) -> do
      decodedResponsePageBytes decodedPage @?= LByteString.fromStrict "wt_777,2026-03-16T14:22:00Z\r\nwt_778,2026-03-16T14:23:00Z\r\n"
      decodedPage.rowCount @?= 2
      byHeader columnConfig <$> decodedPage.lastRow @?= Just (Map.fromList [("Payment Request ID", "wt_778"), ("Placed At", "2026-03-16T14:23:00Z")])
      decodedPage.lastRawRow @?= Just (Aeson.object [("payment_request_id", Aeson.String "wt_778"), ("placed_at", Aeson.String "2026-03-16T14:23:00Z")])

testDecodeResponseChunk :: IO ()
testDecodeResponseChunk = do
  let response =
        ByteString.Char8.pack
          "{\"data\":{\"paymentRequests\":[{\"payment_request_id\":\"wt_777\",\"placed_at\":\"2026-03-16T14:22:00Z\"},{\"payment_request_id\":\"wt_778\",\"placed_at\":\"2026-03-16T14:23:00Z\"}]}}"
      firstRow = LByteString.fromStrict "wt_777,2026-03-16T14:22:00Z\r\n"
  case decodeResponseChunk (LByteString.length firstRow) columnConfig "paymentRequests" paymentRequestKeys response of
    Left err -> assertFailure err
    Right (Left responseErr) -> assertFailure (show responseErr)
    Right (Right decodedPage) -> do
      decodedResponsePageBytes decodedPage @?= firstRow
      decodedPage.rowCount @?= 1
      byHeader columnConfig <$> decodedPage.lastRow @?= Just (Map.fromList [("Payment Request ID", "wt_777"), ("Placed At", "2026-03-16T14:22:00Z")])

testDecodeResponsePageError :: IO ()
testDecodeResponsePageError = do
  let response = ByteString.Char8.pack "{\"error\":\"transient upstream error\"}"
  case decodeResponsePage columnConfig "paymentRequests" paymentRequestKeys response of
    Left err -> assertFailure err
    Right (Left responseErr) -> responseErr @?= ResponseContainsError "transient upstream error"
    Right (Right _) -> assertFailure "expected top-level response error"

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

hasuraRoleClaims :: Aeson.Value
hasuraRoleClaims =
  Aeson.object
    [ ("associated_email", Aeson.String "ops@kronor.io"),
      ( "https://hasura.io/jwt/claims",
        Aeson.object
          [ ("x-hasura-default-role", Aeson.String "accountant_query"),
            ("x-hasura-allowed-roles", Aeson.toJSON ["accountant_query", "po_manager_query" :: Text]),
            ("x-hasura-user-id", Aeson.String "7")
          ]
      )
    ]

testSelectHasuraRole :: IO ()
testSelectHasuraRole =
  selectHasuraRole "po_manager_query" hasuraRoleClaims
    @?= Right
      ( Aeson.object
          [ ("associated_email", Aeson.String "ops@kronor.io"),
            ( "https://hasura.io/jwt/claims",
              Aeson.object
                [ ("x-hasura-default-role", Aeson.String "po_manager_query"),
                  ("x-hasura-allowed-roles", Aeson.toJSON ["po_manager_query" :: Text]),
                  ("x-hasura-user-id", Aeson.String "7")
                ]
            )
          ]
      )

testSelectHasuraRoleClaimCase :: IO ()
testSelectHasuraRoleClaimCase =
  selectHasuraRole "po_manager_query" (hasuraClaimsWith [("X-Hasura-Default-Role", Aeson.String "accountant_query"), ("X-Hasura-Allowed-Roles", Aeson.toJSON ["accountant_query", "po_manager_query" :: Text])])
    @?= Right (hasuraClaimsWith [("x-hasura-default-role", Aeson.String "po_manager_query"), ("x-hasura-allowed-roles", Aeson.toJSON ["po_manager_query" :: Text])])
  where
    hasuraClaimsWith fields = Aeson.object [("https://hasura.io/jwt/claims", Aeson.object fields)]

testSelectHasuraRoleClaimsNotObject :: IO ()
testSelectHasuraRoleClaimsNotObject =
  selectHasuraRole "po_manager_query" (Aeson.object [("https://hasura.io/jwt/claims", Aeson.String "{\"x-hasura-allowed-roles\":[\"po_manager_query\"]}")])
    @?= Left HasuraClaimsNotObject

testSelectHasuraRoleNotAllowed :: IO ()
testSelectHasuraRoleNotAllowed =
  selectHasuraRole "admin" hasuraRoleClaims @?= Left HasuraRoleNotAllowed

testSelectHasuraRoleMissingClaims :: IO ()
testSelectHasuraRoleMissingClaims =
  selectHasuraRole "po_manager_query" (Aeson.object [("associated_email", Aeson.String "ops@kronor.io")])
    @?= Left HasuraClaimsMissing

testCompletionEmailWithUrl :: IO ()
testCompletionEmailWithUrl =
  mkCompletionEmail "CSV" (Just "https://signed.example.com/report.csv")
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
  mkCompletionEmail "CSV" Nothing
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
    [ ("payment_request_id", ColumnSettings Nothing (Just "Payment Request ID") Nothing False Nothing),
      ("transaction_id", ColumnSettings Nothing (Just "Transaction ID") Nothing False Nothing),
      ("merchant_id", ColumnSettings Nothing (Just "Merchant ID") Nothing False Nothing),
      ("placed_at", ColumnSettings Nothing (Just "Placed At") Nothing False Nothing),
      ("reference", ColumnSettings Nothing (Just "Reference") Nothing False Nothing),
      ("payment_method", ColumnSettings Nothing (Just "Payment Method") Nothing False Nothing),
      ("attempts", ColumnSettings Nothing (Just "Card Type") (Just "payment.cardType") False Nothing),
      ("customer", ColumnSettings Nothing (Just "Customer Email") (Just "profile.email") False Nothing),
      ("latest_status", ColumnSettings Nothing (Just "Latest Status") Nothing False Nothing),
      ("currency", ColumnSettings Nothing (Just "Currency") Nothing False Nothing),
      ("amount", ColumnSettings Nothing (Just "Amount") Nothing False Nothing)
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

aliasedRootQueryText :: Text
aliasedRootQueryText =
  "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { \
  \  journalEntries: paymentRequests(limit: $rowLimit, where: $paginationCondition) { \
  \    payment_request_id: waitToken \
  \  } \
  \}"

fragmentOnlyRootQueryText :: Text
fragmentOnlyRootQueryText =
  "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { \
  \  ...RootFields \
  \} \
  \fragment RootFields on query_root { \
  \  paymentRequests(limit: $rowLimit, where: $paginationCondition) { \
  \    payment_request_id: waitToken \
  \  } \
  \}"

fallbackCursorQueryText :: Text
fallbackCursorQueryText =
  "query { orders { internalCursor } }"

missingCursorQueryText :: Text
missingCursorQueryText =
  "query { paymentRequests { payment_request_id: waitToken } }"

compositeCursorQueryText :: Text
compositeCursorQueryText =
  "query { CashSettlement { transactionNo date createdAt } }"

integralCursorQueryText :: Text
integralCursorQueryText =
  "query { transfers { id } }"

nestedCursorQueryText :: Text
nestedCursorQueryText =
  "query { paymentRequests { payment_request_id: waitToken customer { createdAt } } }"

singleFieldPaginationFields :: NonEmpty PaginationField
singleFieldPaginationFields = PaginationField "createdAt" PaginationDesc :| []

fallbackPaginationFields :: NonEmpty PaginationField
fallbackPaginationFields = PaginationField "internalCursor" PaginationDesc :| []

compositePaginationFields :: NonEmpty PaginationField
compositePaginationFields =
  PaginationField "createdAt" PaginationDesc
    :| [PaginationField "date" PaginationDesc, PaginationField "transactionNo" PaginationDesc]

nestedPaginationFields :: NonEmpty PaginationField
nestedPaginationFields =
  PaginationField "customer.createdAt" PaginationDesc
    :| [PaginationField "transactionNo" PaginationDesc]

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

testClassifyHttpStatusErrorServerError :: IO ()
testClassifyHttpStatusErrorServerError =
  classifyHttpStatusError 502
    @?= Retry "GraphQL request failed with HTTP status: 502"

testClassifyHttpStatusErrorThrottled :: IO ()
testClassifyHttpStatusErrorThrottled =
  classifyHttpStatusError 429
    @?= Retry "GraphQL request failed with HTTP status: 429"

testClassifyHttpStatusErrorClientError :: IO ()
testClassifyHttpStatusErrorClientError =
  classifyHttpStatusError 401
    @?= Giveup "GraphQL request failed with HTTP status: 401"

testValidateGraphqlQueryBodyValid :: IO ()
testValidateGraphqlQueryBodyValid =
  SmartCsvValidation.validateGraphqlQueryBody gqlQueryText @?= Right ()

testValidateGraphqlQueryBodyMissingPaginationCondition :: IO ()
testValidateGraphqlQueryBodyMissingPaginationCondition =
  SmartCsvValidation.validateGraphqlQueryBody "query ($rowLimit: Int!) { paymentRequests(limit: $rowLimit) { payment_request_id: waitToken } }"
    @?= Left (pure "The query must define a paginationCondition variable.")

testValidateGraphqlQueryBodyAndGetRootFieldValid :: IO ()
testValidateGraphqlQueryBodyAndGetRootFieldValid =
  SmartCsvValidation.validateGraphqlQueryBodyAndGetRootField gqlQueryText
    @?= Right "paymentRequests"

testValidateGraphqlQueryBodyAndGetRootFieldAlias :: IO ()
testValidateGraphqlQueryBodyAndGetRootFieldAlias =
  SmartCsvValidation.validateGraphqlQueryBodyAndGetRootField aliasedRootQueryText
    @?= Right "paymentRequests"

testValidateGraphqlQueryBodyAndGetRootFieldRejectsNonFieldRoot :: IO ()
testValidateGraphqlQueryBodyAndGetRootFieldRejectsNonFieldRoot =
  SmartCsvValidation.validateGraphqlQueryBodyAndGetRootField fragmentOnlyRootQueryText
    @?= Left (pure "The query root must be a field selection.")

testValidateQueryVariablesInvalidJson :: IO ()
testValidateQueryVariablesInvalidJson =
  SmartCsvValidation.validateQueryVariables "not valid json"
    @?= Left "Invalid JSON"

testValidateQueryVariablesNonObject :: IO ()
testValidateQueryVariablesNonObject =
  SmartCsvValidation.validateQueryVariables "[1,2,3]"
    @?= Left "Query variables must be a JSON object"

testValidateQueryVariablesValid :: IO ()
testValidateQueryVariablesValid =
  case SmartCsvValidation.validateQueryVariables "{\"conditions\":{\"createdAt\":{\"_gte\":\"2026-03-01T00:00:00Z\",\"_lt\":\"2026-03-15T00:00:00Z\"}}}" of
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
      [ ("field_a", ColumnSettings (Just 3) (Just "Column A") (Just "customer.email") False Nothing),
        ("field_b", ColumnSettings Nothing Nothing Nothing False Nothing)
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
      result = csvifyByHeader mempty "orders" row
  Map.lookup "payment_request_id" result @?= Just "wt_123"
  Map.lookup "customer" result @?= Just "user@example.com"
  Map.lookup "tags" result @?= Just "vip"

testCsvifyMultiKeyObject :: IO ()
testCsvifyMultiKeyObject = do
  let row =
        Aeson.object
          [ ("customer", Aeson.object [("email", Aeson.String "user@example.com"), ("name", Aeson.String "Ada")]) ]
      result = csvifyByHeader mempty "orders" row
  Map.lookup "customer" result @?= Nothing

testCsvifyArrayValues :: IO ()
testCsvifyArrayValues = do
  let config =
        Map.fromList
          [ ("amounts", ColumnSettings (Just 1) (Just "Amounts") Nothing False Nothing),
            ("attempts", ColumnSettings Nothing (Just "Card Types") (Just "payment.cardType") False Nothing)
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
      result = csvifyByHeader config "orders" row
  Map.lookup "Amounts" result @?= Just "12,3, 56,7"
  Map.lookup "Card Types" result @?= Just "VISA, MASTERCARD"

testCsvifyArrayIgnoresNulls :: IO ()
testCsvifyArrayIgnoresNulls = do
  let config =
        Map.fromList
          [ ("amounts", ColumnSettings (Just 1) (Just "Amounts") Nothing False Nothing),
            ("attempts", ColumnSettings Nothing (Just "Card Types") (Just "payment.cardType") False Nothing)
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
      result = csvifyByHeader config "orders" row
  Map.lookup "Amounts" result @?= Just "12,3, 56,7"
  Map.lookup "Card Types" result @?= Just "VISA, MASTERCARD"

testCsvifyArrayObjectNullFields :: IO ()
testCsvifyArrayObjectNullFields = do
  let config =
        Map.fromList
          [("attempts", ColumnSettings Nothing (Just "Card Types") (Just "payment.cardType") False Nothing)]
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
      result = csvifyByHeader config "orders" row
  Map.lookup "Card Types" result @?= Just ""

testCsvifyCustomDecimalPlaces :: IO ()
testCsvifyCustomDecimalPlaces = do
  let config =
        Map.fromList
          [ ("amount", ColumnSettings (Just 3) (Just "Amount") Nothing False Nothing),
            ("exchange_rate", ColumnSettings (Just 4) (Just "Rate") Nothing False Nothing)
          ]
      row =
        Aeson.object
          [ ("amount", Aeson.Number 12.3456),
            ("exchange_rate", Aeson.String "1.23456")
          ]
      result = csvifyByHeader config "orders" row
  Map.lookup "Amount" result @?= Just "12,345"
  Map.lookup "Rate" result @?= Just "1,2345"

-- | An entry of @columns@: header, data path, decimal places.
-- | 'csvify' by header, for assertions that read like the file (see 'byHeader').
csvifyByHeader :: ColumnConfig -> Text -> Aeson.Value -> Map Text ByteString
csvifyByHeader config root row = byHeader config (csvify config root row)

paymentRequestKeys :: Vector ColumnKey
paymentRequestKeys = Vector.fromList [ColumnKey "payment_request_id" Nothing, ColumnKey "placed_at" Nothing]

columnEntry :: Maybe Text -> Text -> Maybe Int -> ColumnSettings
columnEntry header dataPath decimals = ColumnSettings decimals header (Just dataPath) False Nothing

-- | A field printed as the given entries.
fieldWithColumns :: [ColumnSettings] -> ColumnSettings
fieldWithColumns entries = defaultColumnSettings {columns = nonEmpty entries}

testParseColumnConfigColumns :: IO ()
testParseColumnConfigColumns = do
  let json =
        Aeson.object
          [ ( "customer",
              Aeson.object
                [ ("header", "ignored"),
                  -- Not a number, but unused: a field with columns only prints its
                  -- entries, so this must not drop them.
                  ("decimalPlaces", "2"),
                  ( "columns",
                    Aeson.toJSON
                      [ Aeson.object [("header", "Customer Name"), ("dataPath", "name")],
                        Aeson.object [("dataPath", "balance"), ("decimalPlaces", Aeson.Number 2)]
                      ]
                  )
                ]
            )
          ]
  parseColumnConfig json
    @?= Map.fromList
      [("customer", fieldWithColumns [columnEntry (Just "Customer Name") "name" Nothing, columnEntry Nothing "balance" (Just 2)])]

testParseColumnConfigEmptyColumns :: IO ()
testParseColumnConfigEmptyColumns = do
  let json =
        Aeson.object
          [ ("customer", Aeson.object [("columns", Aeson.toJSON ([] :: [Aeson.Value]))]),
            ("amount", Aeson.object [("header", "Amount")])
          ]
  parseColumnConfig json @?= Map.fromList [("amount", defaultColumnSettings {header = Just "Amount"})]

testCsvifyColumns :: IO ()
testCsvifyColumns = do
  let config =
        Map.fromList
          [ ( "customer",
              fieldWithColumns
                [ columnEntry (Just "Customer Name") "name" Nothing,
                  columnEntry Nothing "email" Nothing,
                  columnEntry (Just "Balance") "account.balance" (Just 2)
                ]
            )
          ]
      row =
        Aeson.object
          [ ( "customer",
              Aeson.object
                [ ("name", "Ada"),
                  ("email", "ada@example.com"),
                  ("account", Aeson.object [("balance", Aeson.Number 12.345)])
                ]
            ),
            ("amount", Aeson.Number 10)
          ]
  csvifyByHeader config "orders" row
    @?= Map.fromList
      [ ("Customer Name", "Ada"),
        -- An entry without a header is headed by the field and its data path.
        ("customer.email", "ada@example.com"),
        ("Balance", "12,34"),
        ("amount", "10")
      ]

-- | An entry of @columns@ prints exactly what a field of its own with the same
-- settings would.
testCsvifyColumnsLikeOwnColumns :: IO ()
testCsvifyColumnsLikeOwnColumns = do
  let customer =
        Aeson.object
          [ ("name", "Ada"),
            ("balance", Aeson.String "1234.5678"),
            ("tags", Aeson.toJSON ["a", "b" :: Text])
          ]
      fanOut =
        Map.fromList
          [ ( "customer",
              fieldWithColumns
                [ columnEntry (Just "Name") "name" Nothing,
                  columnEntry (Just "Balance") "balance" (Just 3),
                  columnEntry (Just "Tags") "tags" Nothing
                ]
            )
          ]
      separate =
        Map.fromList
          [ ("name", ColumnSettings Nothing (Just "Name") (Just "name") False Nothing),
            ("balance", ColumnSettings (Just 3) (Just "Balance") (Just "balance") False Nothing),
            ("tags", ColumnSettings Nothing (Just "Tags") (Just "tags") False Nothing)
          ]
  csvifyByHeader fanOut "orders" (Aeson.object [("customer", customer)])
    @?= csvifyByHeader separate "orders" (Aeson.object [("name", customer), ("balance", customer), ("tags", customer)])

testCsvifyColumnsArray :: IO ()
testCsvifyColumnsArray = do
  let config =
        Map.fromList
          [ ( "attempts",
              fieldWithColumns
                [ columnEntry (Just "Card Type") "cardType" Nothing,
                  columnEntry (Just "Card Country") "cardCountry" Nothing
                ]
            )
          ]
      attempt cardType country = Aeson.object [("cardType", cardType), ("cardCountry", country)]
      row = Aeson.object [("attempts", Aeson.toJSON [attempt "VISA" "SE", attempt "MC" Aeson.Null])]
  csvifyByHeader config "transactions" row
    @?= Map.fromList [("Card Type", "VISA, MC"), ("Card Country", "SE")]

testCsvifyNumericPreservesOriginal :: IO ()
testCsvifyNumericPreservesOriginal = do
  let config =
        Map.fromList
          [ ("price", ColumnSettings Nothing (Just "Price") Nothing False Nothing),
            ("quantity", ColumnSettings Nothing (Just "Quantity") Nothing False Nothing),
            ("discount", ColumnSettings Nothing (Just "Discount") Nothing False Nothing)
          ]
      row =
        Aeson.object
          [ ("price", Aeson.Number 123.456),
            ("quantity", Aeson.String "50.10"),
            ("discount", Aeson.Number 10)
          ]
      result = csvifyByHeader config "orders" row
  Map.lookup "Price" result @?= Just "123,456"
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
          [ ("payment_request_id", ColumnSettings Nothing (Just "Order ID") Nothing False Nothing),
            ("placed_at", ColumnSettings Nothing (Just "Placed At") Nothing False Nothing),
            ("customer", ColumnSettings Nothing (Just "Customer Email") (Just "profile.email") False Nothing),
            ("attempts", ColumnSettings Nothing (Just "Card Type") (Just "payment.cardType") False Nothing)
          ]
  root <- parseRootSelection gqlQueryText
  inferHeaders config root
    @?= [ "Order ID",
          "Placed At",
          "Customer Email",
          "Card Type"
        ]

testInferHeadersColumns :: IO ()
testInferHeadersColumns = do
  root <-
    parseRootSelection
      "query ($rowLimit: Int!, $paginationCondition: payment_requests_bool_exp!) { \
      \  paymentRequests(limit: $rowLimit, where: $paginationCondition) { \
      \    waitToken \
      \    customer { name email } \
      \    amount \
      \  } \
      \}"
  let config =
        Map.fromList
          [ ( "customer",
              fieldWithColumns [columnEntry (Just "Customer Name") "name" Nothing, columnEntry Nothing "email" Nothing]
            ),
            ("amount", defaultColumnSettings {header = Just "Amount"})
          ]
  inferHeaders config root @?= ["waitToken", "Customer Name", "customer.email", "Amount"]

testCsvRowBytesSharedHeader :: IO ()
testCsvRowBytesSharedHeader = do
  root <- parseRootSelection "query { orders { customer { billing { email } shipping { email } } } }"
  let email dataPath = columnEntry (Just "Email") dataPath Nothing
      config = Map.fromList [("customer", fieldWithColumns [email "billing.email", email "shipping.email"])]
      address value = Aeson.object [("email", Aeson.String value)]
      row = Aeson.object [("customer", Aeson.object [("billing", address "a@example.com"), ("shipping", address "b@example.com")])]
      fileColumns = inferColumns config root
  (.header) <$> fileColumns @?= ["Email", "Email"]
  csvRowBytes config "orders" (Vector.fromList ((.key) <$> fileColumns)) row @?= "a@example.com,b@example.com\r\n"

testExtractCursorMissingSelection :: IO ()
testExtractCursorMissingSelection = do
  root <- parseRootSelection missingCursorQueryText
  let row = Aeson.object [("payment_request_id", Aeson.String "wt_123")]
  extractCursorValue root singleFieldPaginationFields row
    @?= Left (CursorColumnMissing "createdAt")

-- The cursor is read from the response, not from the file's columns: a preset
-- that prints customer.name before customer.id, or leaves customer.id out, does
-- not change it.
testExtractCursorIgnoresColumns :: IO ()
testExtractCursorIgnoresColumns = do
  root <- parseRootSelection "query { orders { customer { name id } } }"
  let row = Aeson.object [("customer", Aeson.object [("name", Aeson.String "Ada"), ("id", Aeson.String "c_1")])]
      nameOnly = Map.fromList [("customer", fieldWithColumns [columnEntry (Just "Name") "name" Nothing])]
  inferHeaders nameOnly root @?= ["Name"]
  extractCursorValue root (PaginationField "customer.id" PaginationAsc :| []) row
    @?= Right (PaginationCursor (Map.fromList [("customer.id", "c_1")]))

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
  fmap (byHeader mempty) <$> decodeResponseRows mempty "orders" (Vector.fromList [ColumnKey "reference_col" Nothing, ColumnKey "amount_col" Nothing]) response
    @?= Right (Vector.fromList [Map.fromList [("reference_col", "ref_001"), ("amount_col", "1500")]])

testCsvifyNumericStringColumn :: IO ()
testCsvifyNumericStringColumn = do
  let config =
        Map.fromList
          [ ("amount", ColumnSettings Nothing (Just "Amount") Nothing True Nothing),
            ("rate", ColumnSettings (Just 2) (Just "Rate") Nothing True Nothing),
            ("reference", ColumnSettings Nothing (Just "Reference") Nothing False Nothing)
          ]
      row =
        Aeson.object
          [ ("amount", Aeson.String "-1234.50"),
            ("rate", Aeson.String "10.4567"),
            ("reference", Aeson.String "12.50")
          ]
      result = csvifyByHeader config "orders" row
  Map.lookup "Amount" result @?= Just "-1234,50"
  Map.lookup "Rate" result @?= Just "10,45"
  Map.lookup "Reference" result @?= Just "12.50"

-- | A cut-down introspection response: Hasura wraps a root list field as [T!]! and
-- a nullable scalar as the bare type.
introspectionResponse :: Aeson.Value
introspectionResponse =
  Aeson.object
    [ ( "data",
        Aeson.object
          [ ( "__schema",
              Aeson.object
                [ ("queryType", Aeson.object [("name", "query_root")]),
                  ( "types",
                    Aeson.toJSON
                      [ objectType "query_root" [("Journal", nonNull (list (nonNull (named "Journal"))))],
                        objectType
                          "Journal"
                          [ ("id", nonNull (named "bigint")),
                            ("totalAmount", named "numeric"),
                            ("exchangeRate", nonNull (named "numeric")),
                            ("memo", named "citext"),
                            ("subsidiary", nonNull (named "Subsidiary")),
                            ("journalLines", nonNull (list (nonNull (named "JournalLine"))))
                          ],
                        objectType "Subsidiary" [("name", named "String"), ("vatRate", named "float8")],
                        objectType "JournalLine" [("amount", nonNull (named "numeric")), ("memo", named "String")],
                        Aeson.object [("name", "numeric"), ("fields", Aeson.Null)]
                      ]
                  )
                ]
            )
          ]
      )
    ]
  where
    objectType :: Text -> [(Text, Aeson.Value)] -> Aeson.Value
    objectType name fields =
      Aeson.object
        [ ("name", Aeson.toJSON name),
          ("fields", Aeson.toJSON [Aeson.object [("name", Aeson.toJSON fieldName), ("type", fieldType)] | (fieldName, fieldType) <- fields])
        ]
    named :: Text -> Aeson.Value
    named name = Aeson.object [("name", Aeson.toJSON name), ("ofType", Aeson.Null)]
    nonNull inner = Aeson.object [("name", Aeson.Null), ("ofType", inner)]
    list inner = Aeson.object [("name", Aeson.Null), ("ofType", inner)]

parsedSchema :: IO SchemaTypes
parsedSchema =
  either (\err -> assertFailure err >> error "unreachable") pure (parseIntrospectionResponse introspectionResponse)

-- | Hasura answers a query it cannot parse with only an @errors@ key, and the export
-- then falls back to dot decimals, so a typo in the query shows up nowhere else.
testIntrospectionRequestBodyParses :: IO ()
testIntrospectionRequestBodyParses =
  case Aeson.decode introspectionRequestBody of
    Just (Aeson.Object body)
      | Just (Aeson.String q) <- KeyMap.lookup "query" body ->
          case parseRequest (GQLRequest {query = q, operationName = Nothing, variables = Nothing}) of
            Failure errs -> assertFailure ("introspection query parse failed: " <> show errs)
            Success _ _ -> pure ()
    _ -> assertFailure "introspectionRequestBody has no query string"

testParseIntrospectionResponse :: IO ()
testParseIntrospectionResponse = do
  schema <- parsedSchema
  schema.queryTypeName @?= "query_root"
  (Map.lookup "query_root" schema.fieldTypes >>= Map.lookup "Journal") @?= Just "Journal"
  (Map.lookup "Journal" schema.fieldTypes >>= Map.lookup "exchangeRate") @?= Just "numeric"
  (Map.lookup "Journal" schema.fieldTypes >>= Map.lookup "journalLines") @?= Just "JournalLine"
  Map.member "numeric" schema.fieldTypes @?= False

testNumericColumns :: IO ()
testNumericColumns = do
  schema <- parsedSchema
  root <-
    parseRootSelection
      "query ($rowLimit: Int!, $paginationCondition: Journal_bool_exp!) { \
      \  Journal(limit: $rowLimit, where: $paginationCondition) { \
      \    id \
      \    totalAmount \
      \    rate: exchangeRate \
      \    memo \
      \    subsidiary { name } \
      \    subsidiaryVat: subsidiary { vatRate } \
      \    subsidiaryBoth: subsidiary { name vatRate } \
      \    lineAmounts: journalLines { amount memo } \
      \    lineMemos: journalLines { amount memo } \
      \  } \
      \}"
  let config =
        Map.fromList
          [ ("lineAmounts", ColumnSettings Nothing Nothing (Just "amount") False Nothing),
            ("lineMemos", ColumnSettings Nothing Nothing (Just "memo") False Nothing)
          ]
  numericColumns schema config root @?= Set.fromList ((`ColumnKey` Nothing) <$> ["totalAmount", "rate", "subsidiaryVat", "lineAmounts"])

testMarkNumericColumns :: IO ()
testMarkNumericColumns = do
  let config = Map.fromList [("amount", ColumnSettings (Just 2) (Just "Amount") Nothing False Nothing)]
  markNumericColumns (Set.fromList [ColumnKey "amount" Nothing, ColumnKey "rate" Nothing]) config
    @?= Map.fromList
      [ ("amount", ColumnSettings (Just 2) (Just "Amount") Nothing True Nothing),
        ("rate", ColumnSettings Nothing Nothing Nothing True Nothing)
      ]

testNumericColumnsEntries :: IO ()
testNumericColumnsEntries = do
  schema <- parsedSchema
  root <-
    parseRootSelection
      "query ($rowLimit: Int!, $paginationCondition: Journal_bool_exp!) { \
      \  Journal(limit: $rowLimit, where: $paginationCondition) { \
      \    subsidiary { name vatRate } \
      \    journalLines { amount memo } \
      \  } \
      \}"
  let config =
        Map.fromList
          [ ("subsidiary", fieldWithColumns [columnEntry Nothing "name" Nothing, columnEntry Nothing "vatRate" Nothing]),
            ("journalLines", fieldWithColumns [columnEntry Nothing "amount" Nothing, columnEntry Nothing "memo" Nothing])
          ]
  numericColumns schema config root
    @?= Set.fromList [ColumnKey "subsidiary" (Just 1), ColumnKey "journalLines" (Just 0)]

testMarkNumericColumnsEntries :: IO ()
testMarkNumericColumnsEntries = do
  let config = Map.fromList [("totals", fieldWithColumns [columnEntry Nothing "net" Nothing, columnEntry Nothing "label" Nothing, columnEntry Nothing "vat" Nothing])]
      numeric entry = entry {numeric = True}
  markNumericColumns (Set.fromList [ColumnKey "totals" (Just 0), ColumnKey "totals" (Just 2)]) config
    @?= Map.fromList [("totals", fieldWithColumns [numeric (columnEntry Nothing "net" Nothing), columnEntry Nothing "label" Nothing, numeric (columnEntry Nothing "vat" Nothing)])]

testMarkNumericColumnsEntry :: IO ()
testMarkNumericColumnsEntry = do
  let name = columnEntry Nothing "name" Nothing
      vatRate = columnEntry Nothing "vatRate" Nothing
      config = Map.fromList [("subsidiary", fieldWithColumns [name, vatRate])]
  markNumericColumns (Set.fromList [ColumnKey "subsidiary" (Just 1)]) config
    @?= Map.fromList [("subsidiary", fieldWithColumns [name, vatRate {numeric = True}])]
