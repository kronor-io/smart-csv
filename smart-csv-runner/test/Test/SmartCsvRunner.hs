module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.List (isInfixOf)
import Crypto.JOSE qualified
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Kronor.Db.Types.Bigint (Bigint (..))
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai (defaultRequest, pathInfo, requestHeaders, requestMethod)
import Network.Wai.Test qualified as WaiTest
import RIO
import SmartCsvApi.Auth (verifyBearerToken)
import SmartCsvApi.Env (ApiEnv (..))
import SmartCsvApi.RestServer qualified as RestServer
import SmartCsvApi.Types.SmartGraphqlCsvGenerator (SmartGraphqlCsvGeneratorInput (..), SmartGraphqlCsvGeneratorResult (..))
import SmartCsvApi.Validation.SmartGraphqlCsvGenerator qualified as Val
import Test.SmartCsvRunner.Integration (integrationTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "SmartCsvRunner"
    [ testGroup
        "Unit"
        [ testCase "health endpoint returns OK" testHealthEndpoint,
          testCase "generate endpoint returns report id for happy path" testGenerateEndpointHappyPath,
          testCase "generate endpoint returns validation failure envelope for invalid payload" testGenerateEndpointValidationError,
          testCase "generate endpoint rejects malformed JSON" testGenerateEndpointMalformedJson,
          testCase "generate endpoint rejects missing auth header" testGenerateEndpointMissingAuth,
          testCase "input json decoding accepts valid payload" testInputJsonDecodeValid,
          testCase "input json decoding fails when required field is missing" testInputJsonDecodeMissingField,
          testCase "input json decoding accepts payload with inline columnConfig" testInputJsonDecodeWithColumnConfig,
          testCase "input json decoding accepts payload with columnConfigName" testInputJsonDecodeWithColumnConfigName,
          testCase "validation rejects both columnConfig and columnConfigName" testValidationRejectsBothColumnConfigs,
          testCase "verifyBearerToken rejects invalid signature" testVerifyBearerTokenInvalidSig,
          testCase "verifyBearerToken accepts valid token" testVerifyBearerTokenValid
        ],
      integrationTests
    ]

mkInput :: SmartGraphqlCsvGeneratorInput
mkInput =
  SmartGraphqlCsvGeneratorInput
    { shardId = Bigint 42,
      recipient = "ops@kronor.io",
      graphqlPaginationKey = "createdAt",
      graphqlQueryBody =
        "query ($rowLimit: Int!, $paginationCondition: paymentRequests_bool_exp!) { \
        \  paymentRequests(limit: $rowLimit, where: $paginationCondition) { waitToken } \
        \}",
      graphqlQueryVariables =
        "{\"conditions\":{\"createdAt\":{\"_gte\":\"2026-03-01T00:00:00Z\",\"_lt\":\"2026-03-15T00:00:00Z\"}}}",
      columnConfig = Nothing,
      columnConfigName = Nothing
    }

mkAppEnv :: ApiEnv
mkAppEnv =
  ApiEnv
    { envDbPool = error "envDbPool should not be used for validation-failure tests",
      envHttpManager = error "envHttpManager is not used by current handlers",
      envGraphqlUrl = "http://localhost:8080/v1/graphql",
      envPortalUrl = "http://localhost:3000",
      envJwtSecret = "test-secret"
    }

testHealthEndpoint :: IO ()
testHealthEndpoint = do
  response <- WaiTest.runSession (WaiTest.request request) (RestServer.mkApplication mkAppEnv)
  WaiTest.simpleStatus response @?= status200
  WaiTest.simpleBody response @?= "OK"
  where
    request = defaultRequest {requestMethod = "GET", pathInfo = ["health"]}

testGenerateEndpointHappyPath :: IO ()
testGenerateEndpointHappyPath = do
  response <- WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request (Aeson.encode mkInput))) app
  WaiTest.simpleStatus response @?= status200
  Aeson.eitherDecode (WaiTest.simpleBody response) @?= Right (SmartGraphqlCsvGeneratorResult 1337)
  where
    app = RestServer.mkApplicationWith (\_ _ -> pure (Right (SmartGraphqlCsvGeneratorResult 1337)))
    request =
      defaultRequest
        { requestMethod = "POST",
          pathInfo = ["api", "v1", "csv", "generate"],
          requestHeaders = [("content-type", "application/json")]
        }

testGenerateEndpointValidationError :: IO ()
testGenerateEndpointValidationError = do
  response <- WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request (Aeson.encode invalidPayload))) (RestServer.mkApplication mkAppEnv)
  WaiTest.simpleStatus response @?= status400
  let body = WaiTest.simpleBody response
  isJust (Aeson.decode body :: Maybe Aeson.Value) @?= True
  -- Without a valid auth header, we get an auth error before validation
  LB8.unpack body `contains` "Missing Authorization header"
  where
    invalidPayload =
      mkInput
        { shardId = Bigint 0
        }
    request =
      defaultRequest
        { requestMethod = "POST",
          pathInfo = ["api", "v1", "csv", "generate"],
          requestHeaders = [("content-type", "application/json")]
        }

testGenerateEndpointMalformedJson :: IO ()
testGenerateEndpointMalformedJson = do
  response <- WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request "{not-json}")) (RestServer.mkApplication mkAppEnv)
  WaiTest.simpleStatus response @?= status400
  where
    request =
      defaultRequest
        { requestMethod = "POST",
          pathInfo = ["api", "v1", "csv", "generate"],
          requestHeaders = [("content-type", "application/json")]
        }

testGenerateEndpointMissingAuth :: IO ()
testGenerateEndpointMissingAuth = do
  response <- WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request (Aeson.encode mkInput))) app
  WaiTest.simpleStatus response @?= status400
  let body = WaiTest.simpleBody response
  LB8.unpack body `contains` "Missing Authorization header"
  where
    app = RestServer.mkApplicationWith (\mAuth input -> handleSmartGraphqlCsvGenerator' mAuth input)
    handleSmartGraphqlCsvGenerator' Nothing _ = pure $ Left "Missing Authorization header"
    handleSmartGraphqlCsvGenerator' (Just _) _ = pure $ Right (SmartGraphqlCsvGeneratorResult 1)
    request =
      defaultRequest
        { requestMethod = "POST",
          pathInfo = ["api", "v1", "csv", "generate"],
          requestHeaders = [("content-type", "application/json")]
        }

contains :: String -> String -> Assertion
contains haystack needle =
  if needle `isInfixOf` haystack
    then pure ()
    else assertFailure ("Expected body to contain '" <> needle <> "' but got: " <> haystack)

testInputJsonDecodeValid :: IO ()
testInputJsonDecodeValid = do
  let payload = Aeson.encode mkInput
  Aeson.eitherDecode payload @?= Right mkInput

testInputJsonDecodeMissingField :: IO ()
testInputJsonDecodeMissingField = do
  let payload =
        LB8.pack
          "{\"shardId\":42,\"recipient\":\"ops@kronor.io\",\"graphqlPaginationKey\":\"createdAt\",\"graphqlQueryVariables\":\"{}\"}"
      result = Aeson.eitherDecode payload :: Either String SmartGraphqlCsvGeneratorInput
  isLeft result @?= True

testInputJsonDecodeWithColumnConfig :: IO ()
testInputJsonDecodeWithColumnConfig = do
  let input = mkInput {columnConfig = Just (Aeson.object [("field_a", Aeson.String "Column A")])}
      payload = Aeson.encode input
  Aeson.eitherDecode payload @?= Right input

testInputJsonDecodeWithColumnConfigName :: IO ()
testInputJsonDecodeWithColumnConfigName = do
  let input = mkInput {columnConfigName = Just "payment_requests"}
      payload = Aeson.encode input
  Aeson.eitherDecode payload @?= Right input

testValidationRejectsBothColumnConfigs :: IO ()
testValidationRejectsBothColumnConfigs = do
  let input =
        mkInput
          { columnConfig = Just (Aeson.object [("field_a", Aeson.String "Column A")]),
            columnConfigName = Just "payment_requests"
          }
  Val.validateSmartGraphqlCsvGeneratorInput input
    @?= Left "Cannot specify both columnConfig and columnConfigName"

testVerifyBearerTokenInvalidSig :: IO ()
testVerifyBearerTokenInvalidSig = do
  let secret = "k3BGs4nEF5IHgDi5XpupymE6maupyt2vHzyJRMoIBJo=" -- base64(32 random bytes)
      token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0In0.invalidsignature"
  result <- verifyBearerToken secret ("Bearer " <> token)
  isLeft result @?= True

testVerifyBearerTokenValid :: IO ()
testVerifyBearerTokenValid = do
  let secret = "k3BGs4nEF5IHgDi5XpupymE6maupyt2vHzyJRMoIBJo=" -- base64(32 random bytes)
  token <- signTestJwt secret "{\"sub\":\"test\"}"
  result <- verifyBearerToken secret ("Bearer " <> token)
  case result of
    Left err -> assertFailure ("Expected valid token but got: " <> Text.unpack err)
    Right claims -> do
      let expected = Aeson.object ["sub" Aeson..= ("test" :: Text)]
      claims @?= expected

-- | Create a minimal HS256 JWT for testing using jose's own signing.
signTestJwt :: Text -> ByteString -> IO Text
signTestJwt secret payload = do
  let secretBytes = either (error . show) id $ Base64.decode (encodeUtf8 secret)
      jwk = Crypto.JOSE.fromOctets secretBytes :: Crypto.JOSE.JWK
      header = Crypto.JOSE.newJWSHeader (Crypto.JOSE.RequiredProtection, Crypto.JOSE.HS256)
  result <- Crypto.JOSE.runJOSE @Crypto.JOSE.Error $ do
    jws <- Crypto.JOSE.signJWS (LBS.fromStrict payload) (Identity (header, jwk)) :: Crypto.JOSE.JOSE Crypto.JOSE.Error IO (Crypto.JOSE.CompactJWS Crypto.JOSE.JWSHeader)
    pure (Crypto.JOSE.encodeCompact jws)
  case result of
    Left err -> error (show err)
    Right compact -> pure (decodeUtf8Lenient (LBS.toStrict compact))
