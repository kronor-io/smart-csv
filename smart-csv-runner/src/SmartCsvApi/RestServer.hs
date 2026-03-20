{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SmartCsvApi.RestServer
  ( startRestApiServer,
    mkApplication,
    mkApplicationWith,
  )
where

import Data.Aeson qualified as Aeson
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import RIO hiding (Handler)
import Servant
import SmartCsvApi.Env (ApiEnv (..), ApiOptions (..))
import SmartCsvApi.Handler.SmartGraphqlCsvGenerator (handleSmartGraphqlCsvGenerator)
import SmartCsvApi.Types.SmartGraphqlCsvGenerator
  ( SmartGraphqlCsvGeneratorInput (..),
    SmartGraphqlCsvGeneratorResult (..),
  )

-- | REST API type definition
type SmartCsvAPI =
  "health" :> Get '[PlainText] Text
    :<|> "api" :> "v1" :> "csv" :> "generate" :> Header "Authorization" Text :> ReqBody '[JSON] SmartGraphqlCsvGeneratorInput :> Post '[JSON] SmartGraphqlCsvGeneratorResult

-- | Start the REST API server with OpenTelemetry tracing middleware.
startRestApiServer :: ApiEnv -> ApiOptions -> IO ()
startRestApiServer apiEnv opts = do
  otelMiddleware <- newOpenTelemetryWaiMiddleware
  Warp.run opts.apiPort (otelMiddleware $ mkApplication apiEnv)

-- | Build a WAI application for the Smart CSV API.
mkApplication :: ApiEnv -> Application
mkApplication apiEnv = mkApplicationWith (handleSmartGraphqlCsvGenerator apiEnv)

-- | Build a WAI application with an injected CSV generation handler.
mkApplicationWith :: (Maybe Text -> SmartGraphqlCsvGeneratorInput -> IO (Either String SmartGraphqlCsvGeneratorResult)) -> Application
mkApplicationWith generateHandler = serve (Proxy :: Proxy SmartCsvAPI) (server generateHandler)

-- | Server implementation
server :: (Maybe Text -> SmartGraphqlCsvGeneratorInput -> IO (Either String SmartGraphqlCsvGeneratorResult)) -> Server SmartCsvAPI
server generateHandler =
  healthCheck :<|> generateCsvHandler generateHandler

-- | Health check endpoint
healthCheck :: Handler Text
healthCheck = pure "OK"

-- | CSV generation endpoint
generateCsvHandler :: (Maybe Text -> SmartGraphqlCsvGeneratorInput -> IO (Either String SmartGraphqlCsvGeneratorResult)) -> Maybe Text -> SmartGraphqlCsvGeneratorInput -> Handler SmartGraphqlCsvGeneratorResult
generateCsvHandler generateHandler mAuthHeader input = do
  result <- liftIO $ generateHandler mAuthHeader input
  case result of
    Left err ->
      throwError err400 {errBody = Aeson.encode $ Aeson.object ["error" Aeson..= err]}
    Right res -> pure res
