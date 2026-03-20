module SmartCsvApi.Env (
    ApiEnv (..),
    ApiOptions (..),
) where

import Hasql.Pool (Pool)
import Network.HTTP.Client (Manager)
import RIO


-- | Options for the API server
data ApiOptions = ApiOptions
    { apiHost :: String
    , apiPort :: Int
    , graphqlUrl :: Text
    , portalUrl :: Text
    , jwtSecret :: Text
    , logLevel :: Text
    }
    deriving stock (Generic)


-- | Runtime environment for the API server
data ApiEnv = ApiEnv
    { envDbPool :: Pool
    , envHttpManager :: Manager
    , envGraphqlUrl :: Text
    , envPortalUrl :: Text
    , envJwtSecret :: Text
    }
    deriving stock (Generic)
