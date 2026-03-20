module SmartCsvApi.Auth (verifyBearerToken) where

import Control.Monad.Except (Except, runExcept, runExceptT)
import Crypto.JOSE (Error, JWK, fromOctets)
import Crypto.JWT (JWTError, SignedJWT, decodeCompact, defaultJWTValidationSettings, verifyClaims)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import RIO


-- | Verify a Bearer JWT token using a base64-encoded HS256 secret and return
-- the claims payload. Uses the jose library for cryptographic verification.
verifyBearerToken :: Text -> Text -> IO (Either Text Aeson.Value)
verifyBearerToken jwtSecret authHeader = do
    case prepare of
        Left err -> pure (Left err)
        Right (jwk, jwt) -> do
            let settings = defaultJWTValidationSettings (const True)
            result <- runExceptT (verifyClaims settings jwk jwt)
            pure $ case result of
                Left (err :: JWTError) -> Left (tshow err)
                Right claims -> Right (Aeson.toJSON claims)
  where
    prepare :: Either Text (JWK, SignedJWT)
    prepare = do
        rawToken <- stripBearerPrefix authHeader
        secretBytes <- first tshow $ Base64.decode (encodeUtf8 jwtSecret)
        let jwk = fromOctets secretBytes :: JWK
        jwt <- first tshow $ runExcept (decodeCompact (LBS.fromStrict (encodeUtf8 rawToken)) :: Except Error SignedJWT)
        Right (jwk, jwt)


stripBearerPrefix :: Text -> Either Text Text
stripBearerPrefix header =
    case Text.stripPrefix "Bearer " header of
        Just token -> Right token
        Nothing -> Left "Authorization header must start with 'Bearer '"
