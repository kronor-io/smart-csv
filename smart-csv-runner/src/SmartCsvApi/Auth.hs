module SmartCsvApi.Auth (signJwtFromClaims, verifyBearerToken) where

import Control.Monad.Except (Except, runExcept, runExceptT)
import Crypto.JOSE (Error)
import Crypto.JOSE qualified as JOSE
import Crypto.JWT (JWTError, SignedJWT, decodeCompact, defaultJWTValidationSettings, verifyClaims)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import RIO


signJwtFromClaims :: Text -> Maybe Text -> Maybe Aeson.Value -> Maybe Text -> Maybe Text -> IO (Either Text Text)
signJwtFromClaims jwtSecret associatedEmail hasuraClaims tokenType tokenId = do
    issuedAt <- floor <$> getPOSIXTime
    case prepareSecret jwtSecret of
        Left err -> pure (Left err)
        Right jwk -> do
            let header = JOSE.newJWSHeader (JOSE.RequiredProtection, JOSE.HS256)
                payload =
                    Aeson.object
                        [ "https://hasura.io/jwt/claims" Aeson..= hasuraClaims
                        , "iat" Aeson..= issuedAt
                        , "exp" Aeson..= (issuedAt + (3600 :: Int))
                        , "tid" Aeson..= tokenId
                        , "ttype" Aeson..= tokenType
                        , "tname" Aeson..= (Nothing :: Maybe Text)
                        , "associated_email" Aeson..= associatedEmail
                        ]
            result <- JOSE.runJOSE @JOSE.Error do
                jws <- JOSE.signJWS (Aeson.encode payload) (Identity (header, jwk)) :: JOSE.JOSE JOSE.Error IO (JOSE.CompactJWS JOSE.JWSHeader)
                pure (JOSE.encodeCompact jws)
            pure $ case result of
                Left err -> Left (tshow err)
                Right compact -> Right (decodeUtf8Lenient (LBS.toStrict compact))


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
    prepare :: Either Text (JOSE.JWK, SignedJWT)
    prepare = do
        rawToken <- stripBearerPrefix authHeader
        jwk <- prepareSecret jwtSecret
        jwt <- first tshow $ runExcept (decodeCompact (LBS.fromStrict (encodeUtf8 rawToken)) :: Except Error SignedJWT)
        Right (jwk, jwt)


prepareSecret :: Text -> Either Text JOSE.JWK
prepareSecret jwtSecret = do
    secretBytes <- first tshow $ Base64.decode (encodeUtf8 jwtSecret)
    Right (JOSE.fromOctets secretBytes :: JOSE.JWK)


stripBearerPrefix :: Text -> Either Text Text
stripBearerPrefix header =
    case Text.stripPrefix "Bearer " header of
        Just token -> Right token
        Nothing -> Left "Authorization header must start with 'Bearer '"
