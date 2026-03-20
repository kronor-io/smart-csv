module Kronor.SmartCsv.TokenClaims (
    ParsedTokenClaims (..),
    TokenClaimsError (..),
    parseTokenClaims,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import RIO


data ParsedTokenClaims
    = ParsedTokenClaims
    { associatedEmail :: Maybe Text
    , hasuraClaims :: Maybe Aeson.Value
    , tokenType :: Maybe Text
    , tokenId :: Maybe Text
    }
    deriving stock (Eq, Show)


data TokenClaimsError
    = TokenClaimsNotObject
    deriving stock (Eq, Show)


parseTokenClaims :: Aeson.Value -> Either TokenClaimsError ParsedTokenClaims
parseTokenClaims (Aeson.Object claims) =
    Right
        ParsedTokenClaims
            { associatedEmail = KeyMap.lookup "associated_email" claims >>= getText
            , hasuraClaims = KeyMap.lookup "https://hasura.io/jwt/claims" claims
            , tokenType = KeyMap.lookup "ttype" claims >>= getText
            , tokenId = KeyMap.lookup "tid" claims >>= getText
            }
parseTokenClaims _ = Left TokenClaimsNotObject


getText :: Aeson.Value -> Maybe Text
getText (Aeson.String t) = Just t
getText _ = Nothing
