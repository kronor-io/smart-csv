module Kronor.SmartCsv.TokenClaims (
    ParsedTokenClaims (..),
    TokenClaimsError (..),
    HasuraRoleError (..),
    parseTokenClaims,
    selectHasuraRole,
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
            , hasuraClaims = KeyMap.lookup hasuraClaimsKey claims
            , tokenType = KeyMap.lookup "ttype" claims >>= getText
            , tokenId = KeyMap.lookup "tid" claims >>= getText
            }
parseTokenClaims _ = Left TokenClaimsNotObject


data HasuraRoleError
    = HasuraClaimsMissing
    | HasuraRoleNotAllowed
    deriving stock (Eq, Show)


-- | Make the role the default role of the token's Hasura claims, so that the token
-- signed from them for the export runs its queries as that role.  The role must be
-- one of the token's allowed roles.
selectHasuraRole :: Text -> Aeson.Value -> Either HasuraRoleError Aeson.Value
selectHasuraRole role (Aeson.Object claims) =
    case KeyMap.lookup hasuraClaimsKey claims of
        Just (Aeson.Object hasuraClaims)
            | role `elem` allowedRoles hasuraClaims ->
                let selected = KeyMap.insert "x-hasura-default-role" (Aeson.String role) hasuraClaims
                 in Right (Aeson.Object (KeyMap.insert hasuraClaimsKey (Aeson.Object selected) claims))
            | otherwise -> Left HasuraRoleNotAllowed
        _ -> Left HasuraClaimsMissing
  where
    allowedRoles hasuraClaims = case KeyMap.lookup "x-hasura-allowed-roles" hasuraClaims of
        Just (Aeson.Array roles) -> mapMaybe getText (toList roles)
        _ -> []
selectHasuraRole _ _ = Left HasuraClaimsMissing


hasuraClaimsKey :: KeyMap.Key
hasuraClaimsKey = "https://hasura.io/jwt/claims"


getText :: Aeson.Value -> Maybe Text
getText (Aeson.String t) = Just t
getText _ = Nothing
