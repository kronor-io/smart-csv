module Kronor.SmartCsv.TokenClaims (
    ParsedTokenClaims (..),
    TokenClaimsError (..),
    HasuraRoleError (..),
    parseTokenClaims,
    selectHasuraRole,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
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
    | HasuraClaimsNotObject
    | HasuraRoleNotAllowed
    deriving stock (Eq, Show)


-- | Make the role the default and only allowed role of the token's Hasura claims,
-- so that the token signed from them for the export runs its queries as that role
-- and cannot pick another.  The role must be one of the token's allowed roles.
-- Hasura reads claim names case-insensitively, so they are matched that way and
-- written back in lower case.
selectHasuraRole :: Text -> Aeson.Value -> Either HasuraRoleError Aeson.Value
selectHasuraRole role (Aeson.Object claims) =
    case KeyMap.lookup hasuraClaimsKey claims of
        Nothing -> Left HasuraClaimsMissing
        Just (Aeson.Object hasuraClaims)
            | role `elem` allowedRoles hasuraClaims ->
                let selected =
                        KeyMap.insert defaultRoleKey (Aeson.String role)
                            . KeyMap.insert allowedRolesKey (Aeson.toJSON [role])
                            $ KeyMap.filterWithKey (\key _ -> not (isRoleClaim key)) hasuraClaims
                 in Right (Aeson.Object (KeyMap.insert hasuraClaimsKey (Aeson.Object selected) claims))
            | otherwise -> Left HasuraRoleNotAllowed
        Just _ -> Left HasuraClaimsNotObject
  where
    allowedRoles hasuraClaims =
        [ allowed
        | (key, Aeson.Array roles) <- KeyMap.toList hasuraClaims
        , isClaim allowedRolesKey key
        , allowed <- mapMaybe getText (toList roles)
        ]
    isRoleClaim key = isClaim defaultRoleKey key || isClaim allowedRolesKey key
selectHasuraRole _ _ = Left HasuraClaimsMissing


-- | Whether a key names the claim, ignoring case as Hasura does.
isClaim :: KeyMap.Key -> KeyMap.Key -> Bool
isClaim claim key = Text.toLower (Key.toText key) == Key.toText claim


defaultRoleKey :: KeyMap.Key
defaultRoleKey = "x-hasura-default-role"


allowedRolesKey :: KeyMap.Key
allowedRolesKey = "x-hasura-allowed-roles"


hasuraClaimsKey :: KeyMap.Key
hasuraClaimsKey = "https://hasura.io/jwt/claims"


getText :: Aeson.Value -> Maybe Text
getText (Aeson.String t) = Just t
getText _ = Nothing
