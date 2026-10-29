{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvApi.Handler.SmartGraphqlCsvGenerator
  ( handleSmartGraphqlCsvGenerator,
    exportTokenClaims,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Pool qualified
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import Kronor.Db (statement)
import Kronor.Db.Models.Shard (shardToInt64)
import Kronor.SmartCsv.TokenClaims (HasuraRoleError (..), selectHasuraRole)
import RIO
import SmartCsvApi.Auth (verifyBearerToken)
import SmartCsvApi.Db.Statements qualified as Statements
import SmartCsvApi.Env (ApiEnv (..))
import SmartCsvApi.Types.SmartGraphqlCsvGenerator (SmartGraphqlCsvGeneratorInput (..), SmartGraphqlCsvGeneratorResult (..))
import SmartCsvApi.Validation.SmartGraphqlCsvGenerator as Val

-- | Handle the smartGraphqlCsvGenerator mutation
handleSmartGraphqlCsvGenerator ::
  ApiEnv ->
  Maybe Text ->
  SmartGraphqlCsvGeneratorInput ->
  IO (Either String SmartGraphqlCsvGeneratorResult)
handleSmartGraphqlCsvGenerator apiEnv mAuthHeader input = do
  -- Authenticate
  case mAuthHeader of
    Nothing -> pure $ Left "Missing Authorization header"
    Just authHeader -> do
      authResult <- verifyBearerToken apiEnv.envJwtSecret authHeader
      case authResult of
        Left err -> pure $ Left ("Authentication failed: " <> Text.unpack err)
        Right tokenClaims -> case exportTokenClaims input.hasuraRole tokenClaims of
          Left err -> pure $ Left ("Authorization failed: " <> err)
          Right exportClaims -> handleValidated apiEnv exportClaims input

-- | The claims the export runs with: the token's own, narrowed to the requested Hasura
-- role (if any).
exportTokenClaims :: Maybe Text -> Aeson.Value -> Either String Aeson.Value
exportTokenClaims Nothing tokenClaims = Right tokenClaims
-- An empty role means the caller's request transform rendered nothing; falling back to
-- the default role would run the export as a role nobody picked.
exportTokenClaims (Just "") _ = Left "hasuraRole must not be empty"
exportTokenClaims (Just role) tokenClaims = first describe (selectHasuraRole role tokenClaims)
  where
    quotedRole = "\"" <> Text.unpack role <> "\""
    describe HasuraClaimsMissing = "token has no Hasura claims to select hasuraRole " <> quotedRole <> " from"
    describe HasuraClaimsNotObject = "token's Hasura claims are not a JSON object, so hasuraRole " <> quotedRole <> " cannot be selected"
    describe HasuraRoleNotAllowed = "hasuraRole " <> quotedRole <> " is not one of the token's allowed roles"

handleValidated ::
  ApiEnv ->
  Aeson.Value ->
  SmartGraphqlCsvGeneratorInput ->
  IO (Either String SmartGraphqlCsvGeneratorResult)
handleValidated apiEnv tokenClaims input = do
  let pool = envDbPool apiEnv

  -- Validate input
  case Val.validateSmartGraphqlCsvGeneratorInput input of
    Left valErr ->
      pure $ Left ("Validation error: " <> valErr)
    Right validated -> do
      let shardId = shardToInt64 (Val.shardId validated)

      -- Generate a request ID for the transaction context
      requestId <- UUID.toText <$> UUID.nextRandom

      -- Write to database
      dbResult <- Hasql.Pool.use pool $ do
        transaction Serializable Write $ do
          statement requestId Statements.setTransactionContext
          statement
            ( shardId,
              Val.recipient validated,
              Key.toText (Val.graphqlPaginationKey validated),
              Val.orderBy validated,
              Val.graphqlQueryBody validated,
              Val.graphqlQueryVariables validated,
              tokenClaims,
              Val.columnConfig validated,
              Val.columnConfigName validated,
              Val.detectNumericColumns validated
            )
            Statements.insertSmartGraphqlCsvGenerator

      case dbResult of
        Left err ->
          pure $ Left ("Database insert failed: " <> show err)
        Right csvId ->
          pure
            $ Right
            $ SmartGraphqlCsvGeneratorResult
              { reportId = csvId
              }
