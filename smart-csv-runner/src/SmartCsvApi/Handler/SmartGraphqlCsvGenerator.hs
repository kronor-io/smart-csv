{-# OPTIONS_GHC -Wno-orphans #-}

module SmartCsvApi.Handler.SmartGraphqlCsvGenerator
  ( handleSmartGraphqlCsvGenerator,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Aeson.Key qualified as Key
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Pool qualified
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import Kronor.Db (statement)
import Kronor.Db.Models.Shard (shardToInt64)
import RIO
import SmartCsvApi.Auth (verifyBearerToken)
import SmartCsvApi.Db.Statements qualified as Statements
import SmartCsvApi.Env (ApiEnv (..))
import SmartCsvApi.Types.SmartGraphqlCsvGenerator (SmartGraphqlCsvGeneratorInput, SmartGraphqlCsvGeneratorResult (..))
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
        Right tokenClaims -> handleValidated apiEnv tokenClaims input

handleValidated ::
  ApiEnv ->
  Aeson.Value ->
  SmartGraphqlCsvGeneratorInput ->
  IO (Either String SmartGraphqlCsvGeneratorResult)
handleValidated apiEnv tokenClaims input = do
  -- Validate input
  case Val.validateSmartGraphqlCsvGeneratorInput input of
    Left valErr ->
      pure $ Left ("Validation error: " <> valErr)
    Right validated -> do
      let pool = envDbPool apiEnv
          shardId = shardToInt64 (Val.shardId validated)

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
              Val.graphqlQueryBody validated,
              Val.graphqlQueryVariables validated,
              tokenClaims,
              Val.columnConfig validated,
              Val.columnConfigName validated
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
