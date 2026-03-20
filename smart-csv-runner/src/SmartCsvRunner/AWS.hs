module SmartCsvRunner.AWS
  ( HasAwsEnv (..),
    AwsCredsFrom (..),
    fetchAwsEnv,
    sesSend,
    readAwsEnv,
    initAwsEnv,
    Aws.Env,
  )
where

import Amazonka qualified as Aws
import Amazonka.Auth qualified as Aws
import Amazonka.SES qualified as Aws
import Amazonka.SES.Types.Body qualified as Aws.SES.Body
import Amazonka.SES.Types.Destination qualified as Aws.SES.Destination
import Control.Exception.Lens (handling)
import Data.ByteString.Builder (toLazyByteString)
import RIO
import RIO.Process qualified as Process

class HasAwsEnv env where
  getAwsEnvL :: Lens' env Aws.Env

instance HasAwsEnv Aws.Env where
  getAwsEnvL = lens id (\_ newAwsEnv -> newAwsEnv)

readAwsEnv :: (MonadReader env m, HasAwsEnv env) => m Aws.Env
readAwsEnv = (^. getAwsEnvL) <$> ask

data AwsCredsFrom
  = FromEnvVars
  | FromDefaultInstanceProfile
  | FromContainerEnv
  | FromNamedInstanceProfile Text
  deriving stock (Show)

fetchAwsEnv :: (MonadReader env m, HasLogFunc env, MonadIO m) => AwsCredsFrom -> m (Either Aws.AuthError Aws.Env)
fetchAwsEnv envFrom = do
  env <- ask
  liftIO
    $ handling
      Aws._AuthError
      (return . Left)
      do
        awsEnv <- Aws.newEnv $ case envFrom of
          FromEnvVars -> Aws.fromKeysEnv
          FromDefaultInstanceProfile -> Aws.fromDefaultInstanceProfile
          FromNamedInstanceProfile t -> Aws.fromNamedInstanceProfile t
          FromContainerEnv -> Aws.fromContainerEnv
        return $ Right $ awsEnv {Aws.logger = makeAwsLogger env, Aws.region = Aws.Stockholm}

makeAwsLogger ::
  (HasLogFunc env) =>
  env ->
  Aws.LogLevel ->
  Builder ->
  IO ()
makeAwsLogger env level message = runRIO env do
  case level of
    Aws.Debug -> pure ()
    Aws.Info -> logInfo (displayBytesUtf8 (toStrictBytes (toLazyByteString message)))
    Aws.Error -> logError (displayBytesUtf8 (toStrictBytes (toLazyByteString message)))
    Aws.Trace -> pure ()

sesSend ::
  (MonadReader env m, HasAwsEnv env, MonadIO m) =>
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  m (Either Aws.Error Aws.SendEmailResponse)
sesSend fromEmail destination summary bodyHtml bodyText = do
  awsEnv <- readAwsEnv
  liftIO
    $ handling
      Aws._Error
      (return . Left)
      do
        res <- Aws.runResourceT do
          Aws.send
            awsEnv
            ( Aws.newSendEmail
                fromEmail
                (Aws.SES.Destination.newDestination {Aws.SES.Destination.toAddresses = Just [destination]})
                (Aws.newMessage (Aws.newContent summary) mkBody)
            )
        return . Right $ res
  where
    mkBody' = Aws.newBody & Aws.SES.Body.body_text .~ fmap Aws.newContent bodyText
    mkBody = mkBody' & Aws.SES.Body.body_html .~ fmap Aws.newContent bodyHtml

initAwsEnv ::
  ( HasLogFunc env,
    Process.HasProcessContext env
  ) =>
  RIO env Aws.Env
initAwsEnv = do
  mAwsFrom <- Process.lookupEnvFromContext "KRONOR_AWS_FROM"
  let awsFrom =
        maybe
          FromDefaultInstanceProfile
          ( \t -> case t of
              "env" -> FromEnvVars
              "ecs" -> FromContainerEnv
              _ -> FromNamedInstanceProfile t
          )
          mAwsFrom
  logInfoS "smart-csv-runner:AWS" $ "Using aws credentials from: " <> displayShow awsFrom

  eAwsEnv <- fetchAwsEnv awsFrom
  case eAwsEnv of
    Left e -> do
      logError $ displayShow ("AWS Auth Error: " <> show e)
      exitFailure
    Right awsenv ->
      pure awsenv
