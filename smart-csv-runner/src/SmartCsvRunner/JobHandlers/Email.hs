{-# OPTIONS_GHC -Wno-partial-fields #-}

module SmartCsvRunner.JobHandlers.Email (
    EmailBuilder {- No need to export it,its gonna come as JSON from the DB -},
    EmailServer {- We export this so we can decide between prod and dev -} (..),
    HasEmailServer (..),
    SendEmail (..),
) where

import Amazonka.SES hiding (SendEmail)
import Data.Aeson qualified as Aeson
import Data.Function
import Data.String.Interpolate (iii)
import Data.UUID (UUID)
import Data.UUID qualified as Uuid
import Hasql.Pool (UsageError)
import Hasql.Statement qualified
import Hasql.TH
import Kronor.Db qualified
import Kronor.Tracer qualified
import Network.Mail.Mime qualified as Email
import Network.Mail.SMTP qualified as SMTP
import Network.Socket qualified as Socket
import RIO hiding (to)
import RIO.Text qualified as T
import RIO.Text.Lazy qualified as LT
import SmartCsvRunner.AWS
import SmartCsvRunner.Job (Job, JobEnv (jobEnv))
import SmartCsvRunner.Job.Type (JobProcessor (..), subJobEnv)


----------
-- MAIN --
----------

-- | The payload we read from the database.
data EmailBuilder = EmailBuilder
    { email_id :: UUID
    , to :: (Maybe Text {- name -}, Text {- email -})
    , from :: (Maybe Text {- name -}, Text {- email -})
    , subject :: Text
    , body :: Text
    }
    deriving stock (Generic)
    deriving anyclass (Aeson.FromJSON)


instance Kronor.Tracer.HasTraceTags EmailBuilder where
    getTraceTags payload =
        [ ("email.id", Kronor.Tracer.toAttribute $ Uuid.toText payload.email_id)
        ]
newtype SendEmail = SendEmail EmailBuilder
    deriving stock (Generic)


instance HasEmailServer env => JobProcessor env SendEmail where
    processJob (SendEmail payload) = subJobEnv getEmailServer do
        sendEmail payload >>= \case
            Left e -> logError (displayShow e)
            Right{} -> return ()


    closeJob _ = pure ()


data EmailServer
    = EmailServerDev
        { emailServerHost :: Socket.HostName
        , emailServerPort :: Socket.PortNumber
        }
    | EmailServerProdSMTP
        { emailServerHost :: Socket.HostName
        , emailUserName :: Text
        , emailPassword :: Text
        }
    | EmailServerProdSES


class HasEmailServer env where
    getEmailServer :: env -> EmailServer


-- | Sends the email case its unique.
sendEmail :: EmailBuilder -> Job EmailServer (Either UsageError ())
sendEmail EmailBuilder{..} = do
    emailSettings <- jobEnv <$> ask
    logDebugS "kronor-worker:Worker.sendEmail" [iii|Sending email to #{snd to} with subject #{subject} and body:\n#{body}|]
    withEmailId email_id $ do
        case emailSettings of
            EmailServerDev{..} ->
                catch
                    (liftIO $ SMTP.sendMail' emailServerHost emailServerPort mail >> return Success)
                    ( \(err :: SomeException) -> do
                        logErrorS "kronor-worker:Worker.sendEmail" ("Failed to send smtp dev mail: " <> displayShow err)
                        return Error
                    )
            EmailServerProdSMTP{..} ->
                catch
                    ( do
                        liftIO $
                            SMTP.sendMailWithLoginSTARTTLS
                                emailServerHost
                                (T.unpack emailUserName)
                                (T.unpack emailPassword)
                                mail
                        return Success
                    )
                    ( \(err :: SomeException) -> do
                        logErrorS "kronor-worker:Worker.sendEmail" ("Failed to send smtp mail: " <> displayShow err)
                        return Error
                    )
            EmailServerProdSES -> do
                sendAWSmail subject from to body
                return Success
  where
    mail =
        Email.addPart
            parts
            (Email.emptyMail mailFrom)
                { Email.mailTo = [mailTo]
                , Email.mailHeaders = headers
                }
    parts = [Email.htmlPart $ LT.fromStrict body]
    headers = [("Subject", subject)]
    mailFrom = uncurry SMTP.Address from
    mailTo = uncurry SMTP.Address to


sendAWSmail ::
    HasCallStack =>
    Text ->
    (Maybe Text, Text) ->
    (Maybe Text, Text) ->
    Text ->
    Job EmailServer ()
sendAWSmail subject (_, from) (_, to) body = do
    eresult <- sesSend from to subject (Just body) Nothing
    case eresult of
        Left err ->
            logErrorS "kronor-worker:Worker.sendAWSmail" ("Failed to send ses mail: " <> displayShow err)
        Right (SendEmailResponse' httpStatus messageId) -> do
            logGeneric
                "kronor-worker:Worker.sendAWSmail"
                LevelInfo
                ("Send ses mail(" <> displayShow httpStatus <> "): " <> displayShow messageId)


-------------
-- PRIVATE --
-------------

data EmailActionResult = Success | Error


-- | If the continuation fails no hash is written to disk.
withEmailId ::
    UUID ->
    RIO (JobEnv w) EmailActionResult ->
    RIO (JobEnv w) (Either UsageError ())
withEmailId emailId cont = do
    checkUniqueness emailId $ \case
        Left e -> return (Left e)
        Right ("created", "sending", attempts) -> do
            res <- cont
            updateEmailRegistry res attempts emailId
        Right ("selected", "error", attempts) ->
            if attempts < 5
                then do
                    res <- cont
                    updateEmailRegistry res attempts emailId
                else return $ Right ()
        Right ("selected", _, _) ->
            return $ Right ()
        Right _ ->
            return $ Right ()


updateEmailRegistry :: EmailActionResult -> Int32 -> UUID -> RIO (JobEnv w) (Either UsageError ())
updateEmailRegistry Success attempts mailId =
    Kronor.Db.write $ Kronor.Db.statement ("done", attempts + 1, mailId) updateEmailRegistryStatement
updateEmailRegistry Error attempts mailId =
    Kronor.Db.write $ Kronor.Db.statement ("error", attempts + 1, mailId) updateEmailRegistryStatement


updateEmailRegistryStatement :: Hasql.Statement.Statement (Text, Int32, UUID) ()
updateEmailRegistryStatement =
    [resultlessStatement|
            update smart_csv.email_registry set
                status = $1::text::smart_csv.email_registry_status,
                amount_attempted = $2::int
            where id = $3::uuid
        |]


checkUniqueness ::
    UUID ->
    (Either UsageError (Text, Text, Int32) -> RIO (JobEnv w) a) ->
    RIO (JobEnv w) a
checkUniqueness emailId cont = do
    res <-
        Kronor.Db.write $
            Kronor.Db.statement
                emailId
                [singletonStatement|
                with cte as (
                    insert into smart_csv.email_registry
                        (id, status)
                    values
                        ($1::uuid, 'sending')
                    on conflict do nothing
                    returning status::text, amount_attempted::int
                )
                select 'created'::text, status::text, amount_attempted::int
                from cte
                union all
                select 'selected'::text, status::text, amount_attempted::int
                from smart_csv.email_registry
                where id = $1::uuid
                |]
    cont res
