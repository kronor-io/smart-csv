module SmartCsvRunner.AWS.S3 (presign) where

import Amazonka qualified as Aws
import Amazonka.S3 qualified as Aws
import Control.Exception.Lens (handling)
import RIO
import RIO.Time (UTCTime)


presign ::
    MonadIO m =>
    Aws.Env ->
    Aws.BucketName ->
    Aws.ObjectKey ->
    UTCTime ->
    Aws.Seconds ->
    m (Either Aws.Error ByteString)
presign awsEnv bucket keyPath time signedUrlExpiryTime = do
    liftIO $
        handling
            Aws._Error
            (return . Left)
            do
                res <- Aws.runResourceT $ do
                    Aws.presignURL
                        awsEnv
                        time
                        signedUrlExpiryTime
                        (Aws.newGetObject bucket keyPath)
                return . Right $ res
