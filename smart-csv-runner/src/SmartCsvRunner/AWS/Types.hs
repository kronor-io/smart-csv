module SmartCsvRunner.AWS.Types (
    S3Config (..),
    HasS3Config (..),
) where

import Amazonka qualified as Aws
import Amazonka.S3 qualified as Aws
import RIO
import SmartCsvRunner.AWS (HasAwsEnv)


type PresignAwsEnvGetterFunction = forall env m. (HasLogFunc env, MonadReader env m, HasAwsEnv env, MonadIO m) => m Aws.Env


data S3Config = S3Config
    { bucket :: Aws.BucketName
    , signedUrlExpiryTime :: Aws.Seconds
    , presignAwsEnv :: PresignAwsEnvGetterFunction
    , presignUserName :: Text
    , presignUserSecretId :: Text
    }


class HasS3Config env where
    getS3ConfigL :: Lens' env S3Config


instance HasS3Config S3Config where
    getS3ConfigL = lens id const
