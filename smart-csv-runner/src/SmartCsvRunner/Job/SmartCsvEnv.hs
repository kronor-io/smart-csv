module SmartCsvRunner.Job.SmartCsvEnv (
    SmartCsvEnv (..),
    mkSmartCsvEnv,
) where

import RIO
import SmartCsvRunner.AWS.Types (HasS3Config (..), S3Config)
import SmartCsvRunner.Env (Options)
import SmartCsvRunner.JobHandlers.Email qualified as Email


data SmartCsvEnv = SmartCsvEnv
    { smartCsvS3Config :: S3Config
    , smartCsvOptions :: Options
    , smartCsvEmailServer :: Email.EmailServer
    }


mkSmartCsvEnv :: S3Config -> Options -> Email.EmailServer -> SmartCsvEnv
mkSmartCsvEnv = SmartCsvEnv


instance HasS3Config SmartCsvEnv where
    getS3ConfigL = lens smartCsvS3Config (\env cfg -> env{smartCsvS3Config = cfg})


instance Email.HasEmailServer SmartCsvEnv where
    getEmailServer = smartCsvEmailServer
