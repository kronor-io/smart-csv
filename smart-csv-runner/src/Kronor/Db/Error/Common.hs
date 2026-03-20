module Kronor.Db.Error.Common (
    clarifyResult,
    clarifyError,
    Failure (..),
) where

import Hasql.Pool qualified
import RIO


data Failure
    = CONSISTENCY_FAILURE Text
    | TEMPORARY_FAILURE Text


clarifyResult :: Monad m => Text -> Either Hasql.Pool.UsageError a -> m (Either Text a)
clarifyResult _ result = pure $ first (utf8BuilderToText . displayShow) result


clarifyError :: Monad m => Text -> Hasql.Pool.UsageError -> m Text
clarifyError _ err = pure (utf8BuilderToText $ displayShow err)
