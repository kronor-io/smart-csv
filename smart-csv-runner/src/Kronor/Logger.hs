module Kronor.Logger
  ( -- * Main
    LoggerCapabilities,
    HasLogEnv (..),

    -- * Request id
    HasRequestId (..),
    RequestId (..),

    -- * Helpers
    jsonLogFunc,
    withJsonLogFunc,
    withAddedContext,
    withAddedNamespace,
    withAddedContextMap,

    -- * Reexports
    Colog.Json.LoggerEnv,
    RIO.logError,
    RIO.logInfo,
  )
where

import Colog.Json (LoggerEnv, addContext, addNamespace, ls, sl)
import Colog.Json qualified as Colog
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import Data.Vector qualified as V
import GHC.Stack (SrcLoc (..), getCallStack)
import RIO
import RIO.Map qualified as Map

-----------------------------------------------------------
-- MAIN ---------------------------------------------------
-----------------------------------------------------------

-- | Pack of constraints for convenience.
type LoggerCapabilities env m =
  ( HasLogEnv env,
    MonadReader env m
  )

class HasLogEnv env where
  logEnvL :: Lens' env LoggerEnv

withJsonLogFunc :: RIO.LogLevel -> ((LoggerEnv -> RIO.LogFunc) -> m a) -> m a
withJsonLogFunc minLevel inner =
  inner $ mkLogFunc . jsonLogFunc minLevel

jsonLogFunc ::
  (MonadIO m) =>
  RIO.LogLevel ->
  LoggerEnv ->
  CallStack ->
  RIO.LogSource ->
  RIO.LogLevel ->
  Utf8Builder ->
  m ()
jsonLogFunc minLevel env cs _src level str =
  when (level >= minLevel) do
    now <- liftIO getCurrentTime
    let newEnv = addContext (sl "meta" (meta now)) env
    dispatchLevel level newEnv $ ls $ textDisplay str
  where
    dispatchLevel LevelDebug = Colog.logDebug
    dispatchLevel LevelInfo = Colog.logInfo
    dispatchLevel LevelWarn = Colog.logWarn
    dispatchLevel LevelError = Colog.logErr
    dispatchLevel (LevelOther _) = Colog.logInfo
    stackToJson (name, SrcLoc {..}) =
      Aeson.String
        $ T.pack
        $ srcLocModule
        ++ "@"
        ++ name
        ++ " ("
        ++ srcLocFile
        ++ ":"
        ++ show srcLocStartLine
        ++ ")"
    csJson = map stackToJson . filter removeSomeLibs $ getCallStack cs
    removeSomeLibs (_, SrcLoc {srcLocFile}) = srcLocFile /= "src/RIO/Prelude/Logger.hs"
    meta time =
      Aeson.object
        [ ("timestamp", Aeson.String $ timeFormatter time),
          ("callstack", Aeson.Array $ V.fromList csJson)
        ]
    timeFormatter = T.pack . formatISO8601Millis

----------
-- REST --
----------

withAddedContext ::
  ( Aeson.ToJSON a,
    LoggerCapabilities env m
  ) =>
  (Text -> a -> m b -> m b)
withAddedContext name obj inner =
  local
    (logEnvL %~ addContext (sl name obj))
    inner

-- | Adds all the key-values from the provided Map as context keys in the logger
withAddedContextMap ::
  (LoggerCapabilities env m) =>
  (Map Text Value -> m b -> m b)
withAddedContextMap contextMap inner = do
  local
    (logEnvL %~ makeNewContext)
    inner
  where
    makeNewContext env = foldl' appendContext env (Map.toList contextMap)
    appendContext !env (name, value) = addContext (sl name value) env

withAddedNamespace ::
  (LoggerCapabilities env m) =>
  (Text -> m a -> m a)
withAddedNamespace ns inner =
  local
    (logEnvL %~ addNamespace ns)
    inner

----------------
-- REQUEST ID --
----------------

class HasRequestId env where
  requestId :: env -> RequestId

newtype RequestId = RequestId Text
  deriving newtype (Show)

instance Semigroup RequestId where
  _ <> b = b

instance Monoid RequestId where
  mempty = RequestId "untagged-request"

instance Aeson.ToJSON RequestId where
  toJSON = Aeson.toJSON . coerce @RequestId @Text
