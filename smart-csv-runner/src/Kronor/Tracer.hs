{-# OPTIONS_GHC -Wno-orphans #-}

module Kronor.Tracer
  ( module OpenTelemetry,
    HasTraceSpan (..),
    HasTraceTags (..),
    RemoteContext,
    extractRemoteContext,
    addSpanTag,
    addTopSpanTag,
    addSpanTags,
    addTopSpanTags,
    markSpanAsError,
    setSpanName,
    withTrace,
    withConsumerTrace,
    withClientTrace,
    getLogAttributes,
    convertToLogAttributes,
    withGlobalTracer,
    injectTraceHeaders,
    encodeSpanRemoteContext,
    OpenTelemetry.Context.lookupSpan,
  )
where

import Data.Aeson qualified as Aeson
import Data.Char qualified
import Data.HashMap.Strict qualified as HashMap
import GHC.Stack (SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import Network.HTTP.Client qualified
import OpenTelemetry.Common as OpenTelemetry
import OpenTelemetry.Context qualified
import OpenTelemetry.Context.ThreadLocal (attachContext, getContext)
import OpenTelemetry.Exporter.OTLP.Span ()
import OpenTelemetry.Propagator qualified as Propagator
import OpenTelemetry.Propagator.W3CTraceContext (encodeSpanContext)
import OpenTelemetry.Resource (Resource, ResourceSchema, ToResource (..), materializeResources, mkResource, toResource, (.=?))
import OpenTelemetry.Resource.Service.Detector (detectService)
import OpenTelemetry.Trace as OpenTelemetry
import OpenTelemetry.Trace.Core as OpenTelemetry (getSpanContext, getTracerProviderPropagators, getTracerTracerProvider)
import OpenTelemetry.Trace.Id as OpenTelemetry (Base (..), spanIdBaseEncodedText, traceIdBaseEncodedText)
import RIO
import RIO.Map qualified as Map
import RIO.Text qualified as T
import RIO.Vector qualified as V
import System.Environment (lookupEnv)

class (HasTracer s) => HasTraceSpan s where
  topTraceSpanL :: Lens' s (Maybe Span)
  traceSpanL :: Lens' s (Maybe Span)

-- | Extracts span tags from a given type that can be used for logging and
-- tracing
class HasTraceTags input where
  getTraceTags :: input -> [(Text, Attribute)]

-- | The simplest function for annotating code with trace information.
withTrace ::
  (HasTraceSpan env) =>
  (MonadUnliftIO m) =>
  (MonadReader env m) =>
  (HasCallStack) =>
  Text ->
  m a ->
  m a
withTrace name action = withFrozenCallStack do
  env <- ask
  let tracer = env ^. tracerL
  let topSpan = env ^. topTraceSpanL

  inSpan' tracer name defaultSpanArguments \sp -> do
    let newEnv old =
          old
            & topTraceSpanL
            .~ (topSpan <|> Just sp)
              & traceSpanL
            .~ Just sp
    local newEnv action

-- | Like 'withTrace', but sets the 'SpanKind' to 'Client'.
withClientTrace ::
  (HasTraceSpan env) =>
  (MonadUnliftIO m) =>
  (MonadReader env m) =>
  (HasCallStack) =>
  Text ->
  m a ->
  m a
withClientTrace name action = withFrozenCallStack do
  env <- ask
  let tracer = env ^. tracerL
  let topSpan = env ^. topTraceSpanL

  inSpan' tracer name defaultSpanArguments {kind = Client} \sp -> do
    let newEnv old =
          old
            & topTraceSpanL
            .~ (topSpan <|> Just sp)
              & traceSpanL
            .~ Just sp
    local newEnv action

-- | Like 'withTrace', but sets the 'SpanKind' to 'Consumer'.
withConsumerTrace ::
  (HasTraceSpan env) =>
  (MonadUnliftIO m) =>
  (MonadReader env m) =>
  (HasCallStack) =>
  Text ->
  RemoteContext ->
  m a ->
  m a
withConsumerTrace name remoteContext action = withFrozenCallStack do
  env <- ask
  let tracer = env ^. tracerL
  let topSpan = env ^. topTraceSpanL
  let propagator = getTracerProviderPropagators $ getTracerTracerProvider tracer

  currentContext <- getContext
  decodedContext <-
    Propagator.extract
      propagator
      do
        catMaybes
          [ fmap ("traceparent",) remoteContext.remoteParent,
            fmap ("tracestate",) remoteContext.remoteState
          ]
      currentContext
  void $ attachContext decodedContext

  inSpan' tracer name defaultSpanArguments {kind = Consumer} \sp -> do
    let newEnv old =
          old
            & topTraceSpanL
            .~ (topSpan <|> Just sp)
              & traceSpanL
            .~ Just sp
    local newEnv action

addSpanTag ::
  (HasTraceSpan env) =>
  (MonadReader env m) =>
  (MonadIO m) =>
  (ToAttribute a) =>
  Text ->
  a ->
  m ()
addSpanTag tagKey value = do
  env <- ask
  case env ^. traceSpanL of
    Nothing -> pure ()
    Just s -> addAttribute s tagKey value

addTopSpanTag ::
  (HasTraceSpan env) =>
  (MonadReader env m) =>
  (MonadIO m) =>
  (ToAttribute a) =>
  Text ->
  a ->
  m ()
addTopSpanTag tagKey value = do
  env <- ask
  case env ^. topTraceSpanL of
    Nothing -> pure ()
    Just s -> addAttribute s tagKey value

addSpanTags ::
  (HasTraceSpan env) =>
  (MonadReader env m) =>
  (MonadIO m) =>
  [(Text, Attribute)] ->
  m ()
addSpanTags attributes = do
  env <- ask
  case env ^. traceSpanL of
    Nothing -> pure ()
    Just s -> addAttributes s (HashMap.fromList attributes)

addTopSpanTags ::
  (HasTraceSpan env) =>
  (MonadReader env m) =>
  (MonadIO m) =>
  [(Text, Attribute)] ->
  m ()
addTopSpanTags attributes = do
  env <- ask
  case env ^. topTraceSpanL of
    Nothing -> pure ()
    Just s -> addAttributes s (HashMap.fromList attributes)

markSpanAsError ::
  (HasTraceSpan env) =>
  (MonadReader env m) =>
  (MonadIO m) =>
  Text ->
  m ()
markSpanAsError message = withFrozenCallStack do
  env <- ask
  case env ^. traceSpanL of
    Nothing -> pure ()
    Just s -> do
      setStatus s (Error message)
      addAttribute s "exception.stacktrace" prettyStack
  where
    prettyStack = T.unlines $ map frameToText (getCallStack callStack)
    frameToText :: (String, SrcLoc) -> Text
    frameToText (frameName, SrcLoc {..}) =
      utf8BuilderToText
        $ fromString srcLocModule
        <> "@"
        <> fromString frameName
        <> " ("
        <> fromString srcLocPackage
        <> "/"
        <> fromString srcLocFile
        <> ":"
        <> displayShow srcLocStartLine
        <> ")"

setSpanName ::
  (HasTraceSpan env) =>
  (MonadReader env m) =>
  (MonadIO m) =>
  Text ->
  m ()
setSpanName spanName = do
  env <- ask
  case env ^. traceSpanL of
    Nothing -> pure ()
    Just s -> updateName s spanName

-- | A record for storing extracted trace context.
data RemoteContext = RemoteContext
  { remoteParent :: Maybe ByteString,
    remoteState :: Maybe ByteString
  }

-- | Extract trace context from the given headers.
extractRemoteContext :: Map Text Aeson.Value -> RemoteContext
extractRemoteContext headers =
  RemoteContext
    { remoteParent = extractHeader "traceparent",
      remoteState = extractHeader "tracestate"
    }
  where
    extractHeader :: Text -> Maybe ByteString
    extractHeader header = do
      Aeson.String value <- Map.lookup header headers
      Just $ encodeUtf8 value

-- | Get a map of log attributes for correlating logs with traces.
getLogAttributes ::
  (HasTraceSpan env, MonadIO m) =>
  (MonadReader env m) =>
  m (Map Text Aeson.Value)
getLogAttributes = do
  mSpan <- asks (^. traceSpanL)
  case mSpan of
    Just sp -> do
      ctxt <- getSpanContext sp
      let tId = hexToDec (T.takeEnd 16 (traceIdBaseEncodedText Base16 ctxt.traceId))
      let sId = hexToDec (spanIdBaseEncodedText Base16 ctxt.spanId)
      return
        $ Map.fromList
          [ ("trace_id", Aeson.String (tshow tId)),
            ("span_id", Aeson.String (tshow sId))
          ]
    Nothing ->
      return
        $ Map.fromList
          [ ("trace_id", Aeson.String "0"),
            ("span_id", Aeson.String "0")
          ]
  where
    hexToDec :: Text -> Integer
    hexToDec = foldr (\c s -> s * 16 + c) 0 . reverse . map (toInteger . Data.Char.digitToInt) . T.unpack

convertToLogAttributes :: [(Text, Attribute)] -> Map Text Aeson.Value
convertToLogAttributes = Map.fromList . map (second convert)
  where
    convert = \case
      AttributeArray a -> Aeson.toJSON (map convertPrimitive a)
      AttributeValue p -> convertPrimitive p

    convertPrimitive = \case
      TextAttribute t -> Aeson.String t
      IntAttribute i -> Aeson.toJSON i
      BoolAttribute b -> Aeson.Bool b
      DoubleAttribute d -> Aeson.toJSON d

instance (ToPrimitiveAttribute a) => ToAttribute (Vector a) where
  toAttribute = AttributeArray . V.toList . V.map toPrimitiveAttribute

-- | Initialize the OpenTelemetry SDK, create a global tracer provider,
-- and run the given action with a Tracer. Shuts down the provider on exit.
withGlobalTracer :: InstrumentationLibrary -> (Tracer -> IO c) -> IO c
withGlobalTracer name f =
  bracket
    do
      (processors, options) <- getTracerProviderInitializationOptions

      serviceResource <- detectService
      ddTags <- detectDatadog

      let opts =
            emptyTracerProviderOptions
              { tracerProviderOptionsIdGenerator = options.tracerProviderOptionsIdGenerator,
                tracerProviderOptionsSampler = options.tracerProviderOptionsSampler,
                tracerProviderOptionsAttributeLimits = options.tracerProviderOptionsAttributeLimits,
                tracerProviderOptionsSpanLimits = options.tracerProviderOptionsSpanLimits,
                tracerProviderOptionsPropagators = options.tracerProviderOptionsPropagators,
                tracerProviderOptionsResources = materializeResources do
                  toResource serviceResource <> toResource ddTags
              }

      provider <- createTracerProvider processors opts
      setGlobalTracerProvider provider
      return provider
    shutdownTracerProvider
    (\provider -> f (makeTracer provider name tracerOptions))

-- | Encode the current span's trace context as a JSON value
-- suitable for storing in the database (for distributed tracing across services).
encodeSpanRemoteContext :: Span -> IO Aeson.Value
encodeSpanRemoteContext originSpan = do
  (parent, state) <- encodeSpanContext originSpan
  return
    $ Aeson.object
      [ "traceparent" Aeson..= decodeUtf8Lenient parent,
        "tracestate" Aeson..= decodeUtf8Lenient state
      ]

-- | Inject trace headers into an HTTP request for distributed tracing.
injectTraceHeaders ::
  (MonadIO m) =>
  Tracer ->
  Network.HTTP.Client.Request ->
  m Network.HTTP.Client.Request
injectTraceHeaders tracer req = do
  let propagator = getTracerProviderPropagators $ getTracerTracerProvider tracer
  context <- getContext
  headers <- Propagator.inject propagator context (Network.HTTP.Client.requestHeaders req)
  return req {Network.HTTP.Client.requestHeaders = headers}

data DatadogTags = DatadogTags
  { ddEnv :: Maybe Text,
    ddVersion :: Maybe Text,
    ddService :: Maybe Text
  }

-- | Get the datadog tags from the environment
detectDatadog :: IO DatadogTags
detectDatadog = do
  ddEnv <- fmap T.pack <$> lookupEnv "DD_ENV"
  ddVersion <- fmap T.pack <$> lookupEnv "DD_VERSION"
  ddService <- fmap T.pack <$> lookupEnv "DD_SERVICE"
  return DatadogTags {..}

instance ToResource DatadogTags where
  type ResourceSchema DatadogTags = 'Nothing
  toResource :: DatadogTags -> Resource (ResourceSchema DatadogTags)
  toResource dd =
    mkResource
      [ "env" .=? dd.ddEnv,
        "version" .=? dd.ddVersion,
        "service.name" .=? dd.ddService,
        "service.version" .=? dd.ddVersion
      ]
