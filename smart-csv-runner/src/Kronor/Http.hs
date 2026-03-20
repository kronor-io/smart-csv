module Kronor.Http
  ( HttpRequestException,
    module Req,
    module Exceptions,
    run,
    postWithHeaders,
  )
where

import Control.Exception.Annotated.UnliftIO qualified as AnnException
import Data.Coerce (coerce)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import GHC.Stack (withFrozenCallStack)
import Kronor.Logger (HasRequestId, RequestId (..), requestId)
import Kronor.Tracer qualified
import Network.HTTP.Client as Exceptions (HttpException (..), HttpExceptionContent (..), RequestBody (..), responseHeaders)
import Network.HTTP.Req as Req hiding (HttpException, runReq)
import Network.HTTP.Req qualified as Req
import RIO

type HttpRequestException = Req.HttpException

run ::
  forall method env m body response scheme.
  ( Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body),
    MonadReader env m,
    HasRequestId env,
    Req.HttpMethod method,
    Req.HttpBody body,
    Req.HttpResponse response,
    Kronor.Tracer.HasTraceSpan env,
    MonadUnliftIO m,
    HasCallStack
  ) =>
  Req.HttpConfig ->
  method ->
  Req.Url scheme ->
  body ->
  Proxy response ->
  Req.Option scheme ->
  m response
run httpConfig met endPoint reqBody resp options = withFrozenCallStack do
  reqId <- asks requestId

  let methodName = decodeUtf8Lenient $ Req.httpMethodName (Proxy @method)
      url = Req.renderUrl endPoint
      (scheme, parts) = Text.breakOn "://" url
      target = Text.drop 3 parts
      host = Text.takeWhile (/= '/') target

  Kronor.Tracer.withClientTrace [i|#{methodName} #{scheme}://#{host}|] do
    Kronor.Tracer.addSpanTags
      [ ("http.request.method", Kronor.Tracer.toAttribute methodName),
        ("http.request.uri", Kronor.Tracer.toAttribute url),
        ("http.request.scheme", Kronor.Tracer.toAttribute scheme),
        ("http.request.host", Kronor.Tracer.toAttribute host),
        ("http.request.target", Kronor.Tracer.toAttribute target),
        ("span.type", "http")
      ]

    env <- ask
    let tracer = env ^. Kronor.Tracer.tracerL
    res <-
      AnnException.checkpoint "HttpRequest"
        . Req.runReq httpConfig
        $ Req.reqCb
          met
          endPoint
          reqBody
          resp
          ( options
              <> Req.header "X-Request-Id" (encodeUtf8 $ coerce @RequestId @Text reqId)
          )
          (Kronor.Tracer.injectTraceHeaders tracer)

    Kronor.Tracer.addSpanTag "http.response.status_code" (Kronor.Tracer.toAttribute $ Req.responseStatusCode res)

    pure res

-- | Convenience wrapper matching the common call pattern in the codebase.
postWithHeaders ::
  ( MonadReader env m,
    HasRequestId env,
    Kronor.Tracer.HasTraceSpan env,
    MonadUnliftIO m,
    Req.HttpBody body,
    Req.HttpResponse response
  ) =>
  Req.Url scheme ->
  body ->
  Proxy response ->
  Req.Option scheme ->
  m response
postWithHeaders = run Req.defaultHttpConfig Req.POST
