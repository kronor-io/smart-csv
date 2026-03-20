{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Kronor.Db
  ( HasPgPool (..),
    HasNamedPool (..),
    Hasql.Pool.Pool,
    Hasql.Transaction.Transaction,
    UsageError (..),
    write,
    writeOr,
    read,
    readOr,
    readOnNamedPool,
    makeUnprepared,
    statement,
  )
where

import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import GHC.TypeLits
import Hasql.Pool
import Hasql.Statement qualified
import Hasql.TH (resultlessStatement)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import Kronor.Logger (HasRequestId, RequestId (..), requestId)
import Kronor.Tracer qualified
import RIO

class HasPgPool env where
  getPgPoolL :: Lens' env Pool

instance HasPgPool Pool where
  getPgPoolL = lens id (\_ x -> x)

class (KnownSymbol poolName) => HasNamedPool poolName env where
  getPgNamedPoolL :: Proxy poolName -> Lens' env Pool

makeUnprepared :: Hasql.Statement.Statement a b -> Hasql.Statement.Statement a b
makeUnprepared (Hasql.Statement.Statement stmt params results _) =
  Hasql.Statement.Statement stmt params results False

statement :: a -> Hasql.Statement.Statement a b -> Hasql.Transaction.Transaction b
statement input = Hasql.Transaction.statement input . makeUnprepared

write ::
  (MonadReader env m) =>
  (MonadUnliftIO m) =>
  (HasPgPool env) =>
  (HasRequestId env) =>
  (Kronor.Tracer.HasTraceSpan env) =>
  Transaction a ->
  m (Either UsageError a)
write tr = do
  env <- ask
  let pool = env ^. getPgPoolL
      reqId = requestId env

  traceContext <- case env ^. Kronor.Tracer.traceSpanL of
    Nothing -> pure Aeson.Null
    Just s -> liftIO $ Kronor.Tracer.encodeSpanRemoteContext s

  Kronor.Tracer.withTrace "Db.write" do
    liftIO
      $ use pool
      $ transaction Serializable Write do
        setLocalContextTransaction reqId traceContext
        tr

writeOr ::
  (MonadReader env m) =>
  (MonadUnliftIO m) =>
  (HasPgPool env) =>
  (HasRequestId env) =>
  (Kronor.Tracer.HasTraceSpan env) =>
  (UsageError -> m a) ->
  Transaction a ->
  m a
writeOr errHandler tr = do
  result <- write tr
  either errHandler pure result

read ::
  (MonadUnliftIO m) =>
  (Kronor.Tracer.HasTraceSpan env) =>
  (MonadReader env m) =>
  (HasPgPool env) =>
  Transaction a ->
  m (Either UsageError a)
read tr = do
  pool <- asks (^. getPgPoolL)
  Kronor.Tracer.withTrace "Db.read" do
    liftIO $ use pool $ transaction Serializable Read tr

readOnNamedPool ::
  forall env m a.
  ( MonadUnliftIO m,
    Kronor.Tracer.HasTraceSpan env,
    MonadReader env m,
    HasNamedPool "csv-replica" env
  ) =>
  Text ->
  Transaction a ->
  m (Either UsageError a)
readOnNamedPool poolName tr = do
  pool <- asks (^. getPgNamedPoolL (Proxy @"csv-replica"))
  Kronor.Tracer.withTrace ("Db.read.namedPool." <> poolName) do
    liftIO $ use pool $ transaction RepeatableRead Read tr

readOr ::
  (MonadUnliftIO m) =>
  (Kronor.Tracer.HasTraceSpan env) =>
  (MonadReader env m) =>
  (HasPgPool env) =>
  (UsageError -> m a) ->
  Transaction a ->
  m a
readOr errHandler tr = do
  result <- read tr
  either errHandler pure result

-- | Set transaction-local context for request tracing and distributed trace correlation.
-- Called at the start of every write transaction so that job_queue.enqueue_payload
-- and other DB functions can include the request ID and trace context.
setLocalContextTransaction :: RequestId -> Aeson.Value -> Transaction ()
setLocalContextTransaction reqId traceContext = do
  Hasql.Transaction.statement (coerce reqId, traceContext)
    $ makeUnprepared
      [resultlessStatement|
                SELECT job_queue.set_local_transaction_context(
                    request_id => $1::text,
                    trace_context => $2::jsonb
                )::text
            |]
