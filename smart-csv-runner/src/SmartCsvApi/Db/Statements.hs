module SmartCsvApi.Db.Statements
  ( insertSmartGraphqlCsvGenerator,
    setTransactionContext,
  )
where

import Data.Aeson qualified as Aeson
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement, singletonStatement)
import RIO

-- | Insert a smart GraphQL CSV generator request into the database.
-- Parameters: (shardId, recipient, paginationKey, queryBody, queryVariables, tokenClaims, columnConfig, columnConfigName)
-- Returns: the generated CSV ID
insertSmartGraphqlCsvGenerator :: Statement (Int64, Text, Text, Text, Aeson.Value, Aeson.Value, Maybe Aeson.Value, Maybe Text) Int64
insertSmartGraphqlCsvGenerator =
  [singletonStatement|
        with gcsv as
        (insert into smart_csv.generated_csv
           (shard_id, expires_at, start_date, end_date)
        select $1::bigint
             , now() + interval '5 days'
             , now()
             , now()
        returning id
        )
        insert into smart_csv.smart_graphql_csv_generator
            ( shard_id
            , recipient
            , id
            , pagination_key
            , query
            , variables
            , token_claims
            , column_config
            , column_config_name
            )
        select
            $1::bigint
          , $2::text
          , gcsv.id
          , $3::text
          , $4::text
          , $5::jsonb
          , $6::jsonb
          , $7::jsonb?
          , $8::text?
        from gcsv
        returning id::bigint
    |]

-- | Set the transaction-local request ID and trace context required by
-- job_queue.enqueue_payload and other DB functions.
setTransactionContext :: Statement Text ()
setTransactionContext =
  [resultlessStatement|
        SELECT job_queue.set_local_transaction_context(
            request_id => $1::text,
            trace_context => '{"traceparent":"none"}'::jsonb
        )::text
    |]
