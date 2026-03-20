module Kronor.SmartCsv.Statements (
    GeneratedCsvPayload (..),
    insertSmartGraphqlCsvGenerator,
    selectGeneratedCsvPayload,
    selectGeneratorConfig,
    selectColumnConfigByName,
    enqueueCompletionEmail,
    signJwtFromClaims,
)
where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, resultlessStatement, singletonStatement)
import Kronor.SmartCsv.Query (GenericQuery (..))
import RIO


data GeneratedCsvPayload = GeneratedCsvPayload
    { shardId :: Int64
    , stateMachineId :: Int64
    , reportId :: Int64
    , startDate :: UTCTime
    , endDate :: UTCTime
    }
    deriving stock (Eq, Show)


insertSmartGraphqlCsvGenerator :: Statement (Int64, Text, Text, Text, Aeson.Value, Aeson.Value) Int64
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
            )
        select
            $1::bigint
          , $2::text
          , gcsv.id
          , $3::text
          , $4::text
          , $5::jsonb
          , $6::jsonb
        from gcsv
        returning id::bigint
    |]


selectGeneratedCsvPayload :: Statement Int64 GeneratedCsvPayload
selectGeneratedCsvPayload =
    [singletonStatement|
        select $GeneratedCsvPayload{
            shardId = gcsv.shard_id::bigint
          , stateMachineId = gcsv.state_machine_id::bigint
          , reportId = gcsv.id::bigint
          , startDate = gcsv.start_date::timestamptz
          , endDate = gcsv.end_date::timestamptz
        }
        from smart_csv.generated_csv gcsv
        where gcsv.id = $1::bigint
    |]


selectGeneratorConfig :: Statement (Int64, Int64) (GenericQuery, Aeson.Value, Text, Maybe Aeson.Value, Maybe Text)
selectGeneratorConfig =
    [singletonStatement|
        select $GenericQuery{paginationKey = pagination_key::text?, query = query::text, variables = variables::jsonb},
            token_claims::jsonb,
            recipient::text,
            column_config::jsonb?,
            column_config_name::text?
        from smart_csv.smart_graphql_csv_generator
        where shard_id = $1::bigint
          and id = $2::bigint
    |]


selectColumnConfigByName :: Statement Text (Maybe Aeson.Value)
selectColumnConfigByName =
    [maybeStatement|
        select config::jsonb
        from smart_csv.column_config
        where name = $1::text
    |]


enqueueCompletionEmail :: Statement (Text, Text, Text, Text, Text, Text, Int32) ()
enqueueCompletionEmail =
    [resultlessStatement|
        with service_mail as (select email from smart_csv.service_mail where name = 'noreply')
        select job_queue.enqueue_payload(
            array[
                jsonb_build_object(
                    'tag', $5::text,
                    'meta', jsonb_build_object(
                        'caller', $4::text
                    ),
                    'contents', jsonb_build_object(
                        'email_id', gen_random_uuid(),
                        'to', json_build_array(null, $1::text),
                        'from', json_build_array(null, (select email from service_mail)),
                        'subject', $2::text,
                        'body', $3::text
                    )
                )
            ]::jsonb[],
            priority_ := $7::int,
            request_id_ := to_jsonb($6::text)
        )::text
    |]


signJwtFromClaims :: Statement (Maybe Text, Maybe Aeson.Value, Maybe Text, Maybe Text) Text
signJwtFromClaims =
    [singletonStatement|
        select
            sign(
                (json_build_object(
                    'https://hasura.io/jwt/claims', $2::jsonb?,
                    'iat', (select extract(epoch from now())),
                    'exp', (select extract(epoch from now() + interval '1 hour')),
                    'tid', $4::text?,
                    'ttype', $3::text?,
                    'tname', null,
                    'associated_email', $1::text?
                )::jsonb)::json,
                current_setting('graphql.jwt_secret')
            )::text
    |]
