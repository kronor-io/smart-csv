-- Deploy finance:smart_csv/view/generated_csv_job to pg

begin;

drop view if exists smart_csv.generated_csv_job;

create view smart_csv.generated_csv_job as
select
    generated_csv.id,
    generated_csv.shard_id,
    coalesce(
        smart_graphql_csv_generator.token_claims #>> '{https://hasura.io/jwt/claims,x-hasura-user-id}',
        smart_graphql_csv_generator.token_claims ->> 'x-hasura-user-id'
    )::bigint as user_id,
    smart_graphql_csv_generator.query,
    smart_graphql_csv_generator.recipient,
    smart_graphql_csv_generator.pagination_key,
    generated_csv.number_of_rows,
    generated_csv.created_at,
    generated_csv.last_updated_at,
    generated_csv.last_pagination_time,
    generated_csv.expires_at,
    generated_csv.start_date,
    generated_csv.end_date,
    generated_csv.status,
    generated_csv.link,
    generated_csv.err_message
from smart_csv.generated_csv
inner join smart_csv.smart_graphql_csv_generator
    on
        generated_csv.shard_id = smart_graphql_csv_generator.shard_id
        and generated_csv.id = smart_graphql_csv_generator.id
where coalesce(
    smart_graphql_csv_generator.token_claims #>> '{https://hasura.io/jwt/claims,x-hasura-user-id}',
    smart_graphql_csv_generator.token_claims ->> 'x-hasura-user-id'
) is not null;

comment on view smart_csv.generated_csv_job is
'Lists smart csv export jobs together with request metadata and the originating Hasura user id.';

commit;
