-- Deploy smart-csv:order_by to pg
-- Requires: smart_csv/smart_csv_tables

-- Mirrors smart-csv database/deploy/order_by.sql: cursor pagination now carries the
-- per-request GraphQL orderBy so exports keep a stable, caller-defined sort.

begin;

alter table smart_csv.smart_graphql_csv_generator
add column if not exists order_by jsonb null;

commit;
