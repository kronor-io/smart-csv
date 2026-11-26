-- Deploy smart-csv:detect_numeric_columns to pg
-- Requires: smart_csv/smart_csv_tables

-- Mirrors smart-csv database/deploy/detect_numeric_columns.sql: a request can ask
-- smart-csv to read the GraphQL schema and write numeric columns with a decimal
-- comma even when Hasura sends the numbers as strings.

begin;

alter table smart_csv.smart_graphql_csv_generator
add column if not exists detect_numeric_columns boolean not null default false;

commit;
