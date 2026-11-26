-- Deploy smart-csv:file_format to pg
-- Requires: smart_csv/smart_csv_tables

-- Mirrors smart-csv database/deploy/file_format.sql: a request can ask smart-csv
-- for an Excel file instead of a CSV file. smart-csv 0.11.0 reads the column for
-- every export.

begin;

alter table smart_csv.smart_graphql_csv_generator
add column if not exists file_format text not null default 'csv'
constraint smart_graphql_csv_generator_file_format_check check (file_format in ('csv', 'xlsx'));

commit;
