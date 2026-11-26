-- Revert smart-csv:file_format from pg

begin;

alter table smart_csv.smart_graphql_csv_generator
drop column if exists file_format;

commit;
