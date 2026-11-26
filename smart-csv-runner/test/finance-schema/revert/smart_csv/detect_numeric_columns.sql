-- Revert smart-csv:detect_numeric_columns from pg

begin;

alter table smart_csv.smart_graphql_csv_generator
drop column if exists detect_numeric_columns;

commit;
