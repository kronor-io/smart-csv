-- Revert smart-csv:order_by from pg

begin;

alter table smart_csv.smart_graphql_csv_generator
drop column if exists order_by;

commit;
