-- Verify smart_csv:smart_csv/file_format

begin;

select file_format from smart_csv.smart_graphql_csv_generator
where false;

rollback;
