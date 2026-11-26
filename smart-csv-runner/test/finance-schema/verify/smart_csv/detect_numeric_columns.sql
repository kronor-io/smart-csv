-- Verify smart_csv:smart_csv/detect_numeric_columns

begin;

select detect_numeric_columns from smart_csv.smart_graphql_csv_generator
where false;

rollback;
