-- Verify smart_csv:smart_csv/order_by

begin;

select order_by from smart_csv.smart_graphql_csv_generator
where false;

rollback;
