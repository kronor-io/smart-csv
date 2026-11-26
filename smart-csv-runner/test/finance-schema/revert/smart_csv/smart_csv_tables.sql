-- Revert smart-csv:smart_csv_tables from pg
BEGIN;
    DROP TABLE IF EXISTS smart_csv.smart_graphql_csv_generator;
    DROP TABLE IF EXISTS smart_csv.generated_csv;
COMMIT;
