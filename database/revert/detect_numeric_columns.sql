-- Revert smart-csv:detect_numeric_columns from pg

BEGIN;

    ALTER TABLE smart_csv.smart_graphql_csv_generator
        DROP COLUMN IF EXISTS detect_numeric_columns;

COMMIT;
