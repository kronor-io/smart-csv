-- Revert smart-csv:order_by from pg

BEGIN;

    ALTER TABLE smart_csv.smart_graphql_csv_generator
        DROP COLUMN IF EXISTS order_by;

COMMIT;