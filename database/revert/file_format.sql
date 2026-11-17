-- Revert smart-csv:file_format from pg

BEGIN;

    ALTER TABLE smart_csv.smart_graphql_csv_generator
        DROP COLUMN IF EXISTS file_format;

COMMIT;
