-- Deploy smart-csv:file_format to pg
-- Requires: smart_csv_tables

BEGIN;

    ALTER TABLE smart_csv.smart_graphql_csv_generator
        ADD COLUMN IF NOT EXISTS file_format text NOT NULL DEFAULT 'csv'
            CONSTRAINT smart_graphql_csv_generator_file_format_check CHECK (file_format IN ('csv', 'xlsx'));

COMMIT;
