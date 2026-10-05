-- Deploy smart-csv:detect_numeric_columns to pg
-- Requires: smart_csv_tables

BEGIN;

    ALTER TABLE smart_csv.smart_graphql_csv_generator
        ADD COLUMN IF NOT EXISTS detect_numeric_columns boolean NOT NULL DEFAULT false;

COMMIT;
