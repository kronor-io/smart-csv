-- Deploy smart-csv:order_by to pg
-- Requires: smart_csv_tables

BEGIN;

    ALTER TABLE smart_csv.smart_graphql_csv_generator
        ADD COLUMN IF NOT EXISTS order_by jsonb NULL;

COMMIT;