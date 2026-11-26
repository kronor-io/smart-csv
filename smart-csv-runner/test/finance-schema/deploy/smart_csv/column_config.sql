BEGIN;

  -- Reusable named column configuration presets for CSV header mapping.
  -- Keys map flattened GraphQL field paths to display names (or null to suppress).
  CREATE TABLE IF NOT EXISTS smart_csv.column_config (
    name text_100 PRIMARY KEY,
    config jsonb NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now()
  );

  -- Per-request column config override and/or reference to a named preset.
  ALTER TABLE smart_csv.smart_graphql_csv_generator
    ADD COLUMN IF NOT EXISTS column_config jsonb,
    ADD COLUMN IF NOT EXISTS column_config_name text_100;

  ALTER TABLE smart_csv.smart_graphql_csv_generator
    ADD CONSTRAINT fk_column_config_name
    FOREIGN KEY (column_config_name)
    REFERENCES smart_csv.column_config(name);

COMMIT;
