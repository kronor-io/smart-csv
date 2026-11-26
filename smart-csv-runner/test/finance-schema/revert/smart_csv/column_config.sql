BEGIN;

  ALTER TABLE smart_csv.smart_graphql_csv_generator
    DROP CONSTRAINT IF EXISTS fk_column_config_name;

  ALTER TABLE smart_csv.smart_graphql_csv_generator
    DROP COLUMN IF EXISTS column_config,
    DROP COLUMN IF EXISTS column_config_name;

  DROP TABLE IF EXISTS smart_csv.column_config;

COMMIT;
