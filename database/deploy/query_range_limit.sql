BEGIN;

  CREATE TABLE IF NOT EXISTS smart_csv.query_range_limit (
    root_name text_100 PRIMARY KEY,
    max_range_days integer NOT NULL CHECK (max_range_days > 0),
    created_at timestamptz NOT NULL DEFAULT now()
  );

COMMIT;
