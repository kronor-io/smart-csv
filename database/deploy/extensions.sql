-- Deploy smart-csv:extensions to pg
-- Requires: schemas

BEGIN;

    -- citext for case-insensitive text comparisons (used by generated_csv.status)
    CREATE EXTENSION IF NOT EXISTS citext;

    -- pgjwt for signing JWTs from stored token claims
    CREATE EXTENSION IF NOT EXISTS pgjwt;

    -- Custom text domains with length constraints
    DO $$ BEGIN
        CREATE DOMAIN text_10000 AS text
            CHECK (char_length(value) <= 10000 AND trim(both ' ' from value) <> '');
    EXCEPTION WHEN duplicate_object THEN NULL;
    END $$;

    DO $$ BEGIN
        CREATE DOMAIN text_100 AS text
            CHECK (char_length(value) <= 100 AND trim(both ' ' from value) <> '');
    EXCEPTION WHEN duplicate_object THEN NULL;
    END $$;

COMMIT;
