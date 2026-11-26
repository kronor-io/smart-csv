-- Deploy smart-csv:smart_csv_tables to pg
-- Requires: schemas, extensions, reference_tables

BEGIN;

    -- Main table tracking CSV generation progress
    CREATE TABLE IF NOT EXISTS smart_csv.generated_csv (
        shard_id bigint NOT NULL,
        id bigint NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY (START WITH 1 CACHE 32767),
        state_machine_id bigint NOT NULL,

        last_uploaded_part int NULL,
        number_of_rows int NOT NULL DEFAULT 0,

        created_at timestamptz NOT NULL DEFAULT now(),
        last_updated_at timestamptz NULL,
        last_pagination_time timestamptz NULL,
        expires_at timestamptz,

        start_date timestamptz NOT NULL,
        end_date timestamptz NOT NULL,

        status citext NOT NULL DEFAULT 'INITIALIZED',
        file_path text_10000,
        link text_10000,
        err_message text_10000,
        bucket_name text NULL,
        last_pagination_key text NULL,
        upload_id text NULL,
        part_entities bytea[] NULL,

        CONSTRAINT link_should_be_present_when_status_is_done
            CHECK (status <> 'DONE' OR link IS NOT NULL),

        CONSTRAINT expires_at_should_be_present_when_status_is_done
            CHECK (status <> 'DONE' OR expires_at IS NOT NULL),

        CONSTRAINT err_message_should_be_null_when_status_is_done
            CHECK (status <> 'DONE' OR err_message IS NULL),

        CONSTRAINT fk_report_status
            FOREIGN KEY (status)
            REFERENCES smart_csv.report_status(name),

        CONSTRAINT fk_state_machine
            FOREIGN KEY (shard_id, state_machine_id)
            REFERENCES fsm.state_machine(shard_id, id)
    );

    CREATE INDEX IF NOT EXISTS idx_generated_csv_start_date
        ON smart_csv.generated_csv (start_date);

    CREATE INDEX IF NOT EXISTS idx_generated_csv_end_date
        ON smart_csv.generated_csv (end_date);

    CREATE INDEX IF NOT EXISTS idx_generated_csv_shard_id
        ON smart_csv.generated_csv (shard_id);

    COMMENT ON TABLE smart_csv.generated_csv IS
        'Tracks the progress and result of CSV generation jobs.';


    -- Stores the GraphQL query configuration for each CSV generation request
    CREATE TABLE IF NOT EXISTS smart_csv.smart_graphql_csv_generator (
        shard_id bigint NOT NULL,
        id bigint NOT NULL,

        created_at timestamptz NOT NULL DEFAULT now(),

        pagination_key text_100 NOT NULL,
        query text_10000 NOT NULL,
        variables jsonb NOT NULL,
        token_claims jsonb NOT NULL,
        recipient text_100,

        PRIMARY KEY (shard_id, id)
    );

    COMMENT ON TABLE smart_csv.smart_graphql_csv_generator IS
        'CSV generation requests with their GraphQL query configuration.';


    -- Email sending status enum
    DO $$ BEGIN
        CREATE TYPE smart_csv.email_registry_status AS ENUM ('sending', 'done', 'error');
    EXCEPTION WHEN duplicate_object THEN NULL;
    END $$;

    -- Email deduplication registry (prevents re-sending on job retries)
    CREATE TABLE IF NOT EXISTS smart_csv.email_registry (
        id uuid NOT NULL,
        status smart_csv.email_registry_status NOT NULL,
        amount_attempted int NOT NULL DEFAULT 0,
        created_at timestamptz NOT NULL DEFAULT now(),
        PRIMARY KEY (id)
    );

    COMMENT ON TABLE smart_csv.email_registry IS
        'Tracks email sending status to prevent duplicate sends on job retries.';

COMMIT;
