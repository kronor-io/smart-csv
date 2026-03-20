-- Deploy smart-csv:job_queue_tables to pg
-- Requires: schemas

BEGIN;

    -- Main job queue: jobs ready to be processed
    CREATE TABLE IF NOT EXISTS job_queue.payload (
        id bigint NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1 CACHE 32767),
        attempts int NOT NULL DEFAULT 0,
        priority int NOT NULL DEFAULT 0,
        value jsonb NOT NULL,
        run_at timestamptz NOT NULL DEFAULT now(),
        enqueued_at timestamptz NOT NULL DEFAULT now(),
        PRIMARY KEY (id)
    );

    CREATE INDEX IF NOT EXISTS idx_payload_priority
        ON job_queue.payload (priority DESC);

    CREATE INDEX IF NOT EXISTS idx_job_queue_payload_tag
        ON job_queue.payload ((value->>'tag'));

    ALTER TABLE job_queue.payload SET (
        autovacuum_vacuum_scale_factor = 0,
        autovacuum_vacuum_threshold = 1000,
        autovacuum_vacuum_insert_threshold = 1000
    );

    -- Scheduled/delayed jobs
    CREATE TABLE IF NOT EXISTS job_queue.task (
        id bigserial PRIMARY KEY,
        run_at timestamptz NOT NULL DEFAULT now(),
        attempts int NOT NULL DEFAULT 0,
        priority int NOT NULL DEFAULT 0,
        value jsonb NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_task_run_at
        ON job_queue.task USING btree (run_at ASC, priority DESC);

    CREATE INDEX IF NOT EXISTS idx_job_queue_task_tag
        ON job_queue.task ((value->>'tag'));

    ALTER TABLE job_queue.task SET (
        autovacuum_vacuum_scale_factor = 0,
        autovacuum_vacuum_threshold = 1000,
        autovacuum_vacuum_insert_threshold = 1000
    );

    -- Jobs currently being processed (unlogged for performance)
    CREATE UNLOGGED TABLE IF NOT EXISTS job_queue.task_in_process (
        id bigint,
        priority int NOT NULL,
        value jsonb NOT NULL,
        run_at timestamptz NOT NULL DEFAULT now(),
        enqueued_at timestamptz NOT NULL DEFAULT now()
    );

    -- Failed jobs archive
    CREATE TABLE IF NOT EXISTS job_queue.failed_job (
        id bigint NOT NULL PRIMARY KEY,
        failed_at timestamptz NOT NULL DEFAULT now(),
        value jsonb
    );

    -- Job expiry settings (per tag)
    CREATE TABLE IF NOT EXISTS job_queue.job_expiry_setting (
        tag text NOT NULL PRIMARY KEY,
        expiry_time interval NOT NULL DEFAULT interval '1 hour'
    );

    -- Job throttle settings (per tag)
    CREATE TABLE IF NOT EXISTS job_queue.tag_throttle_setting (
        tag text NOT NULL PRIMARY KEY,
        job_limit int NOT NULL DEFAULT 2000
    );

    -- Circuit breaker state
    CREATE TABLE IF NOT EXISTS job_queue.circuit_breaker_state (
        label text_100 NOT NULL PRIMARY KEY,
        error_count int NOT NULL DEFAULT 0,
        error_threshold int NOT NULL DEFAULT 5,
        circuit_condition text_100 NOT NULL DEFAULT 'Active',
        drip_frequency int NOT NULL DEFAULT 30000,
        exponentiation_factor float NOT NULL DEFAULT 2,
        exponentiation_cap int NOT NULL DEFAULT 86400,

        CONSTRAINT drip_frequency_cannot_be_negative CHECK (drip_frequency >= 0),
        CONSTRAINT error_threshold_cannot_be_negative CHECK (error_threshold >= 0),
        CONSTRAINT error_count_cannot_be_negative CHECK (error_count >= 0)
    );

COMMIT;
