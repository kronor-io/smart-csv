-- Deploy smart-csv:job_queue_triggers to pg
-- Requires: job_queue_tables

BEGIN;

    CREATE OR REPLACE FUNCTION job_queue.trig_notify_new_payload()
    RETURNS trigger AS
    $$
        BEGIN
            PERFORM pg_notify('job_created', null)
            FROM changed
            LIMIT 1;
            RETURN null;
        END
    $$ LANGUAGE plpgsql;

    DROP TRIGGER IF EXISTS notify_new_payload_insert ON job_queue.payload;
    DROP TRIGGER IF EXISTS notify_new_payload_update ON job_queue.payload;

    CREATE TRIGGER notify_new_payload_insert
    AFTER INSERT ON job_queue.payload
    REFERENCING NEW TABLE AS changed
    FOR EACH STATEMENT
    EXECUTE FUNCTION job_queue.trig_notify_new_payload();

    CREATE TRIGGER notify_new_payload_update
    AFTER UPDATE ON job_queue.payload
    REFERENCING NEW TABLE AS changed
    FOR EACH STATEMENT
    EXECUTE FUNCTION job_queue.trig_notify_new_payload();

COMMIT;
