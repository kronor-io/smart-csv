-- Revert smart-csv:job_queue_triggers from pg
BEGIN;
    DROP TRIGGER IF EXISTS notify_new_payload_insert ON job_queue.payload;
    DROP TRIGGER IF EXISTS notify_new_payload_update ON job_queue.payload;
    DROP FUNCTION IF EXISTS job_queue.trig_notify_new_payload();
COMMIT;
