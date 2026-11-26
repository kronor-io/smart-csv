-- Revert smart-csv:job_queue_functions from pg
BEGIN;
    DROP FUNCTION IF EXISTS job_queue.mark_as_failed(bigint);
    DROP FUNCTION IF EXISTS job_queue.retry_job(bigint, timestamptz, int, int);
    DROP FUNCTION IF EXISTS job_queue.dequeue_payload(int);
    DROP FUNCTION IF EXISTS job_queue.enqueue_payload(jsonb[], int, timestamptz, int, jsonb);
COMMIT;
