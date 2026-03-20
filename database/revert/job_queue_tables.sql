-- Revert smart-csv:job_queue_tables from pg
BEGIN;
    DROP TABLE IF EXISTS job_queue.circuit_breaker_state;
    DROP TABLE IF EXISTS job_queue.tag_throttle_setting;
    DROP TABLE IF EXISTS job_queue.job_expiry_setting;
    DROP TABLE IF EXISTS job_queue.failed_job;
    DROP TABLE IF EXISTS job_queue.task_in_process;
    DROP TABLE IF EXISTS job_queue.task;
    DROP TABLE IF EXISTS job_queue.payload;
COMMIT;
