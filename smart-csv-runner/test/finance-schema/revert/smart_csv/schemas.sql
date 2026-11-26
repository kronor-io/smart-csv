-- Revert smart-csv:schemas from pg
BEGIN;
    DROP SCHEMA IF EXISTS smart_csv CASCADE;
    DROP SCHEMA IF EXISTS job_queue CASCADE;
COMMIT;
