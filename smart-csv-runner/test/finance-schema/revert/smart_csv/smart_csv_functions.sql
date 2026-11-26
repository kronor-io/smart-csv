-- Revert smart-csv:smart_csv_functions from pg
BEGIN;
    DROP FUNCTION IF EXISTS smart_csv.on_csv_report_error(fsm_event_payload);
    DROP FUNCTION IF EXISTS smart_csv.announce_csv_report(fsm_event_payload);
    DROP FUNCTION IF EXISTS smart_csv.enqueue_csv_report_job(fsm_event_payload);
    DROP FUNCTION IF EXISTS job_queue.mk_smart_graphql_csv_generate_job_payload(text, bigint, bigint);
COMMIT;
