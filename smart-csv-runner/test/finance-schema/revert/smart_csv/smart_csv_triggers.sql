-- Revert smart-csv:smart_csv_triggers from pg
BEGIN;
    DROP TRIGGER IF EXISTS enqueue_smart_csv_generate_job ON smart_csv.smart_graphql_csv_generator;
    DROP FUNCTION IF EXISTS smart_csv.trig_enqueue_smart_csv_generate_job();
COMMIT;
