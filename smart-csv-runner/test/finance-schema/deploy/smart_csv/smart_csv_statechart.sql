-- Deploy smart-csv:smart_csv_statechart to pg
-- Requires: smart_csv_tables, smart_csv_functions
-- Note: Requires the FSM infrastructure from https://github.com/kronor-io/statecharts

BEGIN;

DO $$
DECLARE
    chart bigint;
BEGIN
    INSERT INTO fsm.statechart (name, version) VALUES ('csv_report_flow', 1.1::semver)
    RETURNING id INTO chart;

    INSERT INTO fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) VALUES
        (chart, 'initializing', 'INITIALIZING', null, true, false,
            array[('smart_csv', 'enqueue_csv_report_job')]::fsm_callback_name[],
            array[]::fsm_callback_name[]),
        (chart, 'generating_csv_report', 'GENERATING_CSV_REPORT', null, false, false,
            array[]::fsm_callback_name[],
            array[]::fsm_callback_name[]),
        (chart, 'done', 'DONE', null, false, true,
            array[('smart_csv', 'announce_csv_report')]::fsm_callback_name[],
            array[]::fsm_callback_name[]),
        (chart, 'error', 'ERROR', null, false, true,
            array[('smart_csv', 'on_csv_report_error')]::fsm_callback_name[],
            array[]::fsm_callback_name[]);

    INSERT INTO fsm.transition (statechart_id, event, source_state, target_state) VALUES
        (chart, 'csv_report.generate', 'initializing', 'generating_csv_report'),
        (chart, 'csv_report.error', 'initializing', 'error'),
        (chart, 'csv_report.done', 'generating_csv_report', 'done'),
        (chart, 'csv_report.error', 'generating_csv_report', 'error');
END
$$;

COMMIT;
