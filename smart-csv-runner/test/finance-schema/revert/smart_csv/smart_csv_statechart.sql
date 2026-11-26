-- Revert smart-csv:smart_csv_statechart from pg
BEGIN;
    -- Disable triggers to avoid FSM constraint checks during deletion
    SET session_replication_role = replica;

    DELETE FROM fsm.transition WHERE statechart_id IN (
        SELECT id FROM fsm.statechart WHERE name = 'csv_report_flow'
    );
    DELETE FROM fsm.state WHERE statechart_id IN (
        SELECT id FROM fsm.statechart WHERE name = 'csv_report_flow'
    );
    DELETE FROM fsm.statechart WHERE name = 'csv_report_flow';

    SET session_replication_role = DEFAULT;
COMMIT;
