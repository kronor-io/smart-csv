-- Deploy smart-csv:smart_csv_triggers to pg
-- Requires: smart_csv_tables, smart_csv_functions, smart_csv_statechart

BEGIN;

    -- BEFORE INSERT: Create a state machine for the CSV report and set state_machine_id
    CREATE OR REPLACE FUNCTION smart_csv.trig_create_csv_report_flow()
    RETURNS trigger AS $$
    BEGIN
        IF new.state_machine_id IS NULL THEN
            SELECT s.id INTO new.state_machine_id
            FROM fsm.create_state_machine_with_latest_statechart(
                new.shard_id,
                'csv_report_flow'
            ) s;
        END IF;
        RETURN new;
    END
    $$ LANGUAGE plpgsql;

    DROP TRIGGER IF EXISTS create_csv_report_flow ON smart_csv.generated_csv;

    CREATE TRIGGER create_csv_report_flow
    BEFORE INSERT ON smart_csv.generated_csv
    FOR EACH ROW
    EXECUTE FUNCTION smart_csv.trig_create_csv_report_flow();


    -- AFTER INSERT: Start the state machine (transitions from initial state)
    CREATE OR REPLACE FUNCTION smart_csv.trig_initiate_csv_report_flow()
    RETURNS trigger AS $$
    BEGIN
        PERFORM fsm.start_machine(
            new.shard_id,
            new.state_machine_id,
            jsonb_build_object(
                'shardId', new.shard_id,
                'startDate', new.start_date,
                'endDate', new.end_date,
                'reportId', new.id,
                'stateMachineId', new.state_machine_id
            )
        );
        RETURN new;
    END
    $$ LANGUAGE plpgsql;

    DROP TRIGGER IF EXISTS initiate_csv_report_flow ON smart_csv.generated_csv;

    CREATE TRIGGER initiate_csv_report_flow
    AFTER INSERT ON smart_csv.generated_csv
    FOR EACH ROW
    EXECUTE FUNCTION smart_csv.trig_initiate_csv_report_flow();


    -- AFTER INSERT on smart_graphql_csv_generator: Enqueue a SmartGraphqlCsvGenerate job
    CREATE OR REPLACE FUNCTION smart_csv.trig_enqueue_smart_csv_generate_job()
    RETURNS trigger AS $$
    BEGIN
        PERFORM job_queue.enqueue_payload(
            array[job_queue.mk_smart_graphql_csv_generate_job_payload(
                caller := 'trigger:smart_csv.trig_enqueue_smart_csv_generate',
                shard_id := new.shard_id,
                csv_id := new.id
            )],
            priority_ := 5000
        );
        RETURN new;
    END
    $$ LANGUAGE plpgsql;

    DROP TRIGGER IF EXISTS enqueue_smart_csv_generate_job ON smart_csv.smart_graphql_csv_generator;

    CREATE TRIGGER enqueue_smart_csv_generate_job
    AFTER INSERT ON smart_csv.smart_graphql_csv_generator
    FOR EACH ROW
    EXECUTE FUNCTION smart_csv.trig_enqueue_smart_csv_generate_job();

COMMIT;
