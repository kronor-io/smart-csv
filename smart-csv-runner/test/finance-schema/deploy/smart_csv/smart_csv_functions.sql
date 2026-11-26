-- Deploy smart-csv:smart_csv_functions to pg
-- Requires: smart_csv_tables, job_queue_functions

BEGIN;

    -- Job payload builder for SmartGraphqlCsvGenerate jobs
    CREATE OR REPLACE FUNCTION job_queue.mk_smart_graphql_csv_generate_job_payload(
        caller text,
        shard_id bigint,
        csv_id bigint
    )
    RETURNS jsonb AS $$
    DECLARE
        shard_id_ bigint NOT NULL := shard_id;
        csv_id_ bigint NOT NULL := csv_id;
        caller_ text NOT NULL := caller;
    BEGIN
        RETURN jsonb_build_object(
            'tag', 'SmartGraphqlCsvGenerate',
            'meta', jsonb_build_object('caller', caller_),
            'contents', jsonb_build_object('shardId', shard_id_, 'csvId', csv_id_)
        );
    END
    $$ LANGUAGE plpgsql IMMUTABLE;


    -- Called by the state machine on entry to INITIALIZING.
    -- The actual job enqueueing is done by the insert trigger on
    -- smart_graphql_csv_generator, so this is a no-op.
    CREATE OR REPLACE FUNCTION smart_csv.enqueue_csv_report_job(event_payload fsm_event_payload)
    RETURNS void AS $$
    BEGIN
        RETURN;
    END
    $$ LANGUAGE plpgsql;


    -- Called by the state machine on entry to DONE.
    CREATE OR REPLACE FUNCTION smart_csv.announce_csv_report(event_payload fsm_event_payload)
    RETURNS void AS $$
    BEGIN
        RETURN;
    END
    $$ LANGUAGE plpgsql;


    -- Called by the state machine on entry to ERROR.
    CREATE OR REPLACE FUNCTION smart_csv.on_csv_report_error(event_payload fsm_event_payload)
    RETURNS void AS $$
    BEGIN
        RETURN;
    END
    $$ LANGUAGE plpgsql;

COMMIT;
