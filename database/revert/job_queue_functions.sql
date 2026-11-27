-- Deploy smart-csv:job_queue_functions to pg
-- Requires: job_queue_tables

BEGIN;

    -- Sets transaction-local context variables used by enqueue_payload
    -- for request tracing and distributed trace correlation.
    CREATE OR REPLACE FUNCTION job_queue.set_local_transaction_context(
        request_id text,
        trace_context jsonb
    )
    RETURNS void AS $$
    BEGIN
        PERFORM set_config('kronor.request_id', request_id, true);
        PERFORM set_config('kronor.trace_context', trace_context::text, true);
    END
    $$ LANGUAGE plpgsql;


    -- Enqueue one or more jobs into the queue
    CREATE OR REPLACE FUNCTION job_queue.enqueue_payload(
        values_ jsonb[],
        priority_ int,
        at_ timestamptz DEFAULT now(),
        attempts_ int DEFAULT 0,
        request_id_ jsonb DEFAULT null
    )
    RETURNS SETOF job_queue.task AS
    $$
        BEGIN
            IF at_ > now() THEN
                RETURN QUERY
                    INSERT INTO job_queue.task (run_at, attempts, priority, value)
                    SELECT at_, attempts_, priority_,
                        val || jsonb_build_object('meta',
                            coalesce(val->'meta', '{}'::jsonb)
                            || jsonb_build_object('requestId', coalesce(request_id_, (SELECT to_jsonb(current_setting('kronor.request_id')::text))))
                            || coalesce(
                                (CASE current_setting('kronor.trace_context', true)
                                  WHEN 'null' THEN '{}'::jsonb
                                  ELSE (SELECT to_jsonb(current_setting('kronor.trace_context', true)::jsonb))
                                END),
                                '{}'::jsonb
                            )
                            || jsonb_build_object('expires_at', coalesce(
                                (val->'meta'->>'expires_at')::timestamptz,
                                now() + coalesce(job_expiry_setting.expiry_time, interval '1 hour')
                            ))
                        )
                    FROM unnest(values_) val
                    LEFT JOIN job_queue.job_expiry_setting
                        ON val->>'tag' = job_expiry_setting.tag
                    RETURNING *;
            ELSE
                RETURN QUERY
                    INSERT INTO job_queue.payload (attempts, priority, run_at, value)
                    SELECT attempts_, priority_, at_,
                        val || jsonb_build_object('meta',
                            coalesce(val->'meta', '{}'::jsonb)
                            || jsonb_build_object('requestId', coalesce(request_id_, (SELECT to_jsonb(current_setting('kronor.request_id')::text))))
                            || coalesce(
                                (CASE current_setting('kronor.trace_context', true)
                                  WHEN 'null' THEN '{}'::jsonb
                                  ELSE (SELECT to_jsonb(current_setting('kronor.trace_context', true)::jsonb))
                                END),
                                '{}'::jsonb
                            )
                            || jsonb_build_object('expires_at', coalesce(
                                (val->'meta'->>'expires_at')::timestamptz,
                                now() + coalesce(job_expiry_setting.expiry_time, interval '1 hour')
                            ))
                        )
                    FROM unnest(values_) val
                    LEFT JOIN job_queue.job_expiry_setting
                        ON val->>'tag' = job_expiry_setting.tag
                    RETURNING
                        id, at_ AS run_at, attempts, priority, value;
            END IF;
        END
    $$ LANGUAGE plpgsql VOLATILE;

    -- Dequeue a batch of jobs for processing
    CREATE OR REPLACE FUNCTION job_queue.dequeue_payload(limit_ int)
    RETURNS TABLE (id bigint, value jsonb, attempts int, time_in_queue interval, expired boolean, run_at timestamptz) AS
    $$
        WITH available AS (
            SELECT p1.id, p1.priority, p1.value, p1.run_at, p1.enqueued_at, p1.xmin
            FROM job_queue.payload AS p1
            ORDER BY priority DESC
            FOR UPDATE SKIP LOCKED
            LIMIT limit_
        ),
        transition AS (
            INSERT INTO job_queue.task_in_process
            SELECT
                available.id, available.priority, available.value,
                available.run_at, pg_xact_commit_timestamp(available.xmin)
            FROM available
            RETURNING task_in_process.*
        ),
        dequeued AS (
            DELETE FROM job_queue.payload
            USING transition
            WHERE payload.id = transition.id
            RETURNING payload.id, payload.value, payload.attempts, payload.run_at, transition.enqueued_at
        )
        SELECT
            dequeued.id,
            dequeued.value,
            dequeued.attempts,
            now() - dequeued.enqueued_at AS time_in_queue,
            coalesce(now() > (value->'meta'->>'expires_at')::timestamptz, false) AS expired,
            dequeued.run_at
        FROM dequeued
    $$ LANGUAGE sql VOLATILE;

    -- Retry a failed job with decremented priority
    CREATE OR REPLACE FUNCTION job_queue.retry_job(
        id_ bigint,
        at_ timestamptz DEFAULT now(),
        priority_ int DEFAULT 0,
        attempt_ int DEFAULT 1
    )
    RETURNS SETOF job_queue.task AS
    $$
        SELECT job_queue.enqueue_payload(
            array[value],
            at_ := at_,
            priority_ := greatest(priority_ - 100, 0),
            attempts_ := attempt_ + 1,
            request_id_ := (task_in_process.value)->'meta'->'requestId'
        )
        FROM job_queue.task_in_process
        WHERE id = id_
    $$ LANGUAGE sql STRICT VOLATILE;

    -- Mark a job as permanently failed
    CREATE OR REPLACE FUNCTION job_queue.mark_as_failed(id_ bigint)
    RETURNS void AS
    $$
        INSERT INTO job_queue.failed_job (id, value)
        SELECT id_, value
        FROM job_queue.task_in_process
        WHERE id = id_
    $$ LANGUAGE sql STRICT VOLATILE;

    -- Get a batch of jobs for the Streamly-based dequeuer
    CREATE OR REPLACE FUNCTION job_queue.get_job_batch(last_id_ bigint, limit_ int)
    RETURNS SETOF job_queue.payload AS
    $$
        SELECT *
        FROM job_queue.payload AS p
        WHERE p.id > last_id_
        ORDER BY id ASC
        LIMIT limit_
        FOR UPDATE SKIP LOCKED
    $$ LANGUAGE sql STRICT;

    -- Retry a job with circuit breaker exponential backoff
    CREATE OR REPLACE FUNCTION job_queue.retry_job_circuit_closed(
        id_ bigint,
        label_ text,
        priority_ int DEFAULT 0,
        attempt_ int DEFAULT 1
    )
    RETURNS SETOF job_queue.task
    LANGUAGE sql STRICT VOLATILE
    BEGIN ATOMIC
        WITH circuit_attempt AS (
            SELECT jsonb_build_object('meta',
                coalesce(value->'meta', '{}'::jsonb)
                || jsonb_build_object('circuitBreakerAttempts',
                    coalesce((value->'meta'->'circuitBreakerAttempts')::int, 0) + 1)
            ) meta
            FROM job_queue.task_in_process
            WHERE id = id_
        )
        SELECT job_queue.enqueue_payload(
            array[(value || circuit_attempt.meta)],
            at_ := now()
                + make_interval(secs =>
                    round(
                        random() *
                        least(
                            (cbs.drip_frequency/1000::int)
                            + cbs.exponentiation_factor ^ ((circuit_attempt.meta->'meta'->'circuitBreakerAttempts')::int),
                            cbs.exponentiation_cap
                        )::bigint
                    )
                ),
            priority_ := priority_,
            attempts_ := attempt_,
            request_id_ := (task_in_process.value)->'meta'->'requestId'
        )
        FROM job_queue.task_in_process
        JOIN circuit_attempt ON true
        JOIN job_queue.circuit_breaker_state cbs ON cbs.label = label_
        WHERE id = id_;
    END;

COMMIT;
