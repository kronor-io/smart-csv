-- Deploy smart-csv:schemas to pg

BEGIN;

    CREATE SCHEMA IF NOT EXISTS smart_csv;
    CREATE SCHEMA IF NOT EXISTS job_queue;

    -- Helper to LISTEN on a dynamically-named channel (used by the job dequeuer).
    -- Must be in public schema so it's on the default search_path.
    CREATE OR REPLACE FUNCTION public.listen_on(channel text)
    RETURNS void AS $$
    BEGIN
        EXECUTE format(E'listen %I', channel);
    END
    $$ LANGUAGE plpgsql;

    CREATE OR REPLACE FUNCTION public.unlisten_on(channel text)
    RETURNS void AS $$
    BEGIN
        EXECUTE format(E'unlisten %I', channel);
    END
    $$ LANGUAGE plpgsql;

COMMIT;
