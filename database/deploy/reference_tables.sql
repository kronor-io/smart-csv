-- Deploy smart-csv:reference_tables to pg
-- Requires: schemas, extensions

BEGIN;

    -- Report status values
    CREATE TABLE IF NOT EXISTS smart_csv.report_status (
        name text_100 NOT NULL PRIMARY KEY,
        comment text NOT NULL
    );

    INSERT INTO smart_csv.report_status (name, comment) VALUES
        ('INITIALIZED', 'Report generation initialized.'),
        ('DONE', 'Report generation complete.'),
        ('ERROR', 'Something went wrong.')
    ON CONFLICT (name) DO UPDATE SET comment = excluded.comment;

    -- Service mail (for noreply email address)
    CREATE TABLE IF NOT EXISTS smart_csv.service_mail (
        name text NOT NULL PRIMARY KEY,
        email text NOT NULL
    );

    INSERT INTO smart_csv.service_mail (name, email) VALUES
        ('noreply', 'noreply@example.com')
    ON CONFLICT (name) DO UPDATE SET email = excluded.email;

COMMIT;
