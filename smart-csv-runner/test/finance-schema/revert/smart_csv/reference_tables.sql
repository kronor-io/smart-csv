-- Revert smart-csv:reference_tables from pg
BEGIN;
    DROP TABLE IF EXISTS smart_csv.merchant;
    DROP TABLE IF EXISTS smart_csv.organization;
    DROP TABLE IF EXISTS smart_csv.service_mail;
    DROP TABLE IF EXISTS smart_csv.report_status;
COMMIT;
