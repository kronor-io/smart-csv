-- Revert smart-csv:extensions from pg
BEGIN;
    DROP DOMAIN IF EXISTS text_10000;
    DROP DOMAIN IF EXISTS text_100;
COMMIT;
