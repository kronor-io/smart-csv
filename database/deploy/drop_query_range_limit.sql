-- Deploy smart-csv:drop_query_range_limit to pg
-- Requires: query_range_limit

-- Row and time limits now bound export jobs at generation time, so the per-root
-- query range configuration is no longer used.

BEGIN;

    DROP TABLE IF EXISTS smart_csv.query_range_limit;

COMMIT;
