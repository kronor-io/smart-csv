#!/usr/bin/env bash
set -euo pipefail

# Remove all Smart CSV jobs and generated reports from the finance transient
# snapshot DB. This clears the smart_csv report tables and the CSV entries in the
# job queue, leaving any non-CSV finance jobs untouched.
#
# Requires FINANCE_SNAPSHOT_DB_URL (put it in .envrc.local or export it), and a
# running snapshot tunnel (finance: scripts/transient_db.sh create_temp_db).
#
# Usage:
#   bash _dev/clear-finance-csv-jobs.sh

if [ -z "${FINANCE_SNAPSHOT_DB_URL:-}" ]; then
  echo "FINANCE_SNAPSHOT_DB_URL is required. Put it in .envrc.local or export it before running." >&2
  exit 1
fi

# Tag used for Smart CSV jobs in the job_queue payload/task value column.
CSV_JOB_TAG="SmartGraphqlCsvGenerate"

echo "Clearing Smart CSV jobs from $FINANCE_SNAPSHOT_DB_URL ..." >&2

psql "$FINANCE_SNAPSHOT_DB_URL" -v ON_ERROR_STOP=1 -v tag="$CSV_JOB_TAG" <<'SQL'
BEGIN;

\echo 'CSV job rows before cleanup:'
SELECT 'smart_csv.smart_graphql_csv_generator' AS relation, count(*) FROM smart_csv.smart_graphql_csv_generator
UNION ALL SELECT 'smart_csv.generated_csv', count(*) FROM smart_csv.generated_csv
UNION ALL SELECT 'smart_csv.email_registry', count(*) FROM smart_csv.email_registry
UNION ALL SELECT 'job_queue.payload', count(*) FROM job_queue.payload WHERE value->>'tag' = :'tag'
UNION ALL SELECT 'job_queue.task', count(*) FROM job_queue.task WHERE value->>'tag' = :'tag'
UNION ALL SELECT 'job_queue.task_in_process', count(*) FROM job_queue.task_in_process WHERE value->>'tag' = :'tag'
UNION ALL SELECT 'job_queue.failed_job', count(*) FROM job_queue.failed_job WHERE value->>'tag' = :'tag'
ORDER BY relation;

-- Remove CSV entries from the job queue (scoped by tag so non-CSV jobs are kept).
DELETE FROM job_queue.task_in_process WHERE value->>'tag' = :'tag';
DELETE FROM job_queue.task            WHERE value->>'tag' = :'tag';
DELETE FROM job_queue.failed_job      WHERE value->>'tag' = :'tag';
DELETE FROM job_queue.payload         WHERE value->>'tag' = :'tag';

-- The smart_csv report tables hold only CSV data, so truncate them wholesale.
TRUNCATE TABLE
  smart_csv.smart_graphql_csv_generator,
  smart_csv.generated_csv,
  smart_csv.email_registry
RESTART IDENTITY;

COMMIT;
SQL

echo "Done." >&2
