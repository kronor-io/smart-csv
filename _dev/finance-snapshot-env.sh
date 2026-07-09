#!/usr/bin/env bash

# Shared environment for running smart-csv against the finance transient snapshot.

# Database (connects to transient snapshot)
if [ -z "${FINANCE_SNAPSHOT_DB_URL:-}" ]; then
	echo "FINANCE_SNAPSHOT_DB_URL is required for finance snapshot runs. Put it in .envrc.local or export it before running." >&2
	return 1 2>/dev/null || exit 1
fi

SNAPSHOT_URL="$FINANCE_SNAPSHOT_DB_URL"
export DB_LISTENER_URL="${DB_LISTENER_URL:-$SNAPSHOT_URL}"
export DB_DEQUEUER_URL="${DB_DEQUEUER_URL:-$SNAPSHOT_URL}"
export DB_WORKER_URL="${DB_WORKER_URL:-$SNAPSHOT_URL}"
export DB_REPLICA_CSV_URL="${DB_REPLICA_CSV_URL:-$SNAPSHOT_URL}"

# API
export API_PORT="${API_PORT:-8000}"
export GRAPHQL_URL="${GRAPHQL_URL:-http://localhost:8080/v1/graphql}"
export PORTAL_URL="${PORTAL_URL:-http://localhost:3000}"

# CSV generation limits (from feature/row-and-time-limits). Defaults match the
# in-code defaults in SmartCsvRunner.Env; override in .envrc.local or via the
# --csv-generation-* flags of compare-finance-memory.sh to exercise the caps.
export CSV_GENERATION_TIMEOUT_SECONDS="${CSV_GENERATION_TIMEOUT_SECONDS:-1800}"
export CSV_GENERATION_MAX_ROWS="${CSV_GENERATION_MAX_ROWS:-1000000}"
# JWT_SECRET must be base64-encoded (the jose library base64-decodes it to get the HMAC key).
# The raw key is the same as graphql.jwt_secret in the database / Hasura config.
if [ -z "${JWT_SECRET:-}" ]; then
	echo "JWT_SECRET is required for finance snapshot runs. Put it in .envrc.local or export it before running." >&2
	return 1 2>/dev/null || exit 1
fi
export JWT_SECRET

# Email
export MAIL_DEV="${MAIL_DEV:-True}"
export MAIL_HOST="${MAIL_HOST:-localhost}"
export MAIL_PORT="${MAIL_PORT:-1025}"

# AWS / S3 (minio)
export AWS_ACCESS_KEY_ID="${AWS_ACCESS_KEY_ID:-minioadmin}"
export AWS_SECRET_ACCESS_KEY="${AWS_SECRET_ACCESS_KEY:-minioadmin}"
export KRONOR_AWS_FROM="${KRONOR_AWS_FROM:-env}"
export KRONOR_S3_BUCKET="${KRONOR_S3_BUCKET:-smart-csv-local}"
export KRONOR_SIGNED_URL_EXPIRY_TIME_IN_SECONDS="${KRONOR_SIGNED_URL_EXPIRY_TIME_IN_SECONDS:-3600}"
export KRONOR_TEST_S3_ENDPOINT_HOSTNAME="${KRONOR_TEST_S3_ENDPOINT_HOSTNAME:-localhost}"
export KRONOR_TEST_S3_ENDPOINT_PORT="${KRONOR_TEST_S3_ENDPOINT_PORT:-9900}"
export KRONOR_TEST_S3_ENDPOINT_TLS="${KRONOR_TEST_S3_ENDPOINT_TLS:-false}"

# OpenTelemetry
export OTEL_EXPORTER_OTLP_ENDPOINT="${OTEL_EXPORTER_OTLP_ENDPOINT:-http://localhost:4318}"
export OTEL_SERVICE_NAME="${OTEL_SERVICE_NAME:-smart-csv-runner}"