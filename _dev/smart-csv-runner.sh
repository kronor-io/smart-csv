#!/usr/bin/env bash
set -euo pipefail

# Database
export DB_HOST="${DB_HOST:-localhost}"
export DB_PORT="${DB_PORT:-5432}"
export DB_LISTENER_USER="${DB_LISTENER_USER:-smart_csv}"
export DB_LISTENER_PASSWORD="${DB_LISTENER_PASSWORD:-smart_csv}"
export DB_LISTENER_DATABASE="${DB_LISTENER_DATABASE:-smart_csv}"
export DB_DEQUEUER_USER="${DB_DEQUEUER_USER:-smart_csv}"
export DB_DEQUEUER_PASSWORD="${DB_DEQUEUER_PASSWORD:-smart_csv}"
export DB_DEQUEUER_DATABASE="${DB_DEQUEUER_DATABASE:-smart_csv}"
export DB_WORKER_USER="${DB_WORKER_USER:-smart_csv}"
export DB_WORKER_PASSWORD="${DB_WORKER_PASSWORD:-smart_csv}"
export DB_WORKER_DATABASE="${DB_WORKER_DATABASE:-smart_csv}"
export DB_REPLICA_CSV_USER="${DB_REPLICA_CSV_USER:-smart_csv}"
export DB_REPLICA_CSV_PASSWORD="${DB_REPLICA_CSV_PASSWORD:-smart_csv}"
export DB_REPLICA_CSV_DATABASE="${DB_REPLICA_CSV_DATABASE:-smart_csv}"

# API
export API_PORT="${API_PORT:-8000}"
export GRAPHQL_URL="${GRAPHQL_URL:-http://localhost:8080/v1/graphql}"
export PORTAL_URL="${PORTAL_URL:-http://localhost:3000}"
# JWT_SECRET must be base64-encoded (the jose library base64-decodes it to get the HMAC key).
# The raw key is the same as graphql.jwt_secret in the database / Hasura config.
export JWT_SECRET="${JWT_SECRET:-Q2x1cUZwQWpmOWVWY3kxZ1NucmNaV09JS2lHQm9MbDY1MUQ3VEUwbDREYz0=}"

# Email
export MAIL_DEV="${MAIL_DEV:-True}"
export MAIL_HOST="${MAIL_HOST:-localhost}"
export MAIL_PORT="${MAIL_PORT:-1025}"

# AWS / S3 (minio)
export AWS_ACCESS_KEY_ID="${AWS_ACCESS_KEY_ID:-smartcsvak}"
export AWS_SECRET_ACCESS_KEY="${AWS_SECRET_ACCESS_KEY:-smartcsvsk}"
export KRONOR_AWS_FROM="${KRONOR_AWS_FROM:-env}"
export KRONOR_S3_BUCKET="${KRONOR_S3_BUCKET:-smart-csv-reports}"
export KRONOR_SIGNED_URL_EXPIRY_TIME_IN_SECONDS="${KRONOR_SIGNED_URL_EXPIRY_TIME_IN_SECONDS:-3600}"
export KRONOR_TEST_S3_ENDPOINT_HOSTNAME="${KRONOR_TEST_S3_ENDPOINT_HOSTNAME:-localhost}"
export KRONOR_TEST_S3_ENDPOINT_PORT="${KRONOR_TEST_S3_ENDPOINT_PORT:-9000}"
export KRONOR_TEST_S3_ENDPOINT_TLS="${KRONOR_TEST_S3_ENDPOINT_TLS:-false}"

# OpenTelemetry
export OTEL_EXPORTER_OTLP_ENDPOINT="${OTEL_EXPORTER_OTLP_ENDPOINT:-http://localhost:4318}"
export OTEL_SERVICE_NAME="${OTEL_SERVICE_NAME:-smart-csv-runner}"

exec cabal run smart-csv-runner
