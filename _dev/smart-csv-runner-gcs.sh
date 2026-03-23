#!/usr/bin/env bash
set -euo pipefail

# GCS S3-compatible endpoint testing script.
#
# Prerequisites:
#   1. Create HMAC keys for your service account:
#      gcloud storage hmac create SA@PROJECT.iam.gserviceaccount.com --project=boozt-finance
#   2. Export the keys:
#      export AWS_ACCESS_KEY_ID="GOOG..."
#      export AWS_SECRET_ACCESS_KEY="..."
#
# Usage:
#   AWS_ACCESS_KEY_ID=GOOGxxx AWS_SECRET_ACCESS_KEY=xxx bash _dev/smart-csv-runner-gcs.sh

if [ -z "${AWS_ACCESS_KEY_ID:-}" ] || [ -z "${AWS_SECRET_ACCESS_KEY:-}" ]; then
  echo "ERROR: AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY must be set to GCS HMAC keys." >&2
  echo "Create them with: gcloud storage hmac create SA@PROJECT.iam.gserviceaccount.com --project=boozt-finance" >&2
  exit 1
fi

# Override S3 settings for GCS
export KRONOR_S3_BUCKET="smart-csv-test-sandbox"
export KRONOR_TEST_S3_ENDPOINT_HOSTNAME="storage.googleapis.com"
export KRONOR_TEST_S3_ENDPOINT_PORT="443"
export KRONOR_TEST_S3_ENDPOINT_TLS="true"
export KRONOR_AWS_FROM="env"

# Delegate everything else to the standard script
source "$(dirname "$0")/smart-csv-runner.sh"
