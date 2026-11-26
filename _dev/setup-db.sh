#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"

DB_URL="${DB_URL:-db:pg://smart_csv:smart_csv@localhost:5432/smart_csv}"
STATECHARTS_DIR="${STATECHARTS_DIR:-/tmp/statecharts}"

# Which smart_csv schema to deploy: this repository's migrations, or the copy of the
# ones Finance has deployed (see _dev/sync-finance-schema.sh).
SMART_CSV_SCHEMA="${SMART_CSV_SCHEMA:-smart-csv}"
case "$SMART_CSV_SCHEMA" in
    smart-csv) MIGRATIONS_DIR="$REPO_DIR" ;;
    finance) MIGRATIONS_DIR="$REPO_DIR/smart-csv-runner/test/finance-schema" ;;
    *) echo "SMART_CSV_SCHEMA must be smart-csv or finance, not $SMART_CSV_SCHEMA" >&2; exit 1 ;;
esac

echo "==> Setting up database at $DB_URL"

# 1. Clone or update statecharts FSM infrastructure
if [ -d "$STATECHARTS_DIR/.git" ]; then
    echo "==> Updating statecharts in $STATECHARTS_DIR"
    git -C "$STATECHARTS_DIR" pull --ff-only
else
    echo "==> Cloning statecharts to $STATECHARTS_DIR"
    git clone https://github.com/kronor-io/statecharts.git "$STATECHARTS_DIR"
fi

# 2. Deploy statecharts FSM migrations
echo "==> Deploying statecharts FSM migrations"
cd "$STATECHARTS_DIR"
sqitch deploy "$DB_URL" || true  # allow "nothing to deploy"

# 3. Deploy the smart_csv schema
echo "==> Deploying the $SMART_CSV_SCHEMA smart_csv schema from $MIGRATIONS_DIR"
cd "$MIGRATIONS_DIR"
sqitch deploy "$DB_URL" || true  # allow "nothing to deploy"

# 4. Verify the migrations were applied
echo "==> Verifying the $SMART_CSV_SCHEMA smart_csv schema"
sqitch verify "$DB_URL"

echo "==> Database setup complete"
