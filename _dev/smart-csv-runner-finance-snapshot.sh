#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"

# Run smart-csv-runner connected to the finance database snapshot on port 7435.
#
# First, create a snapshot in the finance project:
#   cd /path/to/finance
#   scripts/transient_db.sh create_temp_db
#
# Then run this script from smart-csv:
#   bash _dev/smart-csv-runner-finance-snapshot.sh

# shellcheck source=_dev/finance-snapshot-env.sh
source "$SCRIPT_DIR/finance-snapshot-env.sh"

BINARY_PATH="${BINARY_PATH:-$REPO_ROOT/dist-newstyle/build/aarch64-osx/ghc-9.12.2/smart-csv-runner-0.1.0.0/x/smart-csv-runner/noopt/build/smart-csv-runner/smart-csv-runner}"

if [ ! -f "$BINARY_PATH" ]; then
  echo "Binary not found at $BINARY_PATH"
  echo "Run 'direnv exec . cabal build smart-csv-runner' first"
  exit 1
fi

# Apply smart-csv migrations that are missing from the finance-vendored snapshot
# schema. The snapshot's smart_csv schema is created by finance's own migrations
# and is not tracked in this repo's sqitch registry, so we cannot `sqitch deploy`
# here; instead we apply the idempotent delta migrations directly. A fresh snapshot
# (transient_db.sh create_temp_db) comes back at finance's schema level, so this
# re-runs on every launch. All statements are ADD COLUMN / CREATE TABLE IF NOT
# EXISTS, so re-applying is a no-op.
#
# Only list ADDITIVE migrations that the runner code actually reads/writes (e.g.
# order_by adds a column the insert path depends on). Do NOT list destructive
# migrations such as drop_query_range_limit here: compare-finance-memory.sh runs a
# baseline binary and a candidate binary against the SAME snapshot, and a baseline
# built from a pre-removal ref still queries smart_csv.query_range_limit. Dropping
# that table in snapshot prep would break the baseline run.
SNAPSHOT_MIGRATIONS=(
  order_by
)

echo "Applying smart-csv snapshot migrations: ${SNAPSHOT_MIGRATIONS[*]}" >&2
for migration in "${SNAPSHOT_MIGRATIONS[@]}"; do
  psql "$FINANCE_SNAPSHOT_DB_URL" -v ON_ERROR_STOP=1 -q -f "$REPO_ROOT/database/deploy/$migration.sql"
done

exec "$BINARY_PATH" "$@"
