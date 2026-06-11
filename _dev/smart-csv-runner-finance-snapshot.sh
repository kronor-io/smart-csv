#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"

# Run smart-csv-runner connected to the finance database snapshot on port 7435.
#
# First, create a snapshot in the finance project:
#   cd /Users/daquirm/projects/boozt/finance
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

exec "$BINARY_PATH" "$@"
