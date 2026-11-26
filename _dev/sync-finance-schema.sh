#!/usr/bin/env bash
# Copy the smart_csv migrations Finance deploys into smart-csv-runner/test/finance-schema,
# with the lines of Finance's sqitch plan that deploy them.
#
# Finance copies this repository's migrations into its own database on its own
# schedule, so its smart_csv schema can differ from database/. The integration tests
# also run against this copy (SMART_CSV_SCHEMA=finance in _dev/setup-db.sh), which
# catches a runner change that would not work on the schema Finance actually has.
#
# Usage: _dev/sync-finance-schema.sh <path to a Finance checkout>
set -euo pipefail

FINANCE_DIR="${1:?usage: $0 <path to a Finance checkout>}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="$(dirname "$SCRIPT_DIR")/smart-csv-runner/test/finance-schema"
PLAN="$FINANCE_DIR/sql/sqitch.plan"

[ -f "$PLAN" ] || { echo "No sqitch plan at $PLAN" >&2; exit 1; }

for dir in deploy revert verify; do
    rm -rf "${TARGET:?}/$dir"
    mkdir -p "$TARGET/$dir"
    cp -R "$FINANCE_DIR/sql/$dir/smart_csv" "$TARGET/$dir/"
done

# The smart_csv changes, in Finance's order, and the tags their reworks refer to
# (a rework names the tag it follows: smart_csv/job_queue_functions@2026-04-07-001).
# A kept tag lands after the last smart_csv change before it, as in Finance's plan.
{
    echo "%syntax-version=1.0.0"
    echo "%project=finance"
    echo
    awk '
        NR == FNR {
            if ($1 ~ /^smart_csv\//) {
                for (field = 2; field <= NF && $field != "#"; field++) {
                    if (match($field, /@[^] ]+/)) tags[substr($field, RSTART, RLENGTH)] = 1
                }
            }
            next
        }
        $1 ~ /^smart_csv\// { print; kept = 1; next }
        $1 ~ /^@/ && kept && ($1 in tags) { print }
    ' "$PLAN" "$PLAN"
} > "$TARGET/sqitch.plan"

cat > "$TARGET/sqitch.conf" <<'EOF'
[core]
    engine = pg
    top_dir = .
    plan_file = sqitch.plan
EOF

git -C "$FINANCE_DIR" log -1 --format='Copied from Finance commit %H (%cs).' > "$TARGET/SOURCE"
echo "Copied Finance's smart_csv schema to $TARGET"
cat "$TARGET/SOURCE"
