#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"

DB_URL="${DB_URL:-db:pg://smart_csv:smart_csv@localhost:5432/smart_csv}"
STATECHARTS_DIR="${STATECHARTS_DIR:-/tmp/statecharts}"

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

# 3. Deploy smart-csv migrations
echo "==> Deploying smart-csv migrations"
cd "$REPO_DIR"
sqitch deploy "$DB_URL" || true

echo "==> Database setup complete"
