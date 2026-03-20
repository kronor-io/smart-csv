# Smart CSV — Agent Instructions

## Project Overview

Smart CSV is a standalone service that generates CSV files from GraphQL queries. It consists of:

- **smart-csv** — Haskell library with CSV generation primitives (validation, pagination, flattening, DB statements)
- **smart-csv-runner** — Haskell binary with REST API server + background job worker (S3 upload, email, OpenTelemetry tracing)

## Architecture

```
REST API (Servant)  →  DB insert (generated_csv + smart_graphql_csv_generator)
                              ↓ (trigger)
                       Job enqueued (job_queue.payload)
                              ↓ (NOTIFY)
Worker (Streamly dequeuer)  →  Query Hasura GraphQL  →  Flatten to CSV
                              ↓
                       Upload to S3 (multipart)  →  Store presigned URL in DB
                              ↓
                       Send completion email
```

### Database Schemas

- `smart_csv` — owned tables: `generated_csv`, `smart_graphql_csv_generator`, `report_status`, `service_mail`
- `job_queue` — job infrastructure: `payload`, `task`, `task_in_process`, `failed_job`
- `fsm` — state machine infrastructure (from [kronor-io/statecharts](https://github.com/kronor-io/statecharts))

### Key Haskell Modules

- `Kronor.SmartCsv.Statements` — SQL quasiquotes for DB operations
- `SmartCsvApi.RestServer` — Servant REST API (`/health`, `/api/v1/csv/generate`)
- `SmartCsvApi.Handler.SmartGraphqlCsvGenerator` — Request handler (validation, DB insert, token claims)
- `SmartCsvRunner.ThreadManager` — Starts API + worker threads with `Immortal`
- `SmartCsvRunner.CsvGeneration.Generate` — Core CSV generation (pagination, S3 upload, state machine notifications)
- `SmartCsvRunner.Dequeuer` — Job dequeue loop with error handling and circuit breakers
- `Kronor.Tracer` — OpenTelemetry tracing (spans, context propagation, Datadog resource detection)
- `Kronor.Logger` — Structured JSON logging via co-log-json

## Build & Development

### Prerequisites

- Nix with flakes enabled
- Docker and Docker Compose
- direnv (recommended)

### Commands

All commands should be prefixed with `direnv exec .` to use the Nix shell:

```bash
# Build everything
direnv exec . cabal build all

# Run unit tests (no infrastructure needed)
direnv exec . cabal test smart-csv

# Start local infrastructure
docker compose up -d

# Set up database (clones statecharts, runs sqitch migrations)
direnv exec . bash _dev/setup-db.sh

# Start the service
direnv exec . bash _dev/smart-csv-runner.sh

# Run all tests (requires docker-compose + running smart-csv-runner)
direnv exec . cabal test all
```

### Local Infrastructure (docker-compose)

| Service   | Port  | Purpose                              |
|-----------|-------|--------------------------------------|
| postgres  | 5432  | Database (with pgjwt, semver, citext)|
| hasura    | 8080  | GraphQL engine (admin console)       |
| minio     | 9000  | S3-compatible storage                |
| mailhog   | 8025  | Email capture (web UI)               |
| jaeger    | 16686 | Trace viewer (web UI)                |

### Database Migrations (Sqitch)

Migrations live in `database/deploy/` and `database/revert/`. The `sqitch.conf` at the repo root sets `top_dir = database`.

```bash
# Deploy
direnv exec . sqitch deploy db:pg://smart_csv:smart_csv@localhost:5432/smart_csv

# Revert
direnv exec . sqitch revert db:pg://smart_csv:smart_csv@localhost:5432/smart_csv
```

The FSM infrastructure from [kronor-io/statecharts](https://github.com/kronor-io/statecharts) must be deployed first. The `_dev/setup-db.sh` script handles this automatically.

## SQL Quasiquoters

Haskell SQL uses `hasql-th` quasiquoters:

- `[singletonStatement|...|]` — returns exactly one row
- `[maybeStatement|...|]` — returns zero or one row
- `[vectorStatement|...|]` — returns multiple rows
- `[resultlessStatement|...|]` — no result (INSERT/UPDATE/DELETE)

DDL (CREATE TABLE, etc.) cannot use quasiquoters — use `Hasql.Session.sql` instead.

All tables use the `smart_csv` schema (e.g., `smart_csv.generated_csv`). The `job_queue` schema is for job infrastructure.

## Testing

### Unit Tests

```bash
# Library tests (24 tests — validation, flattening, pagination, error handling)
direnv exec . cabal test smart-csv

# Runner unit tests (6 tests — REST endpoint, JSON codec, WAI)
direnv exec . cabal test smart-csv-runner-test --test-option="--pattern=Unit"
```

### Integration Tests

Require docker-compose stack + running smart-csv-runner:

```bash
# Start infrastructure
docker compose up -d
direnv exec . bash _dev/setup-db.sh
direnv exec . bash _dev/smart-csv-runner.sh &

# Run integration tests
direnv exec . cabal test smart-csv-runner-test --test-option="--pattern=Integration"
```

The integration test:
1. Creates a temporary table in Hasura
2. Inserts test data
3. Calls the REST API
4. Waits for the worker to generate the CSV
5. Downloads and verifies CSV contents
6. Cleans up the temporary table

## Conventions

- **Shard ID**: The `shard_id` column is a partition key for multi-tenant data isolation
- **Formatter**: fourmolu (configured in `fourmolu.yaml`)
- **Linter**: hlint
- **Language**: GHC2024 with extensions in `.cabal` files
