# Smart CSV

## Why

Frontend applications often display data from GraphQL queries in tables. Users frequently need to export that same data as a CSV — for reporting, auditing, or feeding into spreadsheets. Rather than building a separate export backend for every table, Smart CSV lets the frontend reuse the exact same GraphQL query it already uses for display. Submit the query, and the service handles pagination, flattening, file generation, and delivery — no duplicate data-fetching logic required.

## What

A standalone service that turns any GraphQL query into a downloadable CSV file. It accepts the query via a REST API, paginates through all results via Hasura, flattens nested JSON into flat CSV rows, uploads the file to S3 using multipart upload, and emails a presigned download link to the requester.

## Architecture

```
REST API (Servant)  →  DB insert (generated_csv + smart_graphql_csv_generator)
                              ↓ (trigger)
                       Job enqueued (job_queue.payload)
                              ↓ (LISTEN/NOTIFY)
Worker (Streamly dequeuer)  →  Query Hasura GraphQL  →  Flatten to CSV
                              ↓
                       Upload to S3 (multipart)  →  Store presigned URL in DB
                              ↓
                       Send completion email
```

The project is split into two Cabal packages:

- **smart-csv** — Library with CSV generation primitives: validation, pagination, JSON flattening, and database statements (via [hasql-th](https://hackage.haskell.org/package/hasql-th) compile-time–checked SQL).
- **smart-csv-runner** — Binary containing the REST API server and background job worker, with S3 upload, email delivery, and OpenTelemetry tracing.

## Prerequisites (development only)

- [Nix](https://nixos.org/) with flakes enabled — provides GHC, Cabal, and all Haskell dependencies
- [Docker](https://www.docker.com/) and Docker Compose — runs local infrastructure (Postgres, Hasura, MinIO, etc.)
- [direnv](https://direnv.net/) (recommended) — automatically loads the Nix dev shell

## Quick Start

```bash
# Enter the Nix dev shell (or use direnv allow)
direnv allow

# Build everything
direnv exec . cabal build all

# Start local infrastructure (Postgres, Hasura, MinIO, MailHog, Jaeger)
docker compose up -d

# Set up the database (deploys statecharts FSM + smart-csv migrations)
direnv exec . bash _dev/setup-db.sh

# Start the service (REST API on :8000, worker in the same process)
direnv exec . bash _dev/smart-csv-runner.sh
```

## Local Infrastructure

| Service  | Port  | Purpose                                        |
|----------|-------|------------------------------------------------|
| Postgres | 5432  | Database (with pgjwt, semver, citext)          |
| Hasura   | 8080  | GraphQL engine (admin console at /console)     |
| MinIO    | 9000  | S3-compatible object storage                   |
| MailHog  | 8025  | Email capture (web UI for viewing sent emails) |
| Jaeger   | 16686 | Distributed trace viewer                       |

## Database Migrations

Migrations are managed with [Sqitch](https://sqitch.org/) and live in `database/deploy/` and `database/revert/`. The FSM infrastructure from [kronor-io/statecharts](https://github.com/kronor-io/statecharts) is deployed first — `_dev/setup-db.sh` handles both automatically.

```bash
# Deploy manually
direnv exec . sqitch deploy db:pg://smart_csv:smart_csv@localhost:5432/smart_csv

# Revert
direnv exec . sqitch revert db:pg://smart_csv:smart_csv@localhost:5432/smart_csv

# Reset everything (wipe volumes, recreate, re-deploy)
docker compose down -v && docker compose up -d
direnv exec . bash _dev/setup-db.sh
```

### Schemas

- `smart_csv` — Application tables: `generated_csv`, `smart_graphql_csv_generator`, `report_status`, `service_mail`, `column_config`
- `job_queue` — Job infrastructure: `payload`, `task`, `task_in_process`, `failed_job`
- `fsm` — State machine infrastructure (from [kronor-io/statecharts](https://github.com/kronor-io/statecharts))

## Testing

### Unit Tests (no infrastructure needed)

```bash
# Library tests — validation, flattening, pagination, error handling
direnv exec . cabal test smart-csv

# Runner unit tests — REST endpoint, JSON codec, WAI
direnv exec . cabal test smart-csv-runner-test --test-option="--pattern=Unit"
```

### Integration Tests (requires full stack)

```bash
# Start infrastructure + deploy migrations + start the service
docker compose up -d
direnv exec . bash _dev/setup-db.sh
direnv exec . bash _dev/smart-csv-runner.sh &

# Run integration tests
direnv exec . cabal test smart-csv-runner-test --test-option="--pattern=Integration"

# Or run everything at once
direnv exec . cabal test all
```

The integration tests create temporary tables in Hasura, insert test data, call the REST API, wait for the worker to generate a CSV, download it from S3, verify the contents, and clean up.

## API

### `GET /health`

Returns `200 OK` when the service is running.

### `POST /api/v1/csv/generate`

Accepts a JSON payload describing the GraphQL query to run, and returns a report ID. The worker picks up the job asynchronously.

## Configuration

The service reads configuration from environment variables. See `_dev/smart-csv-runner.sh` for the full list of variables used in local development. Key variables include:

- `KRONOR_SMART_CSV_PGPOOL_CONNSTRING` — Postgres connection string
- `KRONOR_SMART_CSV_HTTP_PORT` — REST API port (default: 8000)
- `KRONOR_SMART_CSV_GRAPHQL_ENDPOINT` — Hasura GraphQL endpoint
- `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` — S3 credentials
- `KRONOR_SMART_CSV_S3_BUCKET` — S3 bucket name
- `OTEL_EXPORTER_OTLP_ENDPOINT` — OpenTelemetry collector endpoint

## Tooling

- **Formatter**: [fourmolu](https://fourmolu.github.io/) (config in `fourmolu.yaml`)
- **Linter**: [hlint](https://github.com/ndmitchell/hlint)
- **Language**: GHC 9.12 / GHC2024
- **Build**: Cabal with Nix flake for dependencies

## License

BSD-3-Clause — see [LICENSE](LICENSE).
