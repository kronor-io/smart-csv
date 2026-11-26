# Releases

`smart-csv-runner` images are published to `ghcr.io/kronor-io/smart-csv/smart-csv-runner` by the
`release` job in `.github/workflows/ci.yml`, only for tags.

| Tag | Image tags pushed | Allowed on |
|-----|-------------------|------------|
| `v1.2.3` | `1.2.3` and `latest` | a commit on `main` only; the release job fails otherwise |
| `kronor-rc-<anything>` | `kronor-rc-<anything>` only | any commit, typically on the `kronor` branch |

## Why `v*` tags are restricted

Deployments follow `:latest`, and they don't choose when to upgrade: every `v*` tag reaches them
as soon as it is pushed. A version must therefore leave their output and behaviour unchanged unless
the change has been agreed with them first. In practice, for anything tagged:

- Don't change the `smart_csv.generated_csv` or `smart_csv.smart_graphql_csv_generator` columns, the
  `INITIALIZED` / `DONE` / `ERROR` statuses, the stored `token_claims` shape, the `csv_report_flow`
  event names, or the REST request and response.
- Don't make the runner depend on a new column or migration. Deployments copy migrations into
  their own database on their own schedule, so the image can arrive before the migration does.
- New output behaviour is opt-in, with a default that reproduces today's file.

## The `kronor` branch

Kronor uses the `smart-csv` library from a long-lived `kronor` branch, pinned by commit, while the
shared export engine is built and proven there. CI runs for pushes and pull requests on it. It
never produces a version: its images are `kronor-rc-*` pre-releases, for trying a build in staging.

- `main` is merged into `kronor` weekly, and whenever the export loop changes on `main`.
- Changes that leave the runner's behaviour unchanged go to `main` first.
- When the branch is done it is merged into `main` once, and released as a new major version.
