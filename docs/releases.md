# Releases

`smart-csv-runner` images are published to `ghcr.io/kronor-io/smart-csv/smart-csv-runner` by the
`release` job in `.github/workflows/ci.yml`, only for tags.

| Tag | Image tags pushed | Allowed on |
|-----|-------------------|------------|
| `v1.2.3` | `1.2.3` and `latest` | a commit made on `main` only (on its first-parent history, so not one that reached `main` through a merge); the release job fails otherwise |
| `rc-<name>` | `rc-<name>` only | any commit that has this release job, typically on the `kronor` branch |

For a tag, CI checks the tag in its first job (Check the release tag), and builds nothing if it's refused:

- A version tag is `v<major>.<minor>.<patch>` and nothing else (at most 9 digits each), so a
  version's image tag can never be a pre-release's.
- A pre-release's name becomes its image tag as it is, so after `rc-` it may only use letters,
  digits, `_`, `.` and `-`, at most 125 characters (Docker's limit is 128).

GitHub runs the workflow of the tagged commit. A branch cut from `main` before pre-releases existed
has a workflow that ignores `rc-*` tags, and its tag publishes nothing: merge `main` into that branch
first, then tag.

## Why `v*` tags are restricted

Deployments follow `:latest`, and they don't choose when to upgrade: every `v*` tag reaches them
as soon as it is pushed. A version must therefore leave their output and behaviour unchanged unless
the change has been agreed with them first. In practice, before tagging a `v*` version (an `rc-*`
pre-release reaches no deployment on its own, so it may try anything):

- Don't change the `smart_csv.generated_csv` or `smart_csv.smart_graphql_csv_generator` columns, the
  `INITIALIZED` / `DONE` / `ERROR` statuses, the stored `token_claims` shape, the `csv_report_flow`
  event names, or the REST request and response.
- Don't make the runner depend on a new column or migration. Deployments copy migrations into
  their own database on their own schedule, so the image can arrive before the migration does.
- New output behaviour is opt-in, with a default that reproduces today's file.

## The `kronor` branch

Kronor uses the `smart-csv` library from a long-lived `kronor` branch, pinned by commit, while the
shared export engine is built and proven there. CI runs for pushes and pull requests on it. It
never produces a version: its images are `rc-*` pre-releases, for trying a build in staging.

- `main` is merged into `kronor` weekly, and whenever the export loop changes on `main`.
- Changes that leave the runner's behaviour unchanged go to `main` first.
- When the branch is done it is merged into `main` once, and released as a new major version.
- That merge goes through a pull request, squashed like every other (the only merge method this
  repository allows), so `kronor`'s own commits never land on `main`'s first-parent history and
  can't be tagged as a version. If it is ever merged outside a pull request, use
  `git merge --no-ff kronor`: a fast-forward would put them there.
- A pull request retargeted to `kronor` (or from it) isn't tested against its new base until
  something triggers CI again: close and reopen it.
