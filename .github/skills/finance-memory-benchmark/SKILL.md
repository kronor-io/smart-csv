---
name: finance-memory-benchmark
description: 'Implement or review smart-csv changes that must be validated with the finance snapshot memory benchmark. Use when asked to make a code change, then run the finance benchmark, compare baseline vs candidate memory, verify CSV identity, and summarize the results.'
argument-hint: 'Describe the code change or benchmark goal'
user-invocable: true
---

# Finance Memory Benchmark

Use this skill for repo-specific work where a smart-csv change should be followed by the finance snapshot benchmark and an analysis of the outcome.

## When To Use

- The user wants a smart-csv change implemented and then benchmarked against the finance snapshot.
- The user wants a baseline vs candidate memory comparison for a real finance export.
- The user wants confirmation that the generated CSV output is identical between runs.
- The user wants a short analysis of the memory delta after a code change.

## Repo Workflow

The benchmark entrypoint is `_dev/compare-finance-memory.sh`.

The request file is `request.json`. It is self-contained and includes:

- the GraphQL mutation payload
- the authorization header
- `shardId`
- `recipient`

The benchmark writes a timestamped result directory under `benchmark-results/` and keeps separate per-run folders:

- `baseline/`
- `candidate/`

Each run keeps its local CSV file, heap profile, GC log, eventlog, stdout log, response JSON, status TSV, and summary JSON.

## Procedure

1. Make the requested code change first.
2. Check whether the candidate binary must be rebuilt.
3. Build the candidate binary when the touched code affects `smart-csv` or `smart-csv-runner`.
4. Run the finance benchmark with `bash _dev/compare-finance-memory.sh --request-json request.json`.
5. Read the generated `comparison.md`, `csv-comparison.json`, and the per-run summary JSON files.
6. Report the peak heap comparison, CSV equality result, and the artifact directory.

## Build And Run Guidance

- Prefer rebuilding the candidate binary before the benchmark unless the user explicitly wants to reuse an existing binary.
- Use `direnv exec . cabal build smart-csv-runner` before the benchmark when the candidate should reflect current source.
- The benchmark script already handles:
  - creating a clean baseline worktree
  - building the baseline binary
  - resetting smart-csv and job queue state between runs
  - downloading both CSV files
  - comparing the CSV files byte-for-byte
  - producing a markdown comparison report

## Analysis Checklist

When summarizing results, include:

- whether baseline and candidate both finished with `DONE`
- whether the CSVs are identical
- baseline peak heap in MB
- candidate peak heap in MB
- the delta in MB or percentage when it is meaningful
- the result directory path

If useful, also call out the top peak categories from each run.

## Important Caveats

- The benchmark resets `smart_csv` and `job_queue` tables used by smart-csv, but it does not reset the finance snapshot data itself.
- If the candidate binary was not rebuilt after code changes, the comparison may be meaningless even if the script completes.
- If GraphQL requests fail because of permissions or schema access, verify that `request.json` still contains a valid `authorizationHeader`.
- If CSVs differ, the benchmark should fail nonzero and write `csv-diff.txt` alongside `csv-comparison.json`.

## Expected Outputs

Look for a directory like `benchmark-results/finance-memory-YYYYMMDD-HHMMSS/` containing:

- `comparison.md`
- `csv-comparison.json`
- `baseline/`
- `candidate/`

Use those files as the source of truth for the final summary.
