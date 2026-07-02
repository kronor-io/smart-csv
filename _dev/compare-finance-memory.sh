#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"

# shellcheck source=_dev/finance-snapshot-env.sh
source "$SCRIPT_DIR/finance-snapshot-env.sh"

usage() {
  cat <<'EOF'
Usage: bash _dev/compare-finance-memory.sh --request-json FILE [options]

Runs the same smart-csv job twice against the finance transient snapshot:
1. baseline binary from a clean worktree at HEAD (or an explicit binary)
2. candidate binary from the current worktree (or an explicit binary)

Required:
  --request-json FILE         JSON request body for POST /api/v1/csv/generate

Optional:
  --output-dir DIR            Output directory for reports
  --baseline-ref REF          Git ref for the clean baseline build (default: HEAD)
  --baseline-binary PATH      Use this baseline binary instead of building one
  --candidate-binary PATH     Use this candidate binary instead of building one
  --authorization-header HDR  Explicit Authorization header to use for both runs
  --shard-id ID               Override shardId if the request file does not contain it
  --recipient EMAIL           Override recipient if the request file does not contain it
  --timeout-seconds N         Wait time for job completion per run (default: 600)
  --keep-worktree             Keep the temporary baseline worktree
  --help                      Show this help

The request file can be either:
  1. the raw REST API JSON body
  2. an envelope of the form:
       {
         "authorizationHeader": "Bearer ...",
         "body": { ...raw request body... }
       }
  3. a GraphQL mutation payload with a top-level "variables" object; the script
     will map those variables into the REST API body and fill in missing
     shardId/recipient from CLI overrides when needed.

If no authorization header is supplied, the script signs a benchmark token using
JWT_SECRET and the shardId from the request body.
EOF
}

require_command() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Missing required command: $1" >&2
    exit 1
  fi
}

log() {
  printf '[compare-finance-memory] %s\n' "$*"
}

collect_descendant_pids() {
  local pid="$1"
  local child_pid

  while IFS= read -r child_pid; do
    [ -n "$child_pid" ] || continue
    collect_descendant_pids "$child_pid"
    printf '%s\n' "$child_pid"
  done < <(pgrep -P "$pid" || true)
}

kill_process_tree() {
  local signal="$1"
  local pid="$2"
  local child_pid

  while IFS= read -r child_pid; do
    [ -n "$child_pid" ] || continue
    kill "-$signal" "$child_pid" >/dev/null 2>&1 || true
  done < <(collect_descendant_pids "$pid")

  kill "-$signal" "$pid" >/dev/null 2>&1 || true
}

kill_descendants() {
  local signal="$1"
  local pid="$2"
  local child_pid
  local had_descendants=0

  while IFS= read -r child_pid; do
    [ -n "$child_pid" ] || continue
    had_descendants=1
    kill "-$signal" "$child_pid" >/dev/null 2>&1 || true
  done < <(collect_descendant_pids "$pid")

  if [ "$had_descendants" -eq 0 ]; then
    kill "-$signal" "$pid" >/dev/null 2>&1 || true
  fi
}

stop_runner() {
  local pid="$1"
  if ! kill -0 "$pid" >/dev/null 2>&1; then
    return 0
  fi

  kill_descendants TERM "$pid"
  for _ in $(seq 1 15); do
    if ! kill -0 "$pid" >/dev/null 2>&1; then
      wait "$pid" 2>/dev/null || true
      return 0
    fi
    sleep 1
  done

  kill_process_tree KILL "$pid"
  wait "$pid" 2>/dev/null || true
}

cleanup_on_exit() {
  if [ -n "${RUNNER_PID:-}" ]; then
    stop_runner "$RUNNER_PID"
  fi

  if [ -n "${BASELINE_WORKTREE:-}" ] && [ "$KEEP_WORKTREE" != "1" ]; then
    git worktree remove --force "$BASELINE_WORKTREE" >/dev/null 2>&1 || true
  fi
}

normalize_request_json() {
  python3 - "$REQUEST_JSON" "$REQUEST_BODY_FILE" "$REQUEST_META_FILE" <<'PY'
import json
import sys

src, body_path, meta_path = sys.argv[1:4]
with open(src, 'r', encoding='utf-8') as handle:
    data = json.load(handle)

authorization = None

if isinstance(data, dict) and 'body' in data:
  body = data['body']
  authorization = data.get('authorizationHeader')
elif isinstance(data, dict) and isinstance(data.get('variables'), dict):
  variables = data['variables']
  body = {
    'shardId': variables.get('shardId'),
    'recipient': variables.get('recipient'),
    'graphqlPaginationKey': variables.get('graphqlPaginationKey'),
    'graphqlQueryBody': variables.get('graphqlQueryBody'),
    'graphqlQueryVariables': variables.get('graphqlQueryVariables'),
    'columnConfig': variables.get('columnConfig'),
    'columnConfigName': variables.get('columnConfigName'),
  }
  authorization = data.get('authorizationHeader')
else:
  body = data

if not isinstance(body, dict):
    raise SystemExit('Request JSON must be an object or an envelope with a body object')

meta = {
    'authorizationHeader': authorization,
    'shardId': body.get('shardId'),
    'recipient': body.get('recipient')
}

with open(body_path, 'w', encoding='utf-8') as handle:
    json.dump(body, handle, separators=(',', ':'))

with open(meta_path, 'w', encoding='utf-8') as handle:
    json.dump(meta, handle, separators=(',', ':'))
PY
}

read_meta_field() {
  local field="$1"
  python3 - "$REQUEST_META_FILE" "$field" <<'PY'
import json
import sys

path, field = sys.argv[1:3]
with open(path, 'r', encoding='utf-8') as handle:
    data = json.load(handle)
value = data.get(field)
if value is None:
    sys.exit(1)
if isinstance(value, (dict, list)):
    print(json.dumps(value, separators=(',', ':')))
else:
    print(value)
PY
}

generate_auth_header() {
  local shard_id="$1"
  local recipient="$2"

  python3 - "$JWT_SECRET" "$shard_id" "$recipient" <<'PY'
import base64
import hashlib
import hmac
import json
import sys
import time
import uuid

secret_b64, shard_id, recipient = sys.argv[1:4]
secret = base64.b64decode(secret_b64)

header = {'alg': 'HS256', 'typ': 'JWT'}
issued_at = int(time.time())
payload = {
    'https://hasura.io/jwt/claims': {
        'x-hasura-default-role': 'smart-csv',
        'x-hasura-allowed-roles': ['smart-csv'],
        'x-hasura-shard-id': str(shard_id),
        'x-hasura-user': recipient,
    },
    'iat': issued_at,
    'exp': issued_at + 3600,
    'tid': str(uuid.uuid4()),
    'ttype': 'benchmark',
    'tname': None,
    'associated_email': recipient,
}

def base64url(value: bytes) -> str:
    return base64.urlsafe_b64encode(value).rstrip(b'=').decode('ascii')

signing_input = '.'.join([
    base64url(json.dumps(header, separators=(',', ':')).encode('utf-8')),
    base64url(json.dumps(payload, separators=(',', ':')).encode('utf-8')),
])
signature = base64url(hmac.new(secret, signing_input.encode('ascii'), hashlib.sha256).digest())

print(f'Bearer {signing_input}.{signature}')
PY
}

build_binary() {
  local repo_dir="$1"
  local log_file="$2"

  (
    cd "$repo_dir"
    if [ -f .envrc ]; then
      direnv allow . >/dev/null
    fi
    direnv exec . cabal build smart-csv-runner
    direnv exec . cabal list-bin smart-csv-runner
  ) >"$log_file" 2>&1

  tail -n 1 "$log_file"
}

cleanup_benchmark_state() {
  psql "$DB_WORKER_URL" -v ON_ERROR_STOP=1 <<'SQL' >/dev/null
TRUNCATE TABLE
  job_queue.task_in_process,
  job_queue.failed_job,
  job_queue.task,
  job_queue.payload
RESTART IDENTITY;

TRUNCATE TABLE
  smart_csv.smart_graphql_csv_generator,
  smart_csv.generated_csv,
  smart_csv.email_registry
RESTART IDENTITY;
SQL
}

wait_for_health() {
  local pid="$1"
  local health_url="http://127.0.0.1:${API_PORT}/health"

  for _ in $(seq 1 120); do
    if curl --silent --show-error --fail "$health_url" >/dev/null 2>&1; then
      return 0
    fi

    if ! kill -0 "$pid" >/dev/null 2>&1; then
      echo "Runner exited before it became healthy" >&2
      return 1
    fi

    sleep 1
  done

  echo "Timed out waiting for $health_url" >&2
  return 1
}

submit_request() {
  local authorization_header="$1"
  local response_file="$2"
  local endpoint="http://127.0.0.1:${API_PORT}/api/v1/csv/generate"
  local status_code

  status_code="$(curl --silent --show-error --output "$response_file" --write-out '%{http_code}' \
    -X POST \
    -H 'Content-Type: application/json' \
    -H "Authorization: $authorization_header" \
    --data @"$REQUEST_BODY_FILE" \
    "$endpoint")"

  if [ "$status_code" != "200" ]; then
    echo "Request failed with HTTP $status_code" >&2
    cat "$response_file" >&2
    return 1
  fi

  python3 - "$response_file" <<'PY'
import json
import sys

with open(sys.argv[1], 'r', encoding='utf-8') as handle:
    data = json.load(handle)
print(data['reportId'])
PY
}

wait_for_report() {
  local report_id="$1"
  local status_file="$2"
  local deadline=$((SECONDS + TIMEOUT_SECONDS))

  while [ "$SECONDS" -lt "$deadline" ]; do
    local row
    row="$(psql "$DB_WORKER_URL" -At -F $'\t' -v ON_ERROR_STOP=1 -c "SELECT status, COALESCE(link, ''), COALESCE(err_message, '') FROM smart_csv.generated_csv WHERE id = ${report_id}" 2>/dev/null || true)"
    if [ -n "$row" ]; then
      printf '%s\n' "$row" >"$status_file"
      IFS=$'\t' read -r status _ err_message <"$status_file"
      if [ "$status" = "DONE" ] || [ "$status" = "ERROR" ]; then
        if [ "$status" = "ERROR" ] && [ -n "$err_message" ]; then
          log "Report $report_id finished with ERROR: $err_message"
        fi
        return 0
      fi
    fi
    sleep 1
  done

  echo "Timed out waiting for report ${report_id}" >&2
  return 1
}

download_csv() {
  local label="$1"
  local status_file="$2"
  local output_file="$3"
  local link
  local status
  local error_message

  if [ ! -f "$status_file" ]; then
    echo "Missing status file for $label: $status_file" >&2
    return 1
  fi

  IFS=$'\t' read -r status link error_message <"$status_file"

  if [ "$status" != "DONE" ]; then
    echo "$label run did not finish with DONE status: $status ${error_message:-}" >&2
    return 1
  fi

  if [ -z "$link" ]; then
    echo "$label run completed without a CSV link" >&2
    return 1
  fi

  curl --silent --show-error --fail --location "$link" --output "$output_file"
}

summarize_run() {
  local label="$1"
  local run_dir="$2"
  local binary_path="$3"
  local report_id="$4"
  local summary_file="$5"

  python3 - "$label" "$run_dir/${label}.hp" "$run_dir/${label}.gc.log" "$run_dir/${label}.system.log" "$binary_path" "$report_id" "$run_dir/${label}.status.tsv" "$summary_file" <<'PY'
import json
import re
import sys
from pathlib import Path

label, hp_path, gc_path, system_path, binary_path, report_id, status_path, out_path = sys.argv[1:9]

def parse_hp(path: Path):
    peak_total = 0
    peak_time = 0.0
    peak_entries = {}
    sample_totals = []
    in_sample = False
    current_time = 0.0
    current_total = 0
    current_entries = {}

    with path.open('r', encoding='utf-8') as handle:
        for raw_line in handle:
            line = raw_line.rstrip('\n')
            if line.startswith('BEGIN_SAMPLE '):
                in_sample = True
                current_time = float(line.split()[1])
                current_total = 0
                current_entries = {}
                continue
            if line.startswith('END_SAMPLE '):
                if in_sample:
                    sample_totals.append((current_time, current_total))
                    if current_total > peak_total:
                        peak_total = current_total
                        peak_time = current_time
                        peak_entries = dict(current_entries)
                in_sample = False
                continue
            if not in_sample or not line or line.startswith(('JOB ', 'DATE ', 'SAMPLE_UNIT ', 'VALUE_UNIT ')):
                continue

            parts = line.rsplit(None, 1)
            if len(parts) != 2:
                continue
            name, value = parts
            current_entries[name] = current_entries.get(name, 0) + int(value)
            current_total += int(value)

    top_entries = sorted(peak_entries.items(), key=lambda item: item[1], reverse=True)[:10]
    return {
        'peakBytes': peak_total,
        'peakMB': peak_total / (1024 * 1024),
        'peakTimeSeconds': peak_time,
        'samples': len(sample_totals),
        'topPeakEntries': [
            {'name': name, 'bytes': value, 'mb': value / (1024 * 1024)}
            for name, value in top_entries
        ],
    }


def parse_gc(path: Path):
    result = {}
    gen1_max_pause_seconds = None
    gen1_avg_pause_seconds = None

    with path.open('r', encoding='utf-8') as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line:
                continue
            match = re.match(r'^([0-9,]+) bytes allocated in the heap$', line)
            if match:
                result['bytesAllocated'] = int(match.group(1).replace(',', ''))
                continue
            match = re.match(r'^([0-9,]+) bytes copied during GC$', line)
            if match:
                result['bytesCopied'] = int(match.group(1).replace(',', ''))
                continue
            match = re.match(r'^([0-9,]+) bytes maximum residency \([0-9,]+ sample\(s\)\)$', line)
            if match:
                result['maximumResidencyBytes'] = int(match.group(1).replace(',', ''))
                continue
            match = re.match(r'^([0-9,]+) bytes maximum residency \([0-9]+ sample\(s\)\)$', line)
            if match:
                result['maximumResidencyBytes'] = int(match.group(1).replace(',', ''))
                continue
            match = re.match(r'^([0-9,]+) bytes maximum slop$', line)
            if match:
                result['maximumSlopBytes'] = int(match.group(1).replace(',', ''))
                continue
            match = re.match(r'^([0-9,]+) MiB total memory in use \([0-9,]+ MiB lost due to fragmentation\)$', line)
            if match:
                result['totalMemoryInUseMiB'] = int(match.group(1).replace(',', ''))
                continue
            match = re.match(r'^Gen\s+1\s+.*?([0-9.]+)s\s+([0-9.]+)s$', line)
            if match:
                gen1_avg_pause_seconds = float(match.group(1))
                gen1_max_pause_seconds = float(match.group(2))
                continue
            match = re.match(r'^Productivity\s+([0-9.]+)% of total user, ([0-9.]+)% of total elapsed$', line)
            if match:
                result['productivityUserPct'] = float(match.group(1))
                result['productivityElapsedPct'] = float(match.group(2))
                continue

    if gen1_avg_pause_seconds is not None:
        result['gen1AvgPauseSeconds'] = gen1_avg_pause_seconds
    if gen1_max_pause_seconds is not None:
        result['gen1MaxPauseSeconds'] = gen1_max_pause_seconds
    if 'maximumResidencyBytes' in result:
        result['maximumResidencyMB'] = result['maximumResidencyBytes'] / (1024 * 1024)
    return result


def parse_system(path: Path):
    result = {}
    if not path.exists():
        return result

    metric_patterns = [
        (r'^\s*([0-9.]+) real\s+([0-9.]+) user\s+([0-9.]+) sys$', ('realSeconds', float), ('userSeconds', float), ('sysSeconds', float)),
        (r'^\s*([0-9]+)\s+maximum resident set size$', ('maxResidentSetSizeBytes', int)),
        (r'^\s*([0-9]+)\s+page faults$', ('pageFaults', int)),
        (r'^\s*([0-9]+)\s+voluntary context switches$', ('voluntaryContextSwitches', int)),
        (r'^\s*([0-9]+)\s+involuntary context switches$', ('involuntaryContextSwitches', int)),
        (r'^\s*([0-9]+)\s+instructions retired$', ('instructionsRetired', int)),
        (r'^\s*([0-9]+)\s+cycles elapsed$', ('cyclesElapsed', int)),
        (r'^\s*([0-9]+)\s+peak memory footprint$', ('peakMemoryFootprintBytes', int)),
    ]

    with path.open('r', encoding='utf-8') as handle:
        for raw_line in handle:
            line = raw_line.rstrip('\n')
            for pattern, *fields in metric_patterns:
                match = re.match(pattern, line)
                if not match:
                    continue
                for idx, (name, conv) in enumerate(fields, start=1):
                    result[name] = conv(match.group(idx))
                break

    if 'maxResidentSetSizeBytes' in result:
        result['maxResidentSetSizeMB'] = result['maxResidentSetSizeBytes'] / (1024 * 1024)
    if 'peakMemoryFootprintBytes' in result:
        result['peakMemoryFootprintMB'] = result['peakMemoryFootprintBytes'] / (1024 * 1024)
    if 'instructionsRetired' in result:
        result['instructionsRetiredBillions'] = result['instructionsRetired'] / 1_000_000_000
    if 'cyclesElapsed' in result:
        result['cyclesElapsedBillions'] = result['cyclesElapsed'] / 1_000_000_000
    return result


status = {'status': 'UNKNOWN', 'link': '', 'errorMessage': ''}
status_path = Path(status_path)
if status_path.exists() and status_path.read_text(encoding='utf-8').strip():
    parts = status_path.read_text(encoding='utf-8').rstrip('\n').split('\t')
    status['status'] = parts[0]
    if len(parts) > 1:
        status['link'] = parts[1]
    if len(parts) > 2:
        status['errorMessage'] = parts[2]

summary = {
    'label': label,
    'binaryPath': binary_path,
    'reportId': int(report_id),
    'reportStatus': status['status'],
    'reportLink': status['link'],
    'reportErrorMessage': status['errorMessage'],
    'hp': parse_hp(Path(hp_path)),
    'gc': parse_gc(Path(gc_path)),
    'system': parse_system(Path(system_path)),
}

with open(out_path, 'w', encoding='utf-8') as handle:
    json.dump(summary, handle, indent=2, sort_keys=True)
PY
}

write_comparison_report() {
  local baseline_summary="$1"
  local candidate_summary="$2"
  local csv_comparison_file="$3"
  local output_file="$4"

  python3 - "$baseline_summary" "$candidate_summary" "$csv_comparison_file" "$output_file" <<'PY'
import json
import sys

baseline_path, candidate_path, csv_comparison_path, out_path = sys.argv[1:5]
with open(baseline_path, 'r', encoding='utf-8') as handle:
    baseline = json.load(handle)
with open(candidate_path, 'r', encoding='utf-8') as handle:
    candidate = json.load(handle)
with open(csv_comparison_path, 'r', encoding='utf-8') as handle:
    csv_comparison = json.load(handle)

def value(run, path, default=None):
    current = run
    for part in path:
        if not isinstance(current, dict) or part not in current:
            return default
        current = current[part]
    return current

def fmt_number(val, digits=2):
    if val is None:
        return 'n/a'
    return f'{val:.{digits}f}'

def fmt_delta(base, cand, digits=2, suffix=''):
    if base is None or cand is None:
        return 'n/a'
    delta = cand - base
    pct = 0.0 if base == 0 else (delta / base) * 100.0
    sign = '+' if delta >= 0 else ''
    return f'{sign}{delta:.{digits}f}{suffix} ({sign}{pct:.1f}%)'

metrics = [
    ('Report status', value(baseline, ['reportStatus']), value(candidate, ['reportStatus']), 'n/a'),
    ('CSV identical', 'yes' if csv_comparison.get('identical') else 'no', 'yes' if csv_comparison.get('identical') else 'no', 'n/a'),
    ('Peak heap (MB)', value(baseline, ['hp', 'peakMB']), value(candidate, ['hp', 'peakMB']), 'number'),
    ('Maximum residency (MB)', value(baseline, ['gc', 'maximumResidencyMB']), value(candidate, ['gc', 'maximumResidencyMB']), 'number'),
    ('Bytes allocated (GB)', None if value(baseline, ['gc', 'bytesAllocated']) is None else value(baseline, ['gc', 'bytesAllocated']) / (1024 ** 3), None if value(candidate, ['gc', 'bytesAllocated']) is None else value(candidate, ['gc', 'bytesAllocated']) / (1024 ** 3), 'number'),
    ('Bytes copied during GC (GB)', None if value(baseline, ['gc', 'bytesCopied']) is None else value(baseline, ['gc', 'bytesCopied']) / (1024 ** 3), None if value(candidate, ['gc', 'bytesCopied']) is None else value(candidate, ['gc', 'bytesCopied']) / (1024 ** 3), 'number'),
    ('Gen1 max pause (ms)', None if value(baseline, ['gc', 'gen1MaxPauseSeconds']) is None else value(baseline, ['gc', 'gen1MaxPauseSeconds']) * 1000.0, None if value(candidate, ['gc', 'gen1MaxPauseSeconds']) is None else value(candidate, ['gc', 'gen1MaxPauseSeconds']) * 1000.0, 'number'),
    ('Elapsed productivity (%)', value(baseline, ['gc', 'productivityElapsedPct']), value(candidate, ['gc', 'productivityElapsedPct']), 'number'),
    ('Wall time (s)', value(baseline, ['system', 'realSeconds']), value(candidate, ['system', 'realSeconds']), 'number'),
    ('User CPU (s)', value(baseline, ['system', 'userSeconds']), value(candidate, ['system', 'userSeconds']), 'number'),
    ('System CPU (s)', value(baseline, ['system', 'sysSeconds']), value(candidate, ['system', 'sysSeconds']), 'number'),
    ('Max RSS (MB)', value(baseline, ['system', 'maxResidentSetSizeMB']), value(candidate, ['system', 'maxResidentSetSizeMB']), 'number'),
    ('Peak footprint (MB)', value(baseline, ['system', 'peakMemoryFootprintMB']), value(candidate, ['system', 'peakMemoryFootprintMB']), 'number'),
    ('Page faults', value(baseline, ['system', 'pageFaults']), value(candidate, ['system', 'pageFaults']), 'number0'),
    ('Voluntary context switches', value(baseline, ['system', 'voluntaryContextSwitches']), value(candidate, ['system', 'voluntaryContextSwitches']), 'number0'),
    ('Involuntary context switches', value(baseline, ['system', 'involuntaryContextSwitches']), value(candidate, ['system', 'involuntaryContextSwitches']), 'number0'),
    ('Instructions retired (B)', value(baseline, ['system', 'instructionsRetiredBillions']), value(candidate, ['system', 'instructionsRetiredBillions']), 'number'),
    ('Cycles elapsed (B)', value(baseline, ['system', 'cyclesElapsedBillions']), value(candidate, ['system', 'cyclesElapsedBillions']), 'number'),
]

lines = []
lines.append('# Finance Snapshot Memory Comparison')
lines.append('')
lines.append('| Metric | Baseline | Candidate | Delta |')
lines.append('| --- | ---: | ---: | ---: |')
for name, base, cand, kind in metrics:
    if kind == 'number':
        lines.append(f'| {name} | {fmt_number(base)} | {fmt_number(cand)} | {fmt_delta(base, cand)} |')
    elif kind == 'number0':
        lines.append(f'| {name} | {fmt_number(base, 0)} | {fmt_number(cand, 0)} | {fmt_delta(base, cand, 0)} |')
    else:
        delta = 'same' if base == cand else f'{base} -> {cand}'
        lines.append(f'| {name} | {base or "n/a"} | {cand or "n/a"} | {delta} |')

lines.append('')
for run in (baseline, candidate):
    lines.append(f'## {run["label"].capitalize()} peak heap')
    lines.append('')
    lines.append(f'- Binary: {run["binaryPath"]}')
    lines.append(f'- Report ID: {run["reportId"]}')
    if value(run, ['reportLink']):
        lines.append(f'- CSV link: {value(run, ["reportLink"])}')
    lines.append(f'- Peak heap: {fmt_number(value(run, ["hp", "peakMB"]))} MB at {fmt_number(value(run, ["hp", "peakTimeSeconds"]), 3)} s')
    if value(run, ['system', 'realSeconds']) is not None:
        lines.append(f'- Wall / user / sys: {fmt_number(value(run, ["system", "realSeconds"]), 2)} s / {fmt_number(value(run, ["system", "userSeconds"]), 2)} s / {fmt_number(value(run, ["system", "sysSeconds"]), 2)} s')
    if value(run, ['system', 'peakMemoryFootprintMB']) is not None:
        lines.append(f'- Peak footprint: {fmt_number(value(run, ["system", "peakMemoryFootprintMB"]))} MB')
    if value(run, ['system', 'maxResidentSetSizeMB']) is not None:
        lines.append(f'- Max RSS: {fmt_number(value(run, ["system", "maxResidentSetSizeMB"]))} MB')
    if value(run, ['reportErrorMessage']):
        lines.append(f'- Report error: {value(run, ["reportErrorMessage"])}')
    lines.append('')
    lines.append('| Peak category | MB |')
    lines.append('| --- | ---: |')
    for entry in value(run, ['hp', 'topPeakEntries'], [])[:8]:
        lines.append(f'| {entry["name"]} | {fmt_number(entry["mb"])} |')
    lines.append('')

lines.append('## CSV comparison')
lines.append('')
lines.append(f'- Identical: {"yes" if csv_comparison.get("identical") else "no"}')
lines.append(f'- Baseline SHA256: {csv_comparison.get("baselineSha256", "n/a")}')
lines.append(f'- Candidate SHA256: {csv_comparison.get("candidateSha256", "n/a")}')
if csv_comparison.get('diffPath'):
    lines.append(f'- Diff file: {csv_comparison["diffPath"]}')
lines.append('')

with open(out_path, 'w', encoding='utf-8') as handle:
    handle.write('\n'.join(lines) + '\n')
PY
}

compare_csv_outputs() {
  local baseline_csv="$1"
  local candidate_csv="$2"
  local comparison_json="$3"
  local diff_file="$4"

  python3 - "$baseline_csv" "$candidate_csv" "$comparison_json" "$diff_file" <<'PY'
import difflib
import hashlib
import json
import sys
from pathlib import Path

baseline_path, candidate_path, comparison_path, diff_path = [Path(arg) for arg in sys.argv[1:5]]
baseline_bytes = baseline_path.read_bytes()
candidate_bytes = candidate_path.read_bytes()

result = {
    'baselinePath': str(baseline_path),
    'candidatePath': str(candidate_path),
    'baselineSha256': hashlib.sha256(baseline_bytes).hexdigest(),
    'candidateSha256': hashlib.sha256(candidate_bytes).hexdigest(),
    'identical': baseline_bytes == candidate_bytes,
}

if baseline_bytes != candidate_bytes:
    baseline_text = baseline_bytes.decode('utf-8', errors='replace').splitlines()
    candidate_text = candidate_bytes.decode('utf-8', errors='replace').splitlines()
    diff = '\n'.join(
        difflib.unified_diff(
            baseline_text,
            candidate_text,
            fromfile='baseline.csv',
            tofile='candidate.csv',
            lineterm=''
        )
    )
    diff_path.write_text(diff + ('\n' if diff else ''), encoding='utf-8')
    result['diffPath'] = str(diff_path)

with comparison_path.open('w', encoding='utf-8') as handle:
    json.dump(result, handle, indent=2, sort_keys=True)
PY
}

run_profile() {
  local label="$1"
  local binary_path="$2"
  local run_dir="$3"
  local response_file="$run_dir/${label}.response.json"
  local status_file="$run_dir/${label}.status.tsv"
  local summary_file="$run_dir/${label}-summary.json"
  local csv_file="$run_dir/${label}.csv"
  local stdout_log="$run_dir/${label}.stdout.log"
  local system_log="$run_dir/${label}.system.log"
  local report_id

  mkdir -p "$run_dir"

  cleanup_benchmark_state

  log "Starting $label binary: $binary_path"
  GHCRTS="-S${run_dir}/${label}.gc.log -hT -i0.05 -po${run_dir}/${label} -l-au -ol${run_dir}/${label}.eventlog" \
    BINARY_PATH="$binary_path" \
    /usr/bin/time -l \
      bash -c 'exec bash "$1" >"$2" 2>&1' _ "$SCRIPT_DIR/smart-csv-runner-finance-snapshot.sh" "$stdout_log" \
      2>"$system_log" &
  RUNNER_PID="$!"

  wait_for_health "$RUNNER_PID"
  report_id="$(submit_request "$AUTHORIZATION_HEADER" "$response_file")"
  log "$label report id: $report_id"

  wait_for_report "$report_id" "$status_file"
  download_csv "$label" "$status_file" "$csv_file"
  stop_runner "$RUNNER_PID"
  RUNNER_PID=""

  summarize_run "$label" "$run_dir" "$binary_path" "$report_id" "$summary_file"
}

REQUEST_JSON=""
OUTPUT_DIR=""
BASELINE_REF="HEAD"
BASELINE_BINARY=""
CANDIDATE_BINARY=""
AUTHORIZATION_HEADER=""
SHARD_ID_OVERRIDE=""
RECIPIENT_OVERRIDE=""
TIMEOUT_SECONDS=600
KEEP_WORKTREE=0
RUNNER_PID=""
BASELINE_WORKTREE=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --request-json)
      REQUEST_JSON="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --baseline-ref)
      BASELINE_REF="$2"
      shift 2
      ;;
    --baseline-binary)
      BASELINE_BINARY="$2"
      shift 2
      ;;
    --candidate-binary)
      CANDIDATE_BINARY="$2"
      shift 2
      ;;
    --authorization-header)
      AUTHORIZATION_HEADER="$2"
      shift 2
      ;;
    --shard-id)
      SHARD_ID_OVERRIDE="$2"
      shift 2
      ;;
    --recipient)
      RECIPIENT_OVERRIDE="$2"
      shift 2
      ;;
    --timeout-seconds)
      TIMEOUT_SECONDS="$2"
      shift 2
      ;;
    --keep-worktree)
      KEEP_WORKTREE=1
      shift
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [ -z "$REQUEST_JSON" ]; then
  usage >&2
  exit 1
fi

require_command git
require_command direnv
require_command curl
require_command pgrep
require_command psql
require_command python3

if [ ! -f "$REQUEST_JSON" ]; then
  echo "Request JSON not found: $REQUEST_JSON" >&2
  exit 1
fi

if [ -z "$OUTPUT_DIR" ]; then
  OUTPUT_DIR="$REPO_ROOT/benchmark-results/finance-memory-$(date +%Y%m%d-%H%M%S)"
fi

mkdir -p "$OUTPUT_DIR"
REQUEST_BODY_FILE="$OUTPUT_DIR/request-body.json"
REQUEST_META_FILE="$OUTPUT_DIR/request-meta.json"
cp "$REQUEST_JSON" "$OUTPUT_DIR/request.original.json"

trap cleanup_on_exit EXIT

normalize_request_json

if [ -z "$AUTHORIZATION_HEADER" ]; then
  AUTHORIZATION_HEADER="$(read_meta_field authorizationHeader || true)"
fi

REQUEST_SHARD_ID="$(read_meta_field shardId || true)"
REQUEST_RECIPIENT="$(read_meta_field recipient || true)"

if [ -n "$SHARD_ID_OVERRIDE" ]; then
  REQUEST_SHARD_ID="$SHARD_ID_OVERRIDE"
fi

if [ -n "$RECIPIENT_OVERRIDE" ]; then
  REQUEST_RECIPIENT="$RECIPIENT_OVERRIDE"
fi

if [ -z "$REQUEST_RECIPIENT" ]; then
  REQUEST_RECIPIENT="benchmark@example.com"
fi

if [ -z "$AUTHORIZATION_HEADER" ]; then
  if [ -z "$REQUEST_SHARD_ID" ]; then
    echo "Request body is missing shardId, and no authorization header was provided" >&2
    exit 1
  fi
  AUTHORIZATION_HEADER="$(generate_auth_header "$REQUEST_SHARD_ID" "$REQUEST_RECIPIENT")"
fi

python3 - "$REQUEST_BODY_FILE" "$REQUEST_SHARD_ID" "$REQUEST_RECIPIENT" <<'PY'
import json
import sys

path, shard_id, recipient = sys.argv[1:4]
with open(path, 'r', encoding='utf-8') as handle:
  body = json.load(handle)

if body.get('shardId') is None:
  body['shardId'] = int(shard_id) if shard_id.isdigit() else shard_id
if body.get('recipient') is None:
  body['recipient'] = recipient

required = ['shardId', 'recipient', 'graphqlPaginationKey', 'graphqlQueryBody', 'graphqlQueryVariables']
missing = [key for key in required if body.get(key) in (None, '')]
if missing:
  raise SystemExit(f'Missing required request fields after normalization: {", ".join(missing)}')

with open(path, 'w', encoding='utf-8') as handle:
  json.dump(body, handle, separators=(',', ':'))
PY

if [ -z "$BASELINE_BINARY" ]; then
  BASELINE_WORKTREE="$(mktemp -d "${TMPDIR:-/tmp}/smart-csv-baseline-XXXXXX")"
  log "Creating clean baseline worktree at $BASELINE_WORKTREE"
  git worktree add --detach "$BASELINE_WORKTREE" "$BASELINE_REF" >/dev/null
  BASELINE_BINARY="$(build_binary "$BASELINE_WORKTREE" "$OUTPUT_DIR/baseline-build.log")"
fi

if [ -z "$CANDIDATE_BINARY" ]; then
  CANDIDATE_BINARY="$(build_binary "$REPO_ROOT" "$OUTPUT_DIR/candidate-build.log")"
fi

if [ ! -f "$BASELINE_BINARY" ]; then
  echo "Baseline binary not found: $BASELINE_BINARY" >&2
  exit 1
fi

if [ ! -f "$CANDIDATE_BINARY" ]; then
  echo "Candidate binary not found: $CANDIDATE_BINARY" >&2
  exit 1
fi

printf '%s\n' "$BASELINE_BINARY" >"$OUTPUT_DIR/baseline-binary.txt"
printf '%s\n' "$CANDIDATE_BINARY" >"$OUTPUT_DIR/candidate-binary.txt"

run_profile baseline "$BASELINE_BINARY" "$OUTPUT_DIR/baseline"
run_profile candidate "$CANDIDATE_BINARY" "$OUTPUT_DIR/candidate"

compare_csv_outputs \
  "$OUTPUT_DIR/baseline/baseline.csv" \
  "$OUTPUT_DIR/candidate/candidate.csv" \
  "$OUTPUT_DIR/csv-comparison.json" \
  "$OUTPUT_DIR/csv-diff.txt"

write_comparison_report \
  "$OUTPUT_DIR/baseline/baseline-summary.json" \
  "$OUTPUT_DIR/candidate/candidate-summary.json" \
  "$OUTPUT_DIR/csv-comparison.json" \
  "$OUTPUT_DIR/comparison.md"

python3 - "$OUTPUT_DIR/csv-comparison.json" <<'PY'
import json
import sys

with open(sys.argv[1], 'r', encoding='utf-8') as handle:
    result = json.load(handle)

if not result.get('identical', False):
    raise SystemExit('CSV outputs differ; see csv-comparison.json and csv-diff.txt')
PY

cleanup_benchmark_state

log "Finished. Comparison report: $OUTPUT_DIR/comparison.md"
