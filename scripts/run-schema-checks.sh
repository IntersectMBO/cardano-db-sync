#!/usr/bin/env bash
# Run all schema test scripts against a database.
#
# Usage: ./scripts/run-schema-checks.sh <dbname>
# Example: ./scripts/run-schema-checks.sh cexplorer
#
# All three scripts run even if one fails. Exits 1 if any of them failed (a check
# FAILed, or psql could not run it), 0 if all passed.

set -uo pipefail

DB="${1:?Usage: $0 <dbname>}"
SCRIPTS_DIR="$(cd "$(dirname "$0")" && pwd)"
failed=""

run_check() {
  local label=$1 script=$2
  echo "Running ${label} tests on ${DB}..."
  # ON_ERROR_STOP so the script's failure RAISE propagates as a non-zero exit.
  if ! psql -v ON_ERROR_STOP=1 -d "$DB" -f "$SCRIPTS_DIR/$script"; then
    failed="${failed:+$failed, }$label"
  fi
  echo ""
}

run_check "referential integrity" test-referential-integrity.sql
run_check "uniqueness" test-uniqueness.sql
run_check "value domain" test-value-domains.sql

if [ -n "$failed" ]; then
  echo "Schema checks FAILED: ${failed}"
  exit 1
fi
echo "All schema checks passed."
