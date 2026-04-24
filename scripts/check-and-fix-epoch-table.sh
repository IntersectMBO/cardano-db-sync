#!/usr/bin/env bash
#
# Validate the epoch table and optionally fix mismatches.
#
# Usage:
#   scripts/check-and-fix-epoch-table.sh [epoch_no]
#
#   epoch_no: specific epoch to check (default: -1 = all epochs)
#
# Requires PGPASSFILE to be set (or ~/.pgpass configured), and expects the
# database name to be on the PGDATABASE env var, the first line of the pgpass
# file, or passed explicitly via psql_args.
#
# Examples:
#   PGPASSFILE=config/pgpass-mainnet scripts/check-and-fix-epoch-table.sh
#   PGPASSFILE=config/pgpass-mainnet scripts/check-and-fix-epoch-table.sh 620
set -euo pipefail

EPOCH_NO=${1:--1}
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
VALIDATE_SQL="$SCRIPT_DIR/validate-epoch-table.sql"
FIX_SQL="$SCRIPT_DIR/fix-epoch-table.sql"

if [[ ! -f "$VALIDATE_SQL" || ! -f "$FIX_SQL" ]]; then
  echo "Error: expected $VALIDATE_SQL and $FIX_SQL next to this script." >&2
  exit 1
fi

# Derive the DB name from PGDATABASE or pgpass (field 5 of first line).
DB_NAME=${PGDATABASE:-}
if [[ -z "$DB_NAME" && -n "${PGPASSFILE:-}" && -f "$PGPASSFILE" ]]; then
  DB_NAME=$(awk -F: 'NR==1{print $3}' "$PGPASSFILE")
fi
if [[ -z "$DB_NAME" ]]; then
  echo "Error: could not determine database name. Set PGDATABASE or PGPASSFILE." >&2
  exit 1
fi

echo "Validating epoch table for database '$DB_NAME' (epoch_no=$EPOCH_NO)..."
OUTPUT=$(psql -X --no-psqlrc -v ON_ERROR_STOP=1 -v epoch_no="$EPOCH_NO" -f "$VALIDATE_SQL" "$DB_NAME")

# Header has 2 lines (column names + separator). Tail has "(N rows)".
# Count only data rows by filtering lines that look like data (start with a digit).
MISMATCHES=$(echo "$OUTPUT" | awk '/^[[:space:]]*[0-9]/ {c++} END {print c+0}')

if [[ "$MISMATCHES" -eq 0 ]]; then
  echo "No mismatches. Epoch table is consistent."
  exit 0
fi

echo
echo "Found $MISMATCHES mismatching epoch(s):"
echo "$OUTPUT"
echo

read -r -p "Apply fix? [y/N] " REPLY
case "$REPLY" in
  [yY]|[yY][eE][sS])
    echo "Applying fix..."
    psql -X --no-psqlrc -v ON_ERROR_STOP=1 -v epoch_no="$EPOCH_NO" -f "$FIX_SQL" "$DB_NAME"
    echo "Fix applied. Re-validating..."
    RECHECK=$(psql -X --no-psqlrc -v ON_ERROR_STOP=1 -v epoch_no="$EPOCH_NO" -f "$VALIDATE_SQL" "$DB_NAME")
    REMAINING=$(echo "$RECHECK" | awk '/^[[:space:]]*[0-9]/ {c++} END {print c+0}')
    if [[ "$REMAINING" -eq 0 ]]; then
      echo "Epoch table is now consistent."
    else
      echo "Warning: $REMAINING mismatch(es) remain:" >&2
      echo "$RECHECK" >&2
      exit 1
    fi
    ;;
  *)
    echo "No changes applied."
    exit 0
    ;;
esac
