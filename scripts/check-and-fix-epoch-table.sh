#!/usr/bin/env bash
#
# Validate the epoch table and optionally fix mismatches. The fix only applies before
# 13.7.2.1, when epoch was a table.
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
#
# Exit status: 0 when there is nothing to fix or the fix was declined, 1 when mismatches
# remain, which on the view form is any mismatch.
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

# Since 13.7.2.1 epoch is a view, empty while epoch sync is not enabled.
EPOCH_KIND=$(psql -X --no-psqlrc -At -v ON_ERROR_STOP=1 \
  -c "SELECT relkind FROM pg_class WHERE oid = to_regclass('epoch')" "$DB_NAME")
if [[ "$EPOCH_KIND" == "v" ]]; then
  SYNC_ENABLED=$(psql -X --no-psqlrc -At -v ON_ERROR_STOP=1 \
    -c "SELECT coalesce((SELECT enabled FROM epoch_sync_enabled WHERE singleton), false)" "$DB_NAME")
  if [[ "$SYNC_ENABLED" != "t" ]]; then
    echo "Epoch sync is not enabled (disable_epoch is set, or db-sync has not started since"
    echo "the upgrade), so the epoch view is empty: nothing to validate."
    exit 0
  fi
fi

echo "Validating epoch data for database '$DB_NAME' (epoch_no=$EPOCH_NO)..."
OUTPUT=$(psql -X --no-psqlrc -v ON_ERROR_STOP=1 -v epoch_no="$EPOCH_NO" -f "$VALIDATE_SQL" "$DB_NAME")

# Header has 2 lines (column names + separator). Tail has "(N rows)".
# Count only data rows by filtering lines that look like data (start with a digit).
MISMATCHES=$(echo "$OUTPUT" | awk '/^[[:space:]]*[0-9]/ {c++} END {print c+0}')

if [[ "$MISMATCHES" -eq 0 ]]; then
  echo "No mismatches. Epoch data is consistent."
  exit 0
fi

echo
echo "Found $MISMATCHES mismatching epoch(s):"
echo "$OUTPUT"
echo

if [[ "$EPOCH_KIND" == "v" ]]; then
  echo "Error: epoch is a view since db-sync 13.7.2.1, so this script cannot repair it." >&2
  echo "Finished epochs are stored in epoch_finalized, which db-sync maintains;" >&2
  echo "these mismatches point to a db-sync bug, please report them." >&2
  exit 1
fi

# read fails at end of input; keep what it read (a last line without a newline) and
# treat no input at all as "no".
read -r -p "Apply fix? [y/N] " REPLY || true
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
