#!/usr/bin/env bash
# Run all schema test scripts against a database.
#
# Usage: ./scripts/run-schema-checks.sh <dbname>
# Example: ./scripts/run-schema-checks.sh cexplorer

set -euo pipefail

DB="${1:?Usage: $0 <dbname>}"
SCRIPTS_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Running referential integrity tests on ${DB}..."
psql -d "$DB" -f "$SCRIPTS_DIR/test-referential-integrity.sql"

echo ""
echo "Running uniqueness tests on ${DB}..."
psql -d "$DB" -f "$SCRIPTS_DIR/test-uniqueness.sql"
