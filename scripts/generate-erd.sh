#!/usr/bin/env bash
# Generate the entity-relationship diagram doc/ERD.svg from the current schema.
#
# It builds a throwaway database, applies every migration in schema/, reads the
# resulting tables/columns, takes the foreign-key relationships from the migration
# SQL (the FK constraints are dropped at run time for insert speed, so we read the
# declarations rather than the live constraints), and renders a colour-coded SVG.
#
# Usage:   ./scripts/generate-erd.sh
# Output:  doc/ERD.svg
#
# Requirements: postgresql client (createdb/psql/dropdb), graphviz (dot), python3.
# In the nix dev shell these are already provided. Connection uses the standard
# PG* environment variables (PGHOST, PGUSER, ...); the default local socket works.
#
# Config via env:
#   ERD_DB      name of the throwaway database (default: cardano_db_sync_erd_tmp)
#   SCHEMA_DIR  migrations directory (default: schema)
#   OUT         output file (default: doc/ERD.svg)

set -euo pipefail

export PGOPTIONS="${PGOPTIONS:-} -c client_min_messages=warning" # quiet migration NOTICEs

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SCHEMA_DIR="${SCHEMA_DIR:-$REPO_DIR/schema}"
OUT="${OUT:-$REPO_DIR/doc/ERD.svg}"
ERD_DB="${ERD_DB:-cardano_db_sync_erd_tmp}"

work="$(mktemp -d)"
cleanup() { dropdb --if-exists "$ERD_DB" >/dev/null 2>&1 || true; rm -rf "$work"; }
trap cleanup EXIT

echo "Creating throwaway database '$ERD_DB' and applying migrations..."
dropdb --if-exists "$ERD_DB" >/dev/null 2>&1 || true
createdb "$ERD_DB"
for f in $(ls "$SCHEMA_DIR"/migration-*.sql | sort); do
  psql -q -v ON_ERROR_STOP=1 -d "$ERD_DB" -f "$f" >/dev/null
done

echo "Reading tables, columns and primary keys..."
psql -d "$ERD_DB" -tAF'|' -c "
  SELECT c.table_name, c.ordinal_position, c.column_name, c.data_type
  FROM information_schema.columns c
  JOIN information_schema.tables t
    ON t.table_schema = c.table_schema AND t.table_name = c.table_name
   AND t.table_type = 'BASE TABLE'
  WHERE c.table_schema = 'public'
  ORDER BY c.table_name, c.ordinal_position" > "$work/columns.txt"
psql -d "$ERD_DB" -tAF'|' -c "
  SELECT tc.table_name, kcu.column_name
  FROM information_schema.table_constraints tc
  JOIN information_schema.key_column_usage kcu ON kcu.constraint_name = tc.constraint_name
  WHERE tc.constraint_type = 'PRIMARY KEY' AND tc.table_schema = 'public'" > "$work/pks.txt"

echo "Collecting foreign-key relationships from the migration SQL..."
grep -rhoE 'ALTER TABLE "[a-z_]+" ADD CONSTRAINT "[^"]+" FOREIGN KEY\("[a-z_]+"\) REFERENCES "[a-z_]+"' "$SCHEMA_DIR"/*.sql \
  | sed -E 's/ALTER TABLE "([a-z_]+)" ADD CONSTRAINT "[^"]+" FOREIGN KEY\("([a-z_]+)"\) REFERENCES "([a-z_]+)"/\1|\2|\3/' \
  | sort -u > "$work/fk.txt"

echo "Generating graphviz source and rendering $OUT ..."
python3 "$SCRIPT_DIR/gen-erd.py" "$work/columns.txt" "$work/pks.txt" "$work/fk.txt" "$work/erd.dot"
dot -Tsvg "$work/erd.dot" -o "$OUT"

echo "Done: $OUT"
