#!/usr/bin/env bash
# Test the cardano-db-sync Docker image:
# - basic invocation (defaults to --help)
# - running with the original schema directory (should succeed)
# - running with a modified schema (should fail and report checksum mismatch)
# - check that the binary exists by bypassing the ENTRYPOINT
#
# Usage: ./test-image.sh [IMAGE] [SCHEMA_DIR]
# Defaults:
#   IMAGE=cardano-db-sync:lightweight
#   SCHEMA_DIR=./schema

set -uo pipefail

IMAGE="${1:-cardano-db-sync:lightweight}"
SCHEMA_DIR="${2:-./schema}"

TMPDIR=""
FAILED=0

cleanup() {
  rc=$?
  if [[ -n "$TMPDIR" && -d "$TMPDIR" ]]; then
    rm -rf "$TMPDIR"
  fi
  exit $rc
}
trap cleanup EXIT

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "required command '$1' not found" >&2; exit 2; }
}

require_cmd docker
require_cmd mktemp
require_cmd cp
require_cmd sed
require_cmd realpath

echo "Image: $IMAGE"
echo "Schema dir: $SCHEMA_DIR"

if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "ERROR: Docker image '$IMAGE' not found locally." >&2
  echo "Either build it or pull it before running this test script." >&2
  exit 3
fi

run_container_capture() {
  # args: container args...
  docker run --rm "$@" 2>&1
  return ${PIPESTATUS[0]:-0}
}

echo
echo "1) Basic invocation (no extra mounts) - should exit 0 (show help)"
out=$(run_container_capture "$IMAGE")
rc=$?
if [[ $rc -eq 0 ]]; then
  echo "PASS: container exited 0"
else
  echo "FAIL: container exited with $rc"
  echo "Output:"
  echo "-----"
  echo "$out"
  echo "-----"
  FAILED=$((FAILED+1))
fi

if [[ ! -d "$SCHEMA_DIR" ]]; then
  echo
  echo "WARNING: schema directory '$SCHEMA_DIR' not found; skipping schema-related tests."
else
  echo
  echo "2) Run with host schema mounted to /schema (expected: success)"
  out=$(run_container_capture -v "$(realpath "$SCHEMA_DIR")":/schema:ro "$IMAGE")
  rc=$?
  if [[ $rc -eq 0 ]]; then
    echo "PASS: container exited 0 with host schema"
  else
    echo "FAIL: container exited $rc with host schema"
    echo "Output:"
    echo "-----"
    echo "$out"
    echo "-----"
    FAILED=$((FAILED+1))
  fi

  echo
  echo "3) Run with a modified schema (expected: schema checksum mismatch -> non-zero and error text)"
  TMPDIR=$(mktemp -d)
  cp -a "$SCHEMA_DIR"/. "$TMPDIR"/
  # Modify the schema to change checksum (add a small file)
  echo "injected-change-$(date +%s)" > "$TMPDIR"/.injected-change

  out=$(run_container_capture -v "$TMPDIR":/schema:ro "$IMAGE")
  rc=$?
  if [[ $rc -ne 0 && "$out" == *"schema checksum mismatch"* ]]; then
    echo "PASS: schema mismatch detected as expected (exit $rc)"
  else
    echo "FAIL: expected schema mismatch (non-zero exit and 'schema checksum mismatch' message)."
    echo "Exit code: $rc"
    echo "Output:"
    echo "-----"
    echo "$out"
    echo "-----"
    FAILED=$((FAILED+1))
  fi
fi

echo
echo "4) Binary presence check inside image (override ENTRYPOINT so we can inspect fs)"
# IMPORTANT: override the image ENTRYPOINT so our test runs directly in the container
if docker run --rm --entrypoint /bin/sh "$IMAGE" -c 'test -x /usr/local/bin/cardano-db-sync' >/dev/null 2>&1; then
  echo "PASS: /usr/local/bin/cardano-db-sync exists and is executable (entrypoint bypassed)"
  echo "  File info:"
  docker run --rm --entrypoint /bin/sh "$IMAGE" -c 'ls -la /usr/local/bin/cardano-db-sync || true; file /usr/local/bin/cardano-db-sync || true'
else
  echo "FAIL: /usr/local/bin/cardano-db-sync missing or not executable (even with entrypoint bypassed)"
  FAILED=$((FAILED+1))
fi

echo
if [[ $FAILED -eq 0 ]]; then
  echo "ALL TESTS PASSED"
else
  echo "SOME TESTS FAILED: $FAILED failure(s)"
  exit 4
fi

# cleanup happens via trap
