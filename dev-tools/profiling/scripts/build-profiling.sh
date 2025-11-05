#!/bin/bash
# build-profiling.sh
# Build cardano-db-sync with ghc-debug profiling support enabled

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROFILING_DIR="$(dirname "$SCRIPT_DIR")"
DEV_TOOLS_DIR="$(dirname "$PROFILING_DIR")"
CARDANO_DB_SYNC_DIR="$(dirname "$DEV_TOOLS_DIR")"

cd "$CARDANO_DB_SYNC_DIR"

echo "Building cardano-db-sync with profiling support..."
echo ""

cabal build --flag enable-ghc-debug exe:cardano-db-sync

DBSYNC_BIN=$(cabal list-bin exe:cardano-db-sync --flag enable-ghc-debug)

echo ""
echo "Build complete: $DBSYNC_BIN"
echo ""
