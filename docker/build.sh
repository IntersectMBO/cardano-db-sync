#!/usr/bin/env bash
set -euo pipefail

# Dereference the Nix result symlink
TARBALL=$(basename "$(readlink -f result/*.tar.gz)")
cp -L result/*.tar.gz docker/

# Build with the exact tarball name as build arg
docker build --build-arg TARBALL="$TARBALL" -t cardano-db-sync:lightweight docker/

# Cleanup
rm -f docker/*.tar.gz
