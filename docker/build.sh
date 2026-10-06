#!/usr/bin/env bash
set -euo pipefail

# Dereference the Nix result symlink and get the tarball basename
TARBALL=$(basename "$(readlink -f result/*.tar.gz)")

# copy tarball into docker build context
cp -L result/*.tar.gz docker/

# copy runtime schema into build context so it gets baked into the image
cp -a schema docker/

# compute a deterministic schema checksum (sorted file list -> combined sha)
SCHEMA_HASH=$(cd schema && find . -type f -print0 | LC_ALL=C sort -z | xargs -0 sha256sum | sha256sum | awk '{print $1}')

# Build with the exact tarball name and schema hash as build args
docker build --build-arg TARBALL="$TARBALL" --build-arg SCHEMA_HASH="$SCHEMA_HASH" -t cardano-db-sync:lightweight docker/

# Cleanup
rm -f docker/*.tar.gz
rm -rf docker/schema
