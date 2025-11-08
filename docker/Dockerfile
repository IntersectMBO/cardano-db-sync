# Lightweight Dockerfile for a static cardano-db-sync binary
#
# Usage (example):
# 1. Build a static executable with Nix:
#      nix build .#cardano-db-sync-linux
# 2. Copy the executable into this folder (follow symlinks):
#      cp -L result/bin/cardano-db-sync docker/
# 3. Build the image:
#      docker build -t cardano-db-sync:lightweight docker/
#
# Important: this Dockerfile assumes the binary is a fully static executable
# (statically linked). If it's not static, use a small base image (see docs).

FROM scratch

# Add the static executable produced by Nix. The builder step places
# the binary next to this Dockerfile before running `docker build`.
COPY cardano-db-sync /usr/local/bin/cardano-db-sync

# Minimal entrypoint; configuration should be supplied via mounted files
# or environment variables. We avoid baked-in configs to keep images small
# and easier to customize.
ENTRYPOINT ["/usr/local/bin/cardano-db-sync"]

# Default to printing help if no args supplied (preserves normal CLI behaviour)
CMD ["--help"]

