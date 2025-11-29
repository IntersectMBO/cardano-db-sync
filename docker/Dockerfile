# Lightweight Dockerfile for a static cardano-db-sync binary
#
# Usage (example):
# 1. Build a static executable with Nix:
#      nix build .#cardano-db-sync-linux
# 2. Build the image from the project root:
#      docker build -f docker/Dockerfile -t cardano-db-sync:lightweight .
#
# Note: Since result/ is a Nix symlink, Docker may have issues following it.
# If this fails, you can dereference the symlink first:
#      mkdir -p docker/build && cp -L result/*.tar.gz docker/build/
#      docker build -f docker/Dockerfile -t cardano-db-sync:lightweight .
#
# Important: this Dockerfile assumes the binary is a fully static executable
# (statically linked). If it's not static, use a small base image (see docs).

# Stage 1: Extract the binary from the tarball
FROM busybox:latest AS extractor
ARG TARBALL
# ADD automatically extracts tarballs
ADD ${TARBALL} /tmp/extract/
RUN find /tmp/extract -name cardano-db-sync -type f -exec mv {} /cardano-db-sync \; && \
    chmod +x /cardano-db-sync

# Stage 2: Minimal runtime image
FROM scratch
COPY --from=extractor /cardano-db-sync /usr/local/bin/cardano-db-sync
ENTRYPOINT ["/usr/local/bin/cardano-db-sync"]
CMD ["--help"]
