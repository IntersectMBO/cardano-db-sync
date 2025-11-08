Lightweight Docker image for cardano-db-sync
==========================================

This folder contains a minimal Docker context and `Dockerfile` intended for use with
a statically-built `cardano-db-sync` executable produced by Nix.

Why this exists
---------------

Building the full NixOS-based Docker image is a reproducible approach, but can lead
to large images and bundled configuration that make customization more difficult.
This lightweight approach lets users build a small, standard Docker image and supply
configuration at runtime via volumes or create their own derived images.

Quickstart
----------

1. Build the cardano-db-sync package with Nix:

   ```bash
   nix build .#cardano-db-sync-linux
   ```

2. Extract the binary from the tarball and copy it to this directory:

   ```bash
   tar xf result/cardano-db-sync-*-linux.tar.gz -C docker/ --strip-components=1 ./bin/cardano-db-sync
   ```

3. Build the image:

   ```bash
   docker build -t cardano-db-sync:lightweight docker/
   ```

4. Run the container and mount your configuration (recommended):

   ```bash
   docker run -v $PWD/environments/mainnet:/config \
     --env POSTGRES_HOST=postgres --env POSTGRES_PORT=5432 \
     cardano-db-sync:lightweight run --config /config/db-sync-config.json
   ```

Notes
-----

- The `Dockerfile` uses `FROM scratch` and therefore requires the binary to be
  statically linked. If you need dynamic linking, change the base image to a small
  distro such as `ubuntu:24.04` or `debian:bookworm-slim`.
- The image intentionally omits network configuration files. Use volumes or a
  derived image if you want to embed configs.

See also: the project's `doc/docker.md` for more details and guidance.

