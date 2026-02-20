Lightweight Docker image for cardano-db-sync
==========================================

This folder contains a minimal Docker context and `Dockerfile` intended for use with
a statically-built `cardano-db-sync` executable produced by Nix.

## testing

docker run --rm cardano-db-sync:lightweight version
