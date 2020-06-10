# Transaction bodies

The executable defined in cardano-db-sync/app/cardano-tx-body.hs is a stopgap 
measure for Byron support of hardware wallets.
When we're on Shelly this shouldn't be needed.

This is naive plugin (meant to be run side-by-side with cardano-db-sync I guess?)
that just creates a map of transaction hashes and bodies in a postgresql table.

To build:

```
nix-build -A cardano-tx-body -o tx-body-node
```

to run (assuming cardano-node is running):

```
PGPASSFILE=config/pgpass ./tx-body-node/bin/cardano-tx-body \
    --config config/mainnet-config.yaml \
    --genesis-file ../cardano-node/configuration/defaults/byron-mainnet/genesis.json \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --schema-dir schema/
```
