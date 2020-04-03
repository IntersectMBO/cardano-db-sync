**Validated: 2020/02/19**

# Building and Running the Cardano DB Sync Node

The cardano-db-sync node is built and tested to run on Linux. It may run on Mac OS X or Windows but
that is unsupported.

Running the db sync node will require Nix and either multiple terminals or a multi terminal
emulator like GNU Screen or TMux. Setup [IOHK binary cache](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md) to avoid several hours of build time.

The db sync node is designed to work with a locally running Cardano Node. The two git repositories need to be checked out so that
they are both at the same level. eg:

```
> tree -L 1
.
├── cardano-db-sync
├── cardano-node
```
To setup and run db sync node for testnet replace **mainnet** with **testnet** in all examples below.

### Set up and run a local node
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
./mainnet-node-local
```

### Set up and run the db-sync node
```
git clone https://github.com/input-output-hk/cardano-db-sync
cd cardano-db-sync
nix-build -A cardano-db-sync -o db-sync-node
scripts/postgresql-setup.sh --createdb
PGPASSFILE=config/pgpass db-sync-node/bin/cardano-db-sync \
    --config config/mainnet-config.yaml \
    --genesis-file ../cardano-node/configuration/mainnet-genesis.json \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --schema-dir schema/
```
