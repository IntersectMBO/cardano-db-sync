**Validated: 2020/02/19**

# Building and Running the Explorer

The explorer is built and tested to run on Linux. It may run on Mac OS X or Windows but that is
unsupported.

Running the explorer will require Nix and and either multiple terminals or a multi terminal
emulator like GNU Screen or TMux.

The Explorer is designed to work with a locally running Cardano Node. Currently the node also
needs a locally running Byron proxy. The three git repositories need to be checked out so that
they are all at the same level. eg:

```
> tree -L 1
.
├── cardano-byron-proxy
├── cardano-explorer
├── cardano-node
```
To setup and run explorer for testnet replace **mainnet** with **testnet** in all examples below.

### Set up and run the byron proxy
```
git clone https://github.com/input-output-hk/cardano-byron-proxy
cd cardano-byron-proxy
nix-build -A scripts.mainnet.proxy -o mainnet-byron-proxy
./mainnet-byron-proxy
```

### Set up and run a local node that connects to the byron proxy
```
git clone https://github.com/input-output-hk/cardano-node
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local --arg customConfig '{ useProxy = true; }'
./mainnet-node-local
```

### Set up and run the explorer node
```
git clone https://github.com/input-output-hk/cardano-explorer
cd cardano-explorer
nix-build -A cardano-explorer-node -o explorer-node
scripts/postgresql-setup.sh --createdb
PGPASSFILE=config/pgpass explorer-node/bin/cardano-explorer-node \
    --config config/explorer-mainnet-config.yaml \
    --genesis-file ../cardano-node/configuration/mainnet-genesis.json \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --schema-dir schema/
```

### Set up and run the explorer webapi
In the same `cardano-explorer` directory but a new terminal:
```
nix-build -A cardano-explorer-webapi -o explorer-webapi
PGPASSFILE=config/pgpass ./explorer-webapi/bin/cardano-explorer-webapi
```

### Set up and run the transaction submission webapi

Make sure you have:
- `cabal` 3.0 or higher (`cabal --version` to see currently installed version)
- `ghc` 8.6.5 or higher (`ghc --version`)

You may also need the following native libraries
```
sudo apt install libsystemd-dev
sudo apt-get install libz-dev
sudo apt-get install libpq-dev
sudo apt install libssl-dev
```

In the same `cardano-explorer` directory but in a new terminal:
```
cabal run cardano-tx-submit-webapi -- \
    --config config/tx-submit-mainnet-config.yaml \
    --genesis-file ../cardano-node/configuration/mainnet-genesis.json \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --port 8101
```
