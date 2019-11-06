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

### Set up and run the byron proxy
```
git clone https://github.com/input-output-hk/cardano-byron-proxy
cd cardano-byron-proxy
nix-build -A scripts.mainnet.proxy -o mainnet-byron-proxy
./mainnet-byron-prox
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
    --log-config ../cardano-node/configuration/log-configuration.yaml \
    --genesis-file ../cardano-node/mainnet-genesis.json \
    --genesis-hash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb \
    --socket-path ../cardano-node/state-node-mainnet/socket/node-core-0.socket \
    --schema-dir schema/
```

### Set up and run the explorer webapi
In the same `cardano-explorer` directory but a new terminal:
```
nix-build -A cardano-explorer -o explorer-webapi
PGPASSFILE=config/pgpass ./explorer-webapi/bin/cardano-explorer
```
