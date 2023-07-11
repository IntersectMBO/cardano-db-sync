# Running

## Prerequisites

This guide assumes you have the following tools:

 * [Git](https://git-scm.com/download)
 * [Cardano Node](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md)
 * [Cardano DB Sync](installing.md)

## Download Configuration Files

Create a directory to store configs

```bash
mkdir -p ~/src/cardano-environments/{mainnet,preprod,preview,sanchonet}
cd ~/src/cardano-environments
```

Download the configuration files for the desired environment here:
https://book.world.dev.cardano.org/environments.html

For example, the following commands will download mainnet configs:
```bash
curl --remote-name-all --output-dir mainnet \
    https://book.world.dev.cardano.org/environments/mainnet/config.json \
    https://book.world.dev.cardano.org/environments/mainnet/db-sync-config.json \
    https://book.world.dev.cardano.org/environments/mainnet/submit-api-config.json \
    https://book.world.dev.cardano.org/environments/mainnet/topology.json
    https://book.world.dev.cardano.org/environments/mainnet/byron-genesis.json \
    https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json \
    https://book.world.dev.cardano.org/environments/mainnet/alonzo-genesis.json \
    https://book.world.dev.cardano.org/environments/mainnet/conway-genesis.json
```

## Start Cardano Node
```bash
cardano-node run \
    --config ~/src/cardano-environments/config.json
    --topology ~/src/cardano-environments/topology.json \
    --database-path ~/src/cardano-environments/mainnet/db \
    --socket-path node.socket \
    --host-addr 0.0.0.0 
```

## Start the DB Sync Node

Open another terminal and enter the `cardano-db-sync` repository directory:

```bash
cd ~/src/cardano-db-sync
```

Finally, we can start `cardano-db-sync`:

```
PGPASSFILE=config/pgpass-mainnet cardano-db-sync -- \
    --config ~/src/cardano-environments/mainnet/config.json \
    --socket-path ~/src/cardano-environments/mainnet/node.socket \
    --state-dir ~/src/cardano-environments/mainnet/ledger-state \
    --schema-dir schema/
```
