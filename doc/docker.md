# Docker

## Quickstart

### Clone the repository

```
git clone git@github.com:input-output-hk/cardano-db-sync.git
cd cardano-db-sync
```
### Start `cardano-node`, `postgresql`, and `cardano-db-sync` services using Docker

``` console
docker-compose up -d && docker-compose logs -f
```
### :tada

The PostgreSQL database is exposed on localhost port `5432`

### To connect to another network:
```
NETWORK=testnet docker-compose up && docker-compose logs -f
```

### Take control
Excluding the `NETWORK` ENV will simply just call the `cardano-db-sync` or
`cardano-db-sync-extended` executable as the entrypoint, so you must pass a command and
arguments in this case to provide config at runtime. The `--schema-dir` argument is preset,
so is not required.

Using Docker run to demonstrate, but can be achieved using `docker-compose` too via
`service.command`
```
docker run \
  -v $PWD/config/network/mainnet/cardano-db-sync:/config
  -v $PWD/config/network/mainnet/genesis:/genesis
  -v $PWD/node-ipc:/node-ipc \
  inputoutput/cardano-db-sync \
    run --config /config/config.yaml --socket-path /node-ipc/node.socket # command
```

## Build and load image using Nix

```
docker load -i $(nix-build -A dockerImage --no-out-link)
```

## Restore from Snapshot

Restoring a database by running from gensis can take a number of hours, snapshots are provided for
both networks (Mainnet and Testnet) to restore the postgres database. See the
[latest releases](https://github.com/input-output-hk/cardano-db-sync/releases) for a recent snapshot
matched with the `cardano-db-sync` version.

To download and restore a snapshot include `RESTORE_SNAPSHOT`:

```
RESTORE_SNAPSHOT=https://update-cardano-mainnet.iohk.io/cardano-db-sync/db-sync-snapshot-schema-10-block-6014140-x86_64.tgz \
NETWORK=testnet docker-compose up && docker-compose logs -f

```
