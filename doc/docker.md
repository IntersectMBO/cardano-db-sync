# Docker

## Quickstart

We provide an example
[docker-compose.yml](https://github.com/IntersectMBO/cardano-db-sync/blob/master/docker-compose.example.yml)
to quickly get set up with `cardano-db-sync`. Keep in mind that this is only a template,
and users are encouraged to tweak it to meet their needs.

It is not recommended to use this for a production set up, and should only be
used for local development.

Create a working directory:

```bash
mkdir ~/src
cd ~/src
```

Download the example Docker Compose file:

```bash
curl -o docker-compose.yml \
  https://raw.githubusercontent.com/IntersectMBO/cardano-db-sync/master/docker-compose.example.yml
```

Start `cardano-node`, `postgresql`, and `cardano-db-sync` services:

```bash
docker compose up -d && docker compose logs -f
```

The PostgreSQL database should now be exposed on localhost port `5432`

To connect to PostgreSQL database:

```bash
psql -h 0.0.0.0 -p 5432 -U postgres -d cexplorer
```

### Connecting to a Other Networks

To connect to different network use `NETWORK` environment variable:

```bash
NETWORK=preprod docker compose up -d && docker compose logs -f
```

## Running `cardano-db-sync`

Start `cardano-db-sync`:

```bash
docker run \
  --env NETWORK=mainnet \
  --env POSTGRES_HOST=postgres \
  --env POSTGRES_PORT=5432 \
  --volume db-sync-data:/var/lib/cexplorer \
  --volume node-ipc \
  ghcr.io/IntersectMBO/cardano-db-sync:13.3.0.0
```

### Environment Variables

#### `NETWORK`

Specifies a network to connect to. If specified, `cardano-db-sync` will provide the
configuration files. If not specified, will call the `cardano-db-sync` executable as the
entrypoint. Possible values are:

 * mainnet
 * preprod
 * preview
 * private
 * sanchonet
 * shelley_qa

#### `POSTGRES_HOST` (required)

The PostgreSQL server host to connect to.

#### `POSTGRES_PORT` (required)

Specifies the PostgreSQL server port to connect to.

#### `POSTGRES_USER` (required)

The PostgreSQL server user to connect as.

#### `POSTGRES_PASSWORD` (required)

Specifies the PostgreSQL server password to connect as.

#### `RESTORE_SNAPSHOT` (optional)

Specifies a `cardano-db-sync` snapshot archive to download and restore from. If omitted,
it will sync from genesis. see [Restoring From Snapshots](#restoring-from-snapshots)

### `DB_SYNC_CONFIG` (optional)

Overrides the `db-sync-config.json` provided by the network configuration. See [Overriding
Network Configuration](#overriding-network-configuration).

#### `EXTRA_DB_SYNC_ARGS` (optional)

Extra command line arguments to pass to `cardano-db-sync`. For example:

```bash
docker run \
  --env NETWORK=mainnet \
  --env POSTGRES_HOST=postgres \
  --env POSTGRES_PORT=5432 \
  --env EXTRA_DB_SYNC_ARGS="--skip-fix"
  --volume db-sync-data:/var/lib/cexplorer \
  --volume node-ipc \
  ghcr.io/IntersectMBO/cardano-db-sync:13.3.0.0
```

### Overriding Network Configuration

Overriding the configuration file can be done by passing the `DB_SYNC_CONFIG` environment
variable:

```bash
docker run \
  --env NETWORK=mainnet \
  --env POSTGRES_HOST=postgres \
  --env POSTGRES_PORT=5432 \
  --env DB_SYNC_CONFIG=/config/db-sync-config.json \
  --volume db-sync-data:/var/lib/cexplorer \
  --volume node-ipc:/node-ipc \
  --volume $PWD/environments/mainnet:/config \
  ghcr.io/IntersectMBO/cardano-db-sync:13.3.0.0
```

### Restoring From Snapshots

Snapshots are provided for mainnet to restore the PostgreSQL database. See the [latest
releases](https://github.com/IntersectMBO/cardano-db-sync/releases) for a recent snapshot
matched with the `cardano-db-sync` version.

Although, you can specify a URL directly, we recommend downloading the image separately to avoid
having to download it multiple times in case of failure. First, download the latest snapshot:

```bash
curl -O https://update-cardano-mainnet.iohk.io/cardano-db-sync/13.3/db-sync-snapshot-schema-13.3-block-10611621-x86_64.tgz
```

Restore the snapshot with `RESTORE_SNAPSHOT`:

```bash
docker run \
  --env NETWORK=mainnet \
  --env POSTGRES_HOST=postgres \
  --env POSTGRES_PORT=5432 \
  --env RESTORE_SNAPSHOT=/data/db-sync-snapshot-schema-13.3-block-10611621-x86_64.tgz \
  --volume db-sync-data:/var/lib/cexplorer \
  --volume node-ipc:/node-ipc \
  --volume $PWD:/data
  ghcr.io/IntersectMBO/cardano-db-sync:13.3.0.0
```

### Advanced Usage

Excluding the `NETWORK` environment variable will simply call the `cardano-db-sync`
executable as the entrypoint, so you must pass command line arguments to projvide runtime
configuration. The `--schema-dir` argument is automatically set and is not required.

```bash
docker run \
  --volume db-sync-data:/var/lib/cexplorer \
  --volume node-ipc:/node-ipc \
  --volume $PWD/environments/mainnet:/config \
  ghcr.io/IntersectMBO/cardano-db-sync:13.3.0.0 \
  run --config /config/db-sync-config.json --socket-path /node-ipc/node.socket
```

## Running `cardano-smash-server`

Start `cardano-smash-server`:

```bash
docker run \
  --env NETWORK=mainnet \
  --env POSTGRES_HOST=postgres \
  --env POSTGRES_PORT=5432 \
  --env SMASH_USER=smash-admin \
  --env SMASH_PASSWORD=smash-password \
  --publish 3100:3100 \
  --volume node-ipc:/node-ipc \
  ghcr.io/IntersectMBO/cardano-smash-server:13.3.0.0
```

### Environment Variables

#### `NETWORK` (optional)

Specifies a network to connect to. If specified, `cardano-db-sync` will provide the
configuration files. If not specified, will call the `cardano-db-sync` executable as the
entrypoint. Possible values are:

 * mainnet
 * preprod
 * preview
 * private
 * sanchonet
 * shelley_qa

#### `POSTGRES_HOST` (required)

The PostgreSQL server host to connect to.

#### `POSTGRES_PORT` (required)

Specifies the PostgreSQL server port to connect to.

#### `POSTGRES_USER` (required)

The PostgreSQL server user to connect as.

#### `POSTGRES_PASSWORD` (required)

Specifies the PostgreSQL server password to connect as.

#### `SMASH_USER` (optional)

Sets the user for HTTP basic authentication

#### `SMASH_PASSWORD` (optional)

Sets the password for HTTP basic authentication

### Advanced Usage

Excluding the `NETWORK` environment variable will simply call the `cardano-smash-server`
executable as the entrypoint, so you must pass command line arguments to provide runtime
configuration.

```bash
docker run \
  --volume node-ipc:/node-ipc \
  --volume $PWD/environments/mainnet:/config \
  ghcr.io/IntersectMBO/cardano-db-sync:13.3.0.0 \
  --config /config/db-sync-config.json --port 3100
```

## Building

Building Docker images is done with [Nix](https://nixos.org/) and requires Linux:

```bash
nix build .#cardano-db-sync-docker
```

This will generate a `tar.gz` file linked to `./result` that can be loaded into docker and
run as a normal image.

```bash
docker load < result
docker run cardano-db-sync
```

## Running Tests with Docker PostgreSQL

Create a `pgpass-test` file with the credentials of (taken from config/secrets/postgres_* files):

```bash
echo "localhost:5432:cexplorer:postgres:v8hlDV0yMAHHlIurYupj" > config/pgpass-test
chmod 0600 config/pgpass-test
export PGPASSFILE=$PWD/config/pgpass-test
```

Start PostgreSQL via Docker Compose:

```bash
docker compose -f docker-test.yml up
```

Run the migrations:

```bash
cabal run cardano-db-tool run-migrations -- --mdir schema/ --ldir .
```

Run the tests:

```bash
cabal test cardano-db
```

When you've finished with testing, stop and remove the test containers:

```bash
docker compose -f docker-test.yml down
```
