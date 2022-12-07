# Docker

## Quickstart

### Clone the repository

Regular users should almost never attempt building and running from the `master` branch. Instead,
they should build and run the latest release tag. The tags can be listed using the `git tag`
command. Pre-release tags (eg things like `12.0.0-preX`) should also be avoided in most cases.
```
git clone git@github.com:input-output-hk/cardano-db-sync.git
cd cardano-db-sync
git checkout <latest-official-tag> -b tag-<latest-official-tag>
```
### Start `cardano-node`, `postgresql`, and `cardano-db-sync` services using Docker

``` console
docker-compose up -d && docker-compose logs -f
```
### tada:tada:

The PostgreSQL database is exposed on localhost port `5432`

### To connect to PostgreSQL database:

`$ psql -h 0.0.0.0 -p 5432 -U postgres -d cexplorer` (then enter secret password)

### To change PostgreSQL settings:

Release `13.1.0.0` introduced new flag `POSTGRES_ARGS` inside 
[docker-compose.yml](https://github.com/input-output-hk/cardano-db-sync/blob/master/docker-compose.yml) file wih
reccomended default values for `maintenance_work_mem` and `max_parallel_maintenance_workers` parameters.

- default start
```sh
docker compose up && docker-compose logs -f

docker ps | grep postgres
CONTAINER ID   IMAGE                             COMMAND                  CREATED         STATUS                            PORTS                                       NAMES
fe5fa531761a   postgres:11.18-alpine             "docker-entrypoint.s…"   8 seconds ago   Up 7 seconds (health: starting)   0.0.0.0:5432->5432/tcp, :::5432->5432/tcp   cardano-db-sync-postgres-1

docker exec -it fe5fa531761a bash
fe5fa531761a:/# psql -U postgres -c "SHOW ALL" | grep maintenance
 maintenance_work_mem                   | 1GB                                        | Sets the maximum memory to be used for maintenance operations.
 max_parallel_maintenance_workers       | 4                                          | Sets the maximum number of parallel processes per maintenance operation.
```
- setting custom values through `POSTGRES_ARGS`:
```sh
export POSTGRES_ARGS="-c maintenance_work_mem=2GB -c max_parallel_maintenance_workers=8"
docker compose up && docker-compose logs -f

docker exec -it b73d677399de bash
b73d677399de:/# psql -U postgres -c "SHOW ALL" | grep maintenance
 maintenance_work_mem                   | 2GB                                        | Sets the maximum memory to be used for maintenance operations.
 max_parallel_maintenance_workers       | 8                                          | Sets the maximum number of parallel processes per maintenance operation.
```

`SHOW` displays the current setting of run-time parameters. Be aware that it can differ from the values in `postgresql.conf`.

### To connect to another network:

To connect to different network (preprod or preview) use `NETWORK` environment variable:


```
NETWORK=preprod docker-compose up && docker-compose logs -f
```

### Take control

Excluding the `NETWORK` ENV will simply just call the `cardano-db-sync` executable
as the entrypoint, so you must pass a command and
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
nix build .#dockerImage
docker load -i ./result
```

## Restore from Snapshot

Restoring a database by running from gensis can take a number of hours, snapshots are provided for
Mainnet to restore the postgres database. See the
[latest releases](https://github.com/input-output-hk/cardano-db-sync/releases) for a recent snapshot
matched with the `cardano-db-sync` version.

To download and restore a snapshot include `RESTORE_SNAPSHOT`:

```
RESTORE_SNAPSHOT=https://update-cardano-mainnet.iohk.io/cardano-db-sync/db-sync-snapshot-schema-10-block-6014140-x86_64.tgz \
docker-compose up && docker-compose logs -f
```

### Set folder where to download snapshot

The snapshot is downloaded in current working directory. Setting the working directory should allow to choose where the download is done.

For `docker-compose`:

```yaml
cardano-db-sync:
    image: inputoutput/cardano-db-sync:13.0.5
    ...
    working_dir: /var/lib/cexplorer
    volumes:
      - db-sync-data:/var/lib/cexplorer
      - node-ipc:/node-ipc
    restart: on-failure
    ...
```
After starting `docker-compose` the snapshot file should be downloaded to specified directory
which exact location can be found by using `docker volume inspect` command:

```sh
docker volume inspect cardano-db-sync_db-sync-data
[
    {
        "CreatedAt": "2022-11-08T12:44:55+01:00",
        "Driver": "local",
        "Labels": {
            "com.docker.compose.project": "cardano-db-sync",
            "com.docker.compose.version": "1.29.2",
            "com.docker.compose.volume": "db-sync-data"
        },
        "Mountpoint": "/var/snap/docker/common/var-lib-docker/volumes/cardano-db-sync_db-sync-data/_data",
        "Name": "cardano-db-sync_db-sync-data",
        "Options": null,
        "Scope": "local"
    }
]
```

for `docker` use `--workdir=`.

## Disable options

Consult the configuration [docs](docs/configuration.md) for what these options mean, assuming you have read that
they can be accessed via env variables passed to docker-compose. Leave out any that do not make sense, eg if
you just want to disable the ledger use `EXTRA_DB_SYNC_ARGS=--disable-ledger docker-compose up`.

``` shell
EXTRA_DB_SYNC_ARGS="--disable-ledger --disable-cache --disable-epoch" \
docker-compose up
```

## Running Tests with Docker Postgres

Create a `pgpass-test` file with the credentials of (taken from config/secrets/postgres_* files):

``` shell
echo "localhost:5432:cexplorer:postgres:v8hlDV0yMAHHlIurYupj" > config/pgpass-test
chmod 0600 config/pgpass-test
```

inside `config/pgpass-test`

Startup docker postgres via:

``` shell
docker-compose -f docker-test.yml up
```

Setup database with tables:

``` shell
PGPASSFILE=$PWD/config/pgpass-test \
cabal run cardano-db-tool run-migrations -- --mdir schema/ --ldir .
```

Running the tests:

``` shell
PGPASSFILE=$PWD/config/pgpass-test \
cabal test cardano-db
```

When you've finished with testing either docker-compose down or Ctl-C the terminal session.

``` shell
docker-compose down -f docker-test.yml
```

## Building Docker images with Nix

Assuming a base OSX machine with Nix installed. Building a Docker image for cardano-db-sync requires a
Linux host to compile on. Nix provides a way to do [remote builds](https://nixos.org/manual/nix/unstable/advanced-topics/distributed-builds.html)

Prerequisites:
 * shell account on NixOS Linux machine (ask on Slack)
   eg. builder@x86_64-linux.example.com

Assuming you want a Linux x86 image run:

``` shell
nix build .#legacyPackages.x86_64-linux.dockerImage \
--builders 'ssh://builder@x86_64-linux.example.com x86_64-linux'
```

At the end it will generate a `tar.gz` file
eg `/nix/store/arbrn0fs54whkn64m0wrcbl9hjd35byn-docker-image-cardano-db-sync.tar.gz`

that can be loaded into docker and run as a normal image.

``` shell
$ docker load -i /nix/store/arbrn0fs54whkn64m0wrcbl9hjd35byn-docker-image-cardano-db-sync.tar.gz

$ docker image ls
REPOSITORY                    TAG                                        IMAGE ID       CREATED        SIZE
inputoutput/cardano-db-sync   066b747a8bfd3791b06ea46c2e793f83ed64967f   f34b029e9c5c   15 hours ago   911MB

# Run this as
$ docker run inputoutput/cardano-db-sync:066b747a8bfd3791b06ea46c2e793f83ed64967f
```

## Running SMASH with docker-compose

Edit the docker-compose.yml to add a listening port for the postgres container
e.g.

``` yaml
services:
  postgres:
    ...
    restart: on-failure
    ports:
      - "5432:5432"
    logging:
    ...
```

Follow the instructions from `Restore from Snapshot` and wait until the snapshot has restored.
Create a `pgpass-local` file with the credentials of (taken from config/secrets/postgres_* files):

``` shell
echo "localhost:5432:cexplorer:postgres:v8hlDV0yMAHHlIurYupj" > config/pgpass-local
chmod 0600 config/pgpass-local
```

Run SMASH-server

``` shell
PGPASSFILE=config/pgpass-test cabal exec -- cardano-smash-server \
     --config config/mainnet-config.yaml \
     --port 3100
```

See [smash documentation](doc/smash.sh) for querying the SMASH-server
