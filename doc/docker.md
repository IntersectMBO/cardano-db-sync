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

### To connect to another network:
```
NETWORK=testnet docker-compose up && docker-compose logs -f
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
nix-build -A dockerImage --no-out-link \
--builders 'ssh://builder@x86_64-linux.example.com x86_64-linux' \
--argstr system x86_64-linux
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
