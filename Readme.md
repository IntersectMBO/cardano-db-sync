[![Haskell CI](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-db-sync/haskell.yml?branch=master&style=for-the-badge)](https://github.com/intersectmbo/cardano-db-sync/actions/workflows/haskell.yml)
[![Discord](https://img.shields.io/discord/1136727663583698984?style=for-the-badge&color=blue)](https://discord.gg/GXrTvmzHQN)

# Cardano DB Sync

**Note:** Anyone wishing to build and run anything in this repository should avoid the `master` branch and build/run from the latest release tag.

## Purpose

The purpose of Cardano DB Sync is to follow the Cardano chain and take information from the chain
and an internally maintained copy of ledger state. Data is then extracted from the chain and
inserted into a PostgreSQL database. SQL queries can then be written directly against the database
schema or as queries embedded in any language with libraries for interacting with an SQL database.

Examples of what someone would be able to do via an SQL query against a Cardano DB Sync
instance fully synced to a specific network is:

* Look up any block, transaction, address, stake pool etc on that network, usually by the hash that
  identifies that item or the index into another table.
* Look up the balance of any stake address for any Shelley or later epoch.
* Look up the amount of ADA delegated to each pool for any Shelley or later epoch.

## Documentation

The [doc/](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/Readme.md) directory,  offers essential resources for users, including guides for installation with and without Nix, configuration. Key files cover, Docker usage, PostgreSQL example queries, command-line options and database migrations. There's also a troubleshooting guide for common issues. These documents provide users with the necessary steps to effectively set up and maintain the Cardano DB Sync.

## Architecture

The cardano-db-sync component consists of a set of components:

* `cardano-db` which defines common data types and functions used by any application that needs
  to interact with the data base from Haskell. In particular, it defines the database schema.
* `cardano-db-tool` a tool used to manage the databases of cardano-db-sync (create
  and run migrations, validate and analyse)
* `cardano-db-sync` which acts as a Cardano node, following the chain and inserting
  data from the chain into a PostgreSQL database.

The db-sync node is written in a highly modular fashion to allow it to be as flexible as possible.

The `cardano-db-sync` node connects to a locally running `cardano-node` (ie one connected to other
nodes in the Cardano network over the internet with TCP/IP) using a Unix domain socket, retrieves
blocks, updates its internal ledger state and stores parts of each block in a local PostgreSQL
database. The database does not store things like cryptographic signatures but does store enough
information to follow the chain of blocks and look at the transactions within blocks.

The PostgreSQL database is designed to be accessed in a read-only fashion from other applications.
The database schema is highly normalised which helps prevent data inconsistencies (specifically
with the use of foreign keys from one table to another). More user friendly database queries can be
implemented using [Postgres Views][PostgresView] to implement joins between tables.

## System Requirements
### Last update: June 2026

The system requirements for `cardano-db-sync` (with both `db-sync` and the `node` running
on the same machine are:

* Any of the big well known Linux distributions (eg, Debian, Ubuntu, RHEL, CentOS, Arch
  etc).
* 64 Gigabytes of RAM or more (for `mainnet`).
* 4 CPU cores or more.
* Ensure that the machine has sufficient IOPS (Input/Output Operations per Second). Ie it should be
  60k IOPS or better. Lower IOPS ratings will result in slower sync times and/or falling behind the
  chain tip.
* 1 Terabyte or more of disk storage (preferably SSD which are 2-5 times faster than
  electro-mechanical disks).


Stats for configuration:

```json
  "insert_options": {
    "tx_cbor": "disable",
    "tx_out": {
      "value": "enable",
      "force_tx_in": false,
      "use_address_table": false
    },
    "ledger": "enable",
    "shelley": {
      "enable": true
    },
    "multi_asset": {
      "enable": true
    },
    "metadata": {
      "enable": true
    },
    "plutus": {
      "enable": true
    },
    "pool_stat": "enable",
    "governance": "enable",
    "offchain_pool_data": "enable",
    "offchain_vote_data": "enable",
    "json_type": "text"
  }
```

### Test environment

All stats below were measured on this host:

- **CPU:** AMD Ryzen 9 3900X (12 cores / 24 threads)
- **RAM:** 64 GB
- **Storage:** `cardano-node` DB + `db-sync` ledger state on NVMe SSD (ADATA SX8200PNP, 1 TB, ext4); `Postgres` data dir on SATA SSD (WD Red SA500, 4 TB, ext4)
- **OS:** Linux Mint 22.3, kernel 6.8
- **PostgreSQL 18.4:** `shared_buffers` 10 GB | `effective_cache_size` 24 GB | `maintenance_work_mem` 2 GB | `work_mem` 15 MB | `max_wal_size` 4 GB | `min_wal_size` 1 GB | `wal_level` replica | `synchronous_commit` on | `max_connections` 100

### `mainnet`

#### Storage
- `cardano-node` database size: 235,3 GB
- `cardano-db-sync` total ledger state files size: ~ 10,5 GB
- `Postgres` database `mainnet-dbsync-13.7.1.0-node-11.0.1` total size: 542,6 GB

#### RAM (RSS)
- `cardano-node` Peak RAM: 16,6 GB
- `cardano-db-sync` Peak RAM: 21,6 GB

##### LSM
- `cardano-node` Peak RAM: 5,86 GB
- `cardano-node` max node db dir size: ~ 236,6 GB | `lsm` subdirectory max size: 9,89 GB
- `cardano-db-sync` Peak RAM: 7,54 GB
- `cardano-db-sync` max ledger state dir size: ~ 16,8 GB | `lsm` subdirectory max size: 14,8 GB


### `preprod`

#### Storage
- `cardano-node` database size: 18,5 GB
- `cardano-db-sync` total ledger state files size: ~ 2,8 GB
- `Postgres` database `preprod-dbsync-13.7.1.0-node-11.0.1` total size: 28,1 GB

#### RAM (RSS)
- `cardano-node` Peak RAM: 7,97 GB
- `cardano-db-sync` Peak RAM: 5,0 GB

##### LSM
- `cardano-node` Peak RAM: 5,86 GB
- `cardano-db-sync` Peak RAM: 1,4 GB


### `preview`

#### Storage
- `cardano-node` database size: 15,2 GB
- `cardano-db-sync` total ledger state files size: ~ 2,4 GB
- `Postgres` database `preview-dbsync-13.7.1.0-node-11.0.1` total size: 29,9 GB

#### RAM (RSS)
- `cardano-node` Peak RAM: 4,1 GB
- `cardano-db-sync` Peak RAM: 7,9 GB

##### LSM
- `cardano-node` Peak RAM: 1,48 GB
- `cardano-db-sync` Peak RAM: 2,2 GB


The recommended configuration is to have the `db-sync` and the PostgreSQL server on the same
machine. During syncing (getting historical data from the blockchain) there is a **HUGE** amount
of data traffic between `db-sync` and the database. Traffic to a local database is significantly
faster than traffic to a database on the LAN or remotely to another location.

When building an application that will be querying the database, remember that for fast queries,
low latency disk access is far more important than high throughput (assuming the minimal IOPS
above is met).

## How to Contact the Cardano DB Sync Team

You can discuss development or find help at the following places:

 * Intersect Discord [#db-sync](https://discord.com/channels/1136727663583698984/1239888910537064468) channel, if new to server invite [here](https://discord.gg/GXrTvmzHQN)
 * [GitHub Issues](https://github.com/IntersectMBO/cardano-db-sync/issues)

## Installation

Install db-sync with one of the following methods:

 * [Static Binaries](https://github.com/IntersectMBO/cardano-db-sync/releases/latest)
 * [Installing with Nix][InstallingNix]
 * [Installing from Source][Installing]
 * [Docker][Docker]

Once installed, start db-sync by following the [Running Guide][Running].

## Troubleshooting

If you have any issues with this project, consult the [Troubleshooting][Troubleshooting] page for
possible solutions.

## Further Reading

* [BuildingRunning][BuildingRunning]: Building and running the db-sync node.
* [Docker][Docker]: Instruction for docker-compose, and building the images using nix.
* [ERD][ERD]: The entity relationship diagram.
* [Example SQL queries][ExampleQueries]: Some example SQL and Haskell/Esqueleto queries.
* [OffChainPoolData][OffChainPoolData]: Explanation of how stake pool offchain data is retried.
* [Schema Documentation][Schema Documentation]: The database schema documentation.
* [Schema Management][Schema Management]: How the database schema is managed and modified.
* [StateSnapshot][StateSnapshot]: Document the creation and restoration of state snapshot files.
* [Upgrading PostgreSQL][UpgradingPostgres]
* [Validation][Validation]: Explanation of validation done by the db-sync node and assumptions made.

[InstallingNix]: doc/installing-with-nix.md
[Installing]: doc/installing.md
[BuildingRunning]: doc/building-running.md
[Docker]: doc/docker.md
[Running]: doc/running.md
[ERD]: doc/ERD.png
[ExampleQueries]: doc/interesting-queries.md
[PostgresView]: https://www.postgresql.org/docs/current/sql-createview.html
[OffChainPoolData]: doc/pool-offchain-data.md
[Schema Documentation]: doc/schema.md
[Schema Management]: doc/schema-management.md
[StateSnapshot]: doc/state-snapshot.md
[Troubleshooting]: doc/troubleshooting.md
[UpgradingPostgres]: doc/upgrading-postgresql.md
[Validation]: doc/validation.md
