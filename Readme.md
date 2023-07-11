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

Example SQL queries are available at [Example Queries][ExampleQueries]. You can also find some [DB Sync best practices here](https://docs.cardano.org/cardano-components/cardano-db-sync/best-practices).

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

The system requirements for `cardano-db-sync` (with both `db-sync` and the `node` running
on the same machine are:

* Any of the big well known Linux distributions (eg, Debian, Ubuntu, RHEL, CentOS, Arch
  etc).
* 32 Gigabytes of RAM or more.
* 4 CPU cores or more.
* Ensure that the machine has sufficient IOPS (Input/Output Operations per Second). Ie it should be
  60k IOPS or better. Lower IOPS ratings will result in slower sync times and/or falling behind the
  chain tip.
* 320 Gigabytes or more of disk storage (preferably SSD which are 2-5 times faster than
  electro-mechanical disks).

The recommended configuration is to have the `db-sync` and the PostgreSQL server on the same
machine. During syncing (getting historical data from the blockchain) there is a **HUGE** amount
of data traffic between `db-sync` and the database. Traffic to a local database is significantly
faster than traffic to a database on the LAN or remotely to another location.

When building an application that will be querying the database, remember that for fast queries,
low latency disk access is far more important than high throughput (assuming the minimal IOPS
above is met).

## Installation

Install db-sync with one of the following methods:

 * [Static Binaries](https://github.com/input-output-hk/cardano-db-sync/releases/latest)
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
* [PoolOfflineData][PoolOfflineData]: Explanation of how stake pool offline data is retried.
* [Schema Documentation][Schema Documentation]: The database schema documentation.
* [Schema Management][Schema Management]: How the database schema is managed and modified.
* [StateSnapshot][StateSnapshot]: Document the creation and restoration of state snapshot files.
* [Validation][Validation]: Explanation of validation done by the db-sync node and assumptions made.

[InstallingNix]: doc/installing-with-nix.md
[Installing]: doc/installing.md
[BuildingRunning]: doc/building-running.md
[Docker]: doc/docker.md
[Running]: doc/running.md
[ERD]: doc/ERD.png
[ExampleQueries]: doc/interesting-queries.md
[PostgresView]: https://www.postgresql.org/docs/current/sql-createview.html
[PoolOfflineData]: doc/pool-offline-data.md
[Schema Documentation]: doc/schema.md
[Schema Management]: doc/schema-management.md
[StateSnapshot]: doc/state-snapshot.md
[Troubleshooting]: doc/troubleshooting.md
[Validation]: doc/validation.md
