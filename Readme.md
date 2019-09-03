# Cardano Explorer

The new cardano-explorer consists of three components:

* `cardano-explorer-db` which defines common data types and functions that are shared by the
  following two components. In particular, it defines the database schema.
* `cardano-explorer-node` which acts as a Cardano node, following the chain and inserting
  date from the chain into a PostgreSQL database.
* `cardano-explorer` which serves data from the PostgreSQL database via HTTP.


## Architecture

The explorer is written in a highly modular fashion to allow it to be as flexible as possible.

The `cardano-explorer-node` connects to a locally running `cardano-node` (ie one connected to other
nodes in the Cardano network over the internet with TCP/IP) using a Unix domain socket, retrieves
blocks and stores parts of each block in a local PostgreSQL database. The database does not store
things like cryptographic signatures but does store enough information to follow the chain of
blocks and look at the transactions within blocks.

The PostgreSQL database is designed to be accessed in a read-only fashion from other applications.
The database schema is highly normalised which helps prevent data inconsistencies (specifically
the use of foreign keys from one table to another). More user friendly database queries can be
implemented using [Postgres Views][PostgresView] to implement joins between tables.

The `cardano-explorer` is a client than serves data from the PostgreSQL database as JSON via a
HTTP REST API.


## Schema Management

Schema management for the Cardano Explorer database is a little more complicated than we would like,
but the scheme chosen allows for easy development, evolution and management of the database.

The database schema is defined in three stages, each stage consisting of one or more SQL migrations.
The stages are:

1. Hand written SQL to set up custom SQL data types (using `DOMAIN` statements) and schema
   versioning.
2. SQL generated using the schema defined as Haskell data types (using the [Persistent][Persistent]
   library) to create the database tables.
3. Hand written SQL to create views into the tables defined in stage 2.

All the the schema migrations in these three stages are written to be idempotent (so that they
"know" if they have already been applied).

The migration files all have file names of the form:
```
migration-1-0000-20190730.sql
```
where the `1` denotes "stage 1" of the SQL migration, the `0000` is the migration version and the
last number is the date. Listing the directory containing the schema and sorting the list will
order them in the correct order for applying to the database.



[Persistent]: https://hackage.haskell.org/package/persistent
[PostgresView]: https://www.postgresql.org/docs/current/sql-createview.html
