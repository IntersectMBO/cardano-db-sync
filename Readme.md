# Cardano DB Sync

The cardano-db-sync component consists of a set of components:

* `cardano-db` which defines common data types and functions used by any application that needs
  to interact with the data base from Haskell. In particular, it defines the database schema.
* `cardano-db-sync` which acts as a Cardano node, following the chain and inserting
  data from the chain into a PostgreSQL database.
* `cardano-db-sync-extended` is a relatively simple extension to `cardano-db-sync` which maintains
  an extra table containing epoch data.


## Architecture

The db-sync node is written in a highly modular fashion to allow it to be as flexible as possible.

The `cardano-db-sync` node connects to a locally running `cardano-node` (ie one connected to other
nodes in the Cardano network over the internet with TCP/IP) using a Unix domain socket, retrieves
blocks and stores parts of each block in a local PostgreSQL database. The database does not store
things like cryptographic signatures but does store enough information to follow the chain of
blocks and look at the transactions within blocks.

The PostgreSQL database is designed to be accessed in a read-only fashion from other applications.
The database schema is highly normalised which helps prevent data inconsistencies (specifically
with the use of foreign keys from one table to another). More user friendly database queries can be
implemented using [Postgres Views][PostgresView] to implement joins between tables.

## Further Reading

* [BuildingRunning][BuildingRunning]: Building and running the db-sync node.
* [SchemaManagement][Schema Management]: How the database schema is managed and modified.
* [Validation][Validation]: Explanation of validation done by the db-sync node and assumptions made.

[BuildingRunning]: doc/building-running.md
[PostgresView]: https://www.postgresql.org/docs/current/sql-createview.html
[Schema Management]: doc/schema-management.md
[Validation]: doc/validation.md
