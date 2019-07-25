# Cardano Explorer

The new cardano-explorer consists of three components:

* `cardano-explorer-core` which defines common data types and functions that are shared by the
  following two components. In particular, it defines the database schema.
* `cardano-explorer-db-node` which acts as a Cardano node, following the chain and inserting
  date from the chain into a PostgreSQL database.
* `cardano-explorer` which serves data from the PostgreSQL database via HTTP.

