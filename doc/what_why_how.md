# cardano-db-sync

### What

Cardano DB Sync is program that follows the Cardano chain and takes information from the chain
and an internally maintained copy of ledger state an inserts that data into a PostgreSQL database.
SQL (structured query language) queries can then be written directly against the database schema
or as queries embedded in any language with libraries for interacting with an SQL database.

DB Sync provides historical data right back to the original Genesis block. It does not provide
event sourcing capabilities.

### Why

The database provided and maintained by `cardano-db-sync` allows anybody with some SQL expertise
to extract information about the Cardano chain (including some data that is not on the chain but
rather is part of ledger state). This information includes things like

* The transactions in a specific block (from the chain).
* The balance of a specific address (from the chain).
* The staking rewards earned by a pool or address for a specific epoch (from ledger state).
* ...

### How

Cardano DB Sync is written in Haskell. When it is run, it connects to a local `cardano-node`
instance via a UNIX domain socket. This connection to the `node` allows `db-sync` to extract both
old, historical chain data or to simply follow the current chain tip. Cardano DB Sync, while
following the chain, can also maintain its own copy of ledger state which includes data that is
not on the chain, like stake delegation info, rewards info etc.

