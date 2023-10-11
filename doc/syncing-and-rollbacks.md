# Syncing and Rollbacks

`cardano-db-sync` gets it blocks from a `cardano-node` running on the same machine via a local
domain socket and the `LocalChainSync` protocol provided by the `cardano-node` itself but
implemented in the `ouroboros-network` repository.


## Syncing Procedure

When `db-sync` starts up, it figures out its latest block number and block hash, sends it the
the `node` using the `LocalChainSync` protocol. The `node` then sends back either a rollback
message or a new block message where the new block correctly extends the existing chain that
`db-sync` has already collected.

Ignoring rollbacks for now, the `node` will continue sending `db-sync` blocks that extend
`db-sync`'s existing chain. For each new block, `db-sync` checks that the block's previous
block hash is already in the database, and then inserts the block, extracts the transactions
contained in the block and inserts them, and so on.


## Ledger State

Currently `db-sync` creates and maintains its own copy of ledger state, and stores a number
of recent ledger state files in the directory specified by the `--state-dir` command line
argument to `db-sync`. This ledger state directory must persist across machine reboots.
Each ledger state is valid only for a specific block. It is not valid for any block before
or any block after the block it is valid for.

The option `--state-dir` can be ommited when one doesn't want to use local ledger, for the omittion to work a `--disable-ledger` flag should be used, more information on what this flag does can be found [here](./configuration.md#--disable-ledger).


## Concurrency

`db-sync` currently runs in three threads and size bounded queues are used to communicate
between them. Attempts to add a new element to a full bounded queue, will result in the
insert blocking until an element is removed from the other end. The main queues of interest
are:

* The `DbAction` queue.
* The pool offchain metadata request queue.
* The pool offchain metadata response queue.

Due to limitations of PostgreSQL itself, all database operations are done in a single thread.
If database operations are attempted from more than one thread PostgreSQL returns "failure
to acquire lock" error messages.

The main threads are:

* The database insert thread. This thread retrieves data from the `DbAction` and pool
  offchain metadata response queue and inserts the data into the database.

* The `node` communication thread which retrieves blocks from the `node` and places them in
  the `DbAction` queue.
* The pool metadata thread, which reads the request queue for metadata to fetch, fetches it
  and posts a response in the response queue.


## Rollbacks

Rollbacks are a feature of the vast majority of blockchains. In addition to the normal
blockchain rollbacks, `db-sync` has a second potential source of rollbacks. On start up,
if `db-sync` cannot find a ledger state file at all, it will try to rollback to genesis.
This can happen for instance in a Docker container if the ledger state directory is not
persisted across reboots.

In `db-sync` version `8.0.0` and earlier rollbacks were performed in Haskell code. That
means that if a block was to be rolled back, the database would be queried for all transactions
in that block and all the sub-components of all the transactions. A PostgreSQL delete would
then be issued for each sub-component, each transaction and the block itself. Obviously
this was inefficient and slow.

For `db-sync` version `9.0.0`, a newer version of the `Persistent` library was available that
makes rollbacks much more efficient. In this version of `db-sync` and later tables that
reference/index other tables are linked, with foreign keys. This means that `db-sync` can issue a
delete operation on a single object and then PostgreSQL will recursively delete all the objects that
reference the object to be deleted as well as the object itself. This leads to significantly
improved rollback times. During development it is often useful to be able to rollback 10000
or more blocks, even though that is guaranteed to be an invalid rollback according to the ledger
rules. With this new version of `Persistent` a rollback of 10000 blocks could be done in
minutes whereas previously it was several hours.

For `db-sync` versions 13.1, we removed all foreign keys and switched back to perform rollbacks from
Haskell, but in a much more effecient way. Instead of, for example, finding all transactions from
all the blocks that needs deletion, we found only the oldest one and delete with a single query
every transaction after it. We use the property that fields like `tx.block_id` are non-decreasing,
meaning newer entries will have bigger values. So if we simply find the oldest transaction that
needs to be deleted, it's easy to delete everything with a single query.
