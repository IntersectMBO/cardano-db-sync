# State Snapshot

As the size of the blockchain itself and the number of transactions and other data on the chain
increases, the time required to sync the full chain increases. At epoch 266 it was about 18 hours.
The other issue is that most major upgrades also update the database schema meaning the database
needs to be synced from scatch.

To overcome these issues, we are providing a `cardano-db-sync` state snapshot, which should
drastically reduce the time required to get `db-sync` back up and running after the database is
dropped and recreated. This snapshot is compatible with both `cardano-db-sync` and
`cardano-db-sync` with --no-epoch-table (which doesn't maintain the extra `epoch` table).

**Note:** It is **not** possible to create a snapshot from one version of the database schema and
restore it so it can be used with a `db-sync` that uses another version of the schema.

All of the following assumes that the executable `cardano-db-tool` and the script
`postgresql-setup.sh` is available on the machine where the snapshot is being created or restored.

Currently (at epoch 269), creating a snapshot takes about 15 minutes and restoring one takes about
45 minutes.

## Things to note:
* Snapshots (because they depend on the database schema) are not portable across `db-sync` versions.
* Snapshots (because they include a snapshot of the ledger state) are not portable across CPU
  architectures (ie it is not possible to create a snapshot on `x86_64` and expect it to work
  correctly on say `arm64`).
* Creating and restoring snapshots requires significant amounts of free disk space (at epoch 269
  it required about 10G). If there is insufficient disk space, `gzip` can give some odd error
  messages.
* node tip should be ahead of the snapshot point during restoration otherwise `cardano-db-sync` will 
  roll back to genesis

# Creating a Snapshot

To create a snapshot, the `cardano-db-sync` executable should be stopped. Taking a snapshot is
then a two step process:

```
PGPASSFILE=config/pgpass-mainnet cardano-db-tool prepare-snapshot --state-dir ledger-state/mainnet/
```
which will then print out the command (combining the database schema version with the block number
in the database with the slot number used by the ledger state and the ) required to generated the snapshot:
```
PGPASSFILE=config/pgpass-mainnet scripts/postgresql-setup.sh --create-snapshot \
    db-sync-snapshot-schema-9-block-5796064-x86_64 ledger-state/mainnet/31021676-f3873e4bec.lstate
```

# Restoring from a Snapshot

Restoring the state from a snapsot will drop the current database, recreate the tables and then
populate them. It can be done as simply as:
```
PGPASSFILE=config/pgpass-mainnet scripts/postgresql-setup.sh --restore-snapshot \
	db-sync-snapshot-schema-9-block-5796064-x86_64 ledger-state/mainnet
```

Once the script has completed successfully, `db-sync` can be restarted and it should continue
syncing from the block number listed in the state snapshot file name.

# Mainnet Snapshots Location

`Mainnet` snapshots can be found [here](https://update-cardano-mainnet.iohk.io/cardano-db-sync/index.html#).
They are also linked from the `cardano-db-sync` [releases page](https://github.com/input-output-hk/cardano-db-sync/releases)