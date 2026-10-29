# State Snapshot

State snapshots bundle the PostgreSQL database and the ledger state, allowing
`db-sync` to resume syncing without starting from genesis.

## Prerequisites

- `cardano-db-tool` and `scripts/postgresql-setup.sh` available
- Sufficient disk space for the dump + ledger state
- Node tip should be ahead of the snapshot point, otherwise db-sync may need to roll back

## Constraints

- Not portable across schema versions or possibly CPU architectures
- The ledger snapshot format matches `cardano-node` (consensus format)

## Backends

- **InMemory** -- UTxO in memory, serialized to `<slot>/tables`
- **LSM** -- UTxO on disk via LSM trees, stored in `<state-dir>/lsm/`

Both backends produce snapshots in consensus directory format: `<slot>/state`, `meta`, `utxoSize`.

## Creating

Stop `db-sync`, then:

```
PGPASSFILE=config/pgpass-mainnet cardano-db-tool prepare-snapshot --state-dir <state-dir>
```

This prints the create command, e.g.:

```
PGPASSFILE=config/pgpass-mainnet scripts/postgresql-setup.sh \
  --create-snapshot db-sync-snapshot-schema-13.7-block-5796064-x86_64 <state-dir>/<slot>
```

For LSM, the script auto-detects and bundles `lsm/snapshots/<slot>/` and `lsm/metadata`.

## Restoring

```
PGPASSFILE=config/pgpass-mainnet scripts/postgresql-setup.sh \
  --restore-snapshot db-sync-snapshot.tgz <state-dir>
```

Creates `<state-dir>` if needed. For LSM, restores both ledger state and LSM database.

## Converting between backends

The `snapshot-converter` tool (shipped with `cardano-node`) converts between InMemory and LSM.
Use the same node version db-sync was built against.

```
# InMemory -> LSM
snapshot-converter \
  --input-mem <state-dir>/<slot> \
  --output-lsm-snapshot <output-dir>/<slot> \
  --output-lsm-database <output-dir>/lsm \
  --config <node-config.json>

# LSM -> InMemory
snapshot-converter \
  --input-lsm-snapshot <state-dir>/<slot> \
  --input-lsm-database <state-dir>/lsm \
  --output-mem <output-dir>/<slot> \
  --config <node-config.json>
```

The `<slot>` directory name must match the slot number in the ledger state.
The `--config` flag takes the **node** config, not db-sync config.
Converting does not modify the source.

## Producing snapshots without a database

`--ledger-only` replays the chain through the ledger and writes nothing else.
db-sync never opens the database in this mode, so there is no connection, no
migration, no query and no insert, and nothing it does depends on any
database's tip. It resumes from the newest snapshot in `--state-dir` and
follows the node from there, writing new snapshots on the usual cadence.

```
cardano-db-sync \
  --config config/mainnet-config.yaml \
  --socket-path <node.socket> \
  --state-dir <state-dir> \
  --schema-dir schema \
  --ledger-only
```

The config needs `"ledger": "enable"`, because the snapshots are the only
output and the other two settings leave the apply result empty so that no
snapshot is ever taken. `PGPASSFILE` is not read and PostgreSQL does not have
to be running; `--schema-dir` is still accepted but unused. `--rollback-to-slot`
is rejected, since it deletes blocks from a database this mode does not open.

Because a snapshot is keyed by chain point and carries nothing
database-specific, one produced this way is loadable by an ordinary db-sync
run. That makes it a way to prepare the ledger state for an upgrade on a second
machine, or alongside a live instance, without touching its database or
competing with it for one.

## Mainnet Snapshots

Available at the [downloads page](https://update-cardano-mainnet.iohk.io/cardano-db-sync/index.html#)
and linked from [releases](https://github.com/IntersectMBO/cardano-db-sync/releases).
