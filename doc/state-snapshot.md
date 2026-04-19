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

## Mainnet Snapshots

Available at the [downloads page](https://update-cardano-mainnet.iohk.io/cardano-db-sync/index.html#)
and linked from [releases](https://github.com/IntersectMBO/cardano-db-sync/releases).
