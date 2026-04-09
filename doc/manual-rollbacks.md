# Manual Rollbacks

## Overview

Manual rollbacks allow you to revert the database to a previous state by deleting blocks and related data from a specified point forward. This is useful when:

- Fixing incorrect data after deploying a bug fix
- Recovering from a corrupted database state
- Testing and development scenarios

## Methods

There are two ways to perform manual rollbacks:

1. **cardano-db-sync --rollback-to-slot**: Rollback before starting db-sync
2. **cardano-db-tool rollback**: Standalone rollback tool (db-sync must be stopped)

## Using cardano-db-sync --rollback-to-slot

Use this method to rollback before starting normal sync operations:

```bash
cardano-db-sync \
  --config config/mainnet-config.yaml \
  --socket-path /path/to/node.socket \
  --state-dir /path/to/ledger-state \
  --schema-dir schema/ \
  --rollback-to-slot 134892800
```

The rollback will be performed, then db-sync will start syncing from that point forward.

## Using cardano-db-tool rollback

Use this method for standalone rollback operations:

```bash
# Stop db-sync first!
PGPASSFILE=config/pgpass-mainnet cardano-db-tool rollback --slot 134892800
```

After the rollback completes, restart db-sync to re-sync from the rollback point.

## What Gets Deleted

When you rollback to a specific slot, the following tables are affected:

### Epoch-Related Tables
- `epoch` - Epoch entries for the rollback epoch
- `drep_distr` - DRep distribution for epochs after rollback
- `reward_rest` - Instant rewards for epochs after rollback
- `pool_stat` - Pool statistics for epochs after rollback
- `epoch_param` - Epoch parameters

### Block and Transaction Tables
- `block` - Blocks at or after the rollback slot
- `tx` - All transactions in deleted blocks
- `tx_in` - Transaction inputs
- `tx_out` / `tx_out_address` - Transaction outputs (variant dependent)
- `ma_tx_out` / `ma_tx_out_address` - Multi-asset transaction outputs
- `tx_metadata` - Transaction metadata
- `tx_cbor` - Transaction CBOR data

### Stake and Delegation Tables
- `stake_registration` - Stake address registrations
- `stake_deregistration` - Stake address deregistrations
- `delegation` - Stake delegations
- `delegation_vote` - Voting delegations
- `withdrawal` - Reward withdrawals

### Pool Tables
- `pool_retire` - Pool retirement certificates

### Plutus Tables
- `redeemer` - Script redeemers
- `redeemer_data` - Redeemer data
- `script` - Scripts
- `datum` - Datums
- `collateral_tx_in` - Collateral inputs
- `collateral_tx_out` / `collateral_tx_out_address` - Collateral outputs
- `reference_tx_in` - Reference inputs
- `extra_key_witness` - Extra key witnesses

### Multi-Asset Tables
- `ma_tx_mint` - Multi-asset minting

### Governance Tables (Conway era)
- `gov_action_proposal` - Governance action proposals
- `voting_procedure` - Voting procedures
- `committee_registration` - Committee registrations
- `committee_de_registration` - Committee deregistrations
- `drep_registration` - DRep registrations
- `treasury_withdrawal` - Treasury withdrawals
- `param_proposal` - Protocol parameter proposals
- `voting_anchor` - Voting anchors
- `off_chain_vote_data` - Off-chain vote data
- `off_chain_vote_fetch_error` - Vote fetch errors
- Various other off-chain vote related tables

### Treasury Tables
- `treasury` - Treasury movements
- `reserve` - Reserve movements
- `pot_transfer` - Pot transfers
- `ada_pots` - ADA pot snapshots

### Pool Metadata
- `pool_metadata_ref` - Pool metadata references
- `pool_offline_data` - Pool offline metadata
- `pool_offline_fetch_error` - Pool metadata fetch errors

### Indexes
- `reverse_index` - Reverse indexes for rollback optimization

Note: The exact tables affected depend on the schema variant (tx_out vs tx_out_address) and which era features are active.

## Finding the Right Slot Number

To find a slot number for a specific epoch or time:

1. **By Epoch**: Use a block explorer to find the first slot of the desired epoch
2. **By Time**: Query the database:
   ```sql
   SELECT slot_no, block_no, time 
   FROM block 
   WHERE time >= '2026-03-01 00:00:00' 
   ORDER BY slot_no ASC 
   LIMIT 1;
   ```
3. **By Block Number**: Query the database:
   ```sql
   SELECT slot_no, block_no, epoch_no 
   FROM block 
   WHERE block_no = 12345678;
   ```

## What Happens After Rollback

1. **Database State**: All blocks and related data from the rollback point onwards are removed
2. **Epoch Data**: Epoch entries are deleted for affected epochs. When db-sync re-syncs, it will re-insert this data with the corrected logic
3. **Re-sync**: When db-sync restarts, it will:
   - Detect the new database tip
   - Request blocks from the node starting at that point
   - Re-process and re-insert all the deleted data

## Important Considerations

### Choosing the Rollback Point

- **For bug fixes**: Roll back to just before the incorrect data starts
- **For epoch issues**: Roll back to the beginning of the affected epoch or slightly before
- **Safety margin**: Consider rolling back a bit further than strictly necessary to ensure clean state

### Ledger State

If using ledger state (`--state-dir`), db-sync will:
- Attempt to load a ledger state snapshot at or before the rollback point
- If no suitable snapshot exists, it may need to replay from an earlier point
- Ledger state snapshots are automatically managed during re-sync

### Performance

- Small rollbacks (< 1000 blocks): Complete in seconds
- Medium rollbacks (1000-10000 blocks): Complete in minutes  
- Large rollbacks (> 10000 blocks): May take longer depending on database size

## Verifying Rollback Success

After rollback, check the database tip:

```sql
SELECT slot_no, block_no, epoch_no, time 
FROM block 
ORDER BY id DESC 
LIMIT 1;
```

The result should show a block at or just after your specified rollback slot.

## Common Issues

### "No block found at or after slot"

This means the specified slot doesn't exist in the database. The rollback will find the nearest block at or after the specified slot. If no such block exists, the rollback will do nothing.

### Database Constraint Errors on Re-sync

If you see unique constraint violations after a rollback, this may indicate:
- The rollback wasn't performed correctly (fixed in recent versions)
- Manual cleanup may be needed for some tables
