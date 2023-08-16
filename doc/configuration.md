# Configuration

The initial design of db-sync was a one size fits all approach. It served as a general-purpose
backend for Cardano applications, including light wallets, explorers, etc. Over time, many new
features have been added, including historic rewards with Shelley, scripts with Allegra, multiassets
with Mary, Plutus scripts and redeemers with Alonzo, stake pool metadata with the integration of
SMASH, etc.

While db-sync needs to use the resources that all these features require, many applications use only
a small fraction of these features. Therefore, it is reasonable to introduce flags and options that
turn off some of these features, especially the most expensive ones in terms of performance. These
configuration options require proper documentation, which is presented below.

### --disable-ledger

One of the db-sync features that uses the most resources is that it maintains a ledger state and
replays all the ledger rules. This is the only way to get historic reward details and other data
that is not included in the blocks (ie. historic stake distribution, ada pots, epoch parameters,
etc). The flag --disable-ledger provides the option to turn off these features and significantly
reduce memory usage (by up to 10GB on mainnet) and sync time. Another benefit of this option is
that there are no rollbacks on startup, which tend to take quite some time, since there are no
ledger snapshots maintained on disk.

When this flag is enabled, some features are missing and some DB tables are left empty:
- `redeemer.fee` is left null
- `reward` table is left empty
- `epoch_stake` table is left empty
- `ada_pots` table is left empty
- `epoch_param` table is left empty
- `tx.deposit` is left null (too expensive to calculate without the ledger)

Warning: Running db-sync with this flag and then restarting it without the flag will cause crashes and should be avoided.

Warning: It was previously required to still have a `--state-dir` option provided when in conjunction with `--disable-ledger`. This is no longer the case and now an error will occure if they are both present at the same time. 

If used with docker, this flag can be provided as an extra flag to docker image.

Released snapshots are compatible with these options. Since the snapshots are created without the
option, there still can be some minor inconsistencies. The above data may exist up to the slot/epoch
of the snapshot creation and can be missing afterward. To fix this, when db-sync is initiated with
this flag, it will automatically remove all these data.

Warning: This will irreversibly delete data from existing snapshots.

Here are the exact queries db-sync with this flag will run after restoring a snapshot:

```sql
update redeemer set fee = null;
delete from reward;
delete from epoch_stake;
delete from ada_pots;
delete from epoch_param;
```

### --disable-cache : Experimental

This flag disables the application level caches of db-sync. It slightly reduces memory usage but
increases the syncing time. This flag is worth using only when experiencing significant memory
issues.

### --disable-epoch : Experimental

With this option the epoch table is left empty. Mostly left for historical reasons, since it
provides a negligible improvement in sync time.

### --disable-multiassets : Experimental

Disables the multi assets tables and entries.

### --disable-metadata : Experimental

Disables the tx_metadata table.

### --disable-plutus-extra : Experimental

Disables most tables and entries related to plutus and scripts.

### --disable-offline-data : Experimental

Disables fetching pool offline metadata.

### --consumed-tx-out

Adds a new field `tx_out (consumed_by_tx_in_id)` and populated it accordingly. This allows users to
query the tx_out table for unspent outputs directly, without the need to join with the tx_in table.
If this is set once, then it must be always be set on following executions of db-sync, unless
`prune-tx-out` is used instead.

### --prune-tx-out

If `prune-tx-out` is set it's assumed that --consumed-tx-out is also set, even if it's not.
This flag periodically prunes the consumed tx_out table. So it allows to query for utxo
without having to maintain the whole tx_out table. Deletes to `tx_out` are propagated to `ma_tx_out`
through foreign keys. If this is set once, then it must be always set on following executions of
db-sync. Failure to do this can result in crashed and db-sync currently has no way to detect it.
