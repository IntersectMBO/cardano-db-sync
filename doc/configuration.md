# Configuration

The initial design of db-sync was a one fit all approach. It served as a general-purpose backend for Cardano applications, including light wallets, explorers, etc. Over time, many new features have been added, including historic rewards with Shelley, scripts with Allegra, multi assets with Mary, Plutus scripts and redeemers with Alonzo, stake pool metadata with the integration of SMASH, etc. 

While db-sync needs to use the resources that all these features require, many applications use only a small fraction of these features. Therefore, it is reasonable to introduce flags and options that turn off some of these features, especially the most expensive ones in terms of performance. These configuration options require proper documentation, which is presented below.

### --disable-ledger

One of the db-sync features that uses the most resources is that it maintains a ledger state and replays all the ledger rules. This is the only way to get historic reward details and other data that is not included in the blocks (ie. historic stake distribution, ada pots, epoch parameters, etc). The flag --disable-ledger provides the option to turn off these features and significantly reduce memory usage (by up to 10GB on mainnet) and sync time.

When this flag is enabled, some features are missing and some DB tables are left empty:
- `redeemer.fee` is left null
- `reward` table is left empty
- `epoch_stake` table is left empty
- `ada_pots` table is left empty
- `epoch_param` table is left empty

Released snapshots are compatible with these options. Since the snapshots are created using the option, there still can be some minor inconsistencies. The above data may exist up to the slot/epoch of the snapshot creation and can be missing afterward. The simplest way to fix this is to delete the existing data. Since this is still an experimental feature (and it is not yet clear how users would like to use this feature), db-sync doesn't do anything automatically. 

Here are some queries you might want to run after restoring a snapshot:

```sql
update redeemer set fee = null;
delete from reward;
delete from epoch_stake;
delete from ada_pots;
delete from epoch_param;
```

### --disable-cache

This flag disables the application level caches of db-sync. It slightly reduces memory usage but increases the syncing time. This flag is worth using only when experiencing significant memory issues.

