# Configuration

The initial design of db-sync was an one fit all approach, where it would serve as a general purpose
backend for cardano applications, including light wallets, explorers etc. Over the time many new
feautures have been added, like historic rewards with Shelley, scripts with Allegra, multiassets
with Mary, Plutus scripts and reeemers with Alonzo, pool metadata with the integration of SMASH etc.
Many applications use only a small fraction of these feautures, but db-sync needs to use the
resources that all these features require. It is reasonable to introduce flags and options that
turn off some of these features, expecially the most expensive ones in terms of performance. These
configuration options require proper documentation, which is presented below

### --disable-ledger

One of the features of db-sync that uses the most resources maintaining the ledger state and
replaying all the ledger rules. This is the only way to get historic rewards and other things that
are not included in the blocks, like historic stake distribution, ada pots, epoch params etc. The
flag `--disable-ledger` provides the option to turn off these feauture and significantly reduces
usage of memory (by up to 10GB on mainnet) and sync time.

When this flag is enabled, some feautures are missing and some db tables are left empty:
- `redeemer.fee` is left null
- `reward` table is left empty
- `epoch_stake` table is left empty
- `ada_pots` table is left empty
- `epoch_param` table is left empty

Released snapshots are compatible with this options. There could be some small inconsistencies
though, since the snapshots are created using the option. So the above data may exist up to the
slot/epoch of the snapshot creation and missing afterwards. The simplest way to fix this, is to
delete the existing data. Since this is still an experimental feauture, db-sync doesn't do anything
automatically, since we don't know how users want to use the feature.

Some queries someone may want to run after restoring a snapshot:

```sql
update redeemer set fee = null;
delete from reward;
delete from epoch_stake;
delete from ada_pots;
delete from epoch_param;
```

### --disable-cache : Experimental

This disables the application level caches of db-sync. It reduces memory usage by a bit, but
increases sync time. Usually this flag is not worth it, unless there are some pretty big memory
issues.

## --disable-epoch : Experimental

With this option the epoch table is left empty. Mostly left for historical reasons, since it
provides a negligible improvement in sync time.

