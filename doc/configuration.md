# Configuration

The initial design of db-sync was an one fit all approach, where it would serve as a general purpose backend for cardano applications, including light wallets, explorers etc. Over the time many new feautures have been added, like historic rewards with Shelley, scripts with Allegra, multiassets with Mary, plutus scripts and reeemers with Alonzo, pool metadata with the integration of smash etc. Many applications use only a small fraction of these feautures, but db-sync needs to use the resources that all these features require. It is reasonable to introduce flags and options that turn off some of these features, expecially the most expensive ones in terms of performance. These configuration options require proper documentation, which is presented below

### --disable-ledger

One of the features of db-sync that uses the most recourses is that it maintains a ledger state and it replays all the ledger rules. This is the only way to get historic rewards an other things that are not included in the blocks, like historic stake distribution, ada pots, epoch params etc. The flag --disable-ledger provides the option to turn off these feauture an significantly reduces usage of memory (by up to 10GB) and sync time.

When this flag is enables, some feautures are missing and some db tables are left empty:
- `redeemer.fee` is left null
- `reward` table is left empty
- `epoch_stake` table is left empty
- `ada_pots` table is left empty
- `epoch_param` table is left empty
