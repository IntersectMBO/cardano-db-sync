# The Plugin System

The `cardano-db-sync` functionality is intended to be reasonably minimal. However, in order to
make it more flexible, it offers a plugin/extension system. This plugin system allows the existing
`cardano-db-sync` functionality to be used as a library which can then be reasonably easily
extended.

## Limitations

The main limitation is that extensions should never extend, modify or write to any existing table
(reading however is OK). The extension should only ever write or modify tables that are not part
of the core set of database tables.

## Operation

The plugin system is defined in the [Plugin][plugin] modules. Plugins would implement 3 functions,
one for each of three operations:

* Startup - Called once every time the program is started.
* Insert block - Called each time a new block arrive (including Epoch Boundary Blocks).
* Rollback - Called on a chain rollback event.

The [default][default] plugin specifies the set of functions (only insert and rollback in this
case) needed for standard operation of the `cardano-db-sync` program.

In order to run a version of the `cardano-db-sync` with extension, it is necessary to duplicate
the top-level `cardano-db-sync` [program][program] which just does command line parsing and
then calls into the library that implements the database/node functionality with the required
extended `DbSyncNodePlugin` struct.

New tables are added to the database schema by updating the [schema definition][schema], and then
following the [schema management documentation][schema-doc].

## An Example Plugin

An useful example plugin has been written to create and update an `Epoch` table. This table was
initially done as a PostgreSQL `VIEW` but the performance was very poor. Changing it from a `VIEW`
to a table updated by a plugin, resulted in a huge performance improvement over the `VIEW` version.

The epoch table plugin is implemented in [epoch][epoch] which specifies all three of the plugin
operations. The extended `DbSyncNodePlugin` is [extended][extended] by calling the plugin
functions after the default functions (it is also possible to call them before if needed, but
extending at the head of the lists in the `DbSyncNodePlugin` instead of the tail) and adding
a new top-level [program][new-top-level].



[default]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Plugin/Default.hs
[epoch]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Plugin/Epoch.hs
[extended]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync-extended/src/Cardano/DbSync/Plugin/Extended.hs
[new-top-level]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync-extended/app/cardano-db-sync-extended.hs
[plugin]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Plugin.hs
[program]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync/app/cardano-db-sync.hs
[schema]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db-sync-db/src/Cardano/Db/Schema.hs
[schema-doc]: https://github.com/input-output-hk/cardano-db-sync/blob/master/doc/schema-management.md
