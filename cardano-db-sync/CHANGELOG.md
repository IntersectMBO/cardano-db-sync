# Revision history for cardano-db-sync

## 11.0.0
* Note that this release requires the database to be dropped and recreated.
* Alonzo support.
* Database changes as per cardano-db changelog.
* Add run time validation that the on-disk schema matches what the app expects (#472).
* Add partial validation of `Reward` table.

## 10.0.1
* Fix docker issue (#686).

## 10.0.0
* Note that this release requires the database to be dropped and recreated. Restoring the `db-sync`
  state can take a long time so it is possible to restore it from a snapsot file.
  See `doc/state-snapshot.md`.
* Ability to create and restore state snapshots (#613).
* Documentation updates.
* Fix network id for reward_addr in pool_update table (#546).
* Update utxo_view query (#543).
* Minor update of database schema.
* Remove `merkle_root` field from `block` table.
* Refactor/improve rollbacks (#570).
* Interleave bulk insertion (`epoch_stake`, `reward` and `orphaned_reward`) with regular insertions.
* Fetch and insert pool offline metadata.
* Update a couple of uniqueness constraints.
* Fix PoolUpdate activeEpochNo field (#610)
* Add EpochSyncTime table (#621).
* Update system requirements in Readme.hd.

## 9.0.0
* Note that this release requires the database to be dropped and recreated.
* Requires ghc-8.10.x tp build.
* Documentation updates.
* Improve DbSync API.
* Add `delegation_slot_no` column to `delegation` table to simplify a query used when populating the
  database.
* Improve the way database inserts are done.
* Fix `blk_count` column in `epoch` table for epochs without transactions (#296).
* Extract new package `cardano-sync` (which contains only the functionality require to sync the
  chain, and avoids all PostgreSQL dependencies) from `cardano-db-sync`.
* Add static (musl64) linux builds.
* Add and populate `ada_pots` table.
* Fix network id for `reward_addr` in `pool_update` table (#546).

## 8.0.0
* Note that this release requires the database to be dropped and recreated.
* Requires version 1.25.0 or later of the node.
* Documentation updates.
* Update dependencies.
* Vastly improve database rollback performance (#256, #397).
* Split the ledger state rewards into valid and invalid sets (#415, #467).
* Fix tx_count for genesis blocks (#471).
* Fix typo in database column name (Merkel -> Merkle) (#446).
* Improve logging.
* Handle case where latest ledger state file cannot be parsed (#483).
* Improve handling/naming of ledger state files.
* Fix Prometheus metrics server (#154).
* Make the port that the Prometheus metrics server listens on configurable (#488).

## 7.1.0
* Upgrading from 7.0.x will not require the database to be dropped and recreated but upgrading from
  6.0.x and earlier will require it.
* Update dependencies.
* Support for the Mary era (ie multi-asset support).
* Fix issues with transactions invalidBefore/invalidHereafter fields.

## 7.0.0
* Note that this release requires the database to be dropped and recreated.
* Update dependencies.
* Support for the Allegra era (ie support for time locking of transactions).
* Log an error if excessive rollback is required on startup. It is up to the operator to drop the
  database and restart.
* Add hash checking when maintaining ledger state and fix race condition (#398).

## 6.0.0

* Note that this release requires the database to be dropped and recreated.
* Update dependencies.
* Maintain a copy of the ledger state which contains data that is not on chain.
* Add Reward table (populated from ledger state).
* Rejig how configuration is handled (it now reads the node's config file).
* Read node config from a path relative to db-sync config (#321).
* Rename ParamUpdate table to ParamProposal.
* Fix uniqueness constraint for PoolRetire table (#306).
* Add and populate EpochParam table (for Shelley not Byron).
* Add and populate EpochStake table (#319).
* Fix handling of PersistRational DbWord64 values (#334).
* Store Bech32 encodings of two more fields (#295).
* Fix bad registeredTxId uniqueness constraint on StakeAddress (#326, #327) (this was never
  included in a release).
* Robustify handling of the Word64 types in database (#334, #351).
* Add version number CLI commands for standard and extended db-sync
* Fix incorrect hash for SlotLeader (#349).
* Fix pool_id column in reward table (#361).
* Add a stake_address_id column to the tx_out table
* Documentation updates.
* Improve the way ProtVer is stored in the database (#368).
* Fix EpochNo column of EpochStake table (#379).
* Add the epoch nonce field to the EpochParam table (#332).

## 5.0.2

* Fix handling of unsigned 64bit integer fields (#334, #335).

## 5.0.1

* Update dependencies.
* Temporary workaround for Unicode NUL character issue (#297).

## 5.0.0

* Note that this release requires the database to be dropped and recreated.
* Correct uniqueness constraint on PoolOwner (#251).
* Resurrect the 'cardano-db-tool validate' functionality.
* Rename (correct and clarify) column names in ParamUpdate table.
* Fix calculation of 'deposit' field of Tx table (#249).
* Add a 'registeredTxId' column to PoolOwner table (#281)

## 4.0.0

* Note that this release requires the database to be dropped and recreated.
* Add and populate 'tx_metadata' table.
* Insert stake deregistrations into correct table. Previously deregistrations were
  inserted into the registration table.
* For all transaction certificates, add index within the transaction (#230).
* Fix certificate ordering issues that resulted in an abort if a MIR certificate was
  found whose output was a stake address that was inserted later in the same tx (#237).
* Make sure stake_address registrations are correctly handled on rollbacks (#238).
* In 'tx_out' table, store the raw as well as rendered version of 'address' (#223).
* In 'stake_address' table, store the raw as well as rendered version (#224).
* Fix epoch start and end times in epoch table (#242).
* Make 'delegation' and 'pool_retire' tables reference 'pool_hash' table instead of 'pool_update.

## 3.1.0 -- July 2020

* * Updates to support the schema additions, see cardano-db 2.0.0 CHANGELOG.md

## 3.0.0 -- July 2020

* Note that this release requires the database to be dropped and recreated.
  It requires cardano-node 1.16 or later.
* Add support for the cardano-node in Cardano mode (#186, #188, #196)

## 2.1.0 -- July 2020

* Note that this release requires the database to be dropped and recreated
* Schema changes, see cardano-db 2.1.0 CHANGELOG.md
* Add handling of Shelley era blocks to the existing handling of Byron era blocks
* Shelley is still not fully supported
* Fix/improve the rollback handling logic

## 2.0.0 -- May 2020

* Note that this release requires the database to be dropped and recreated
* Schema changes, see cardano-db 2.0.0 CHANGELOG.md

## 1.5.0 -- April 2020

* Fix a bug related to block rollback (#61)
* Fix handling of OBFT epochs without transactions (#40)
* Add a block count column to the "epoch" cache table (#42, #46, #51)
* Add validation test for the epoch cache table (#42, #46, #51)
* Add an "Epoch" convenience VIEW to the DB schema (#31)
* Add index to the TransactionInput view (#67)
* Add support for multiple chains in a single database (#76, #77)
* Update dependencies to latest versions (#39, #55, #78)
* Improve README (#32, #38, #57, #58)
* Improve example docker config (#59)

## 1.4.0 -- March 2020

* Renamed from cardano-explorer-node to cardano-db-sync.
* Source repository renamed to cardano-db-sync.
* Improve chain sync behavior when database chain is ahead of node.
* New plugin system to allow application-specific variations (e.g. caching)
* Improve exception handling and reporting.
* Improved documentation.
* Update dependencies to latest versions.
* Use threaded RTS to avoid potential IPC problems on OSX.
* Better default logging verbosity configuration.

## 1.3.0 -- January 2020

* Update dependencies to latest versions.
* Docker image: log all runit services to stdout
* Initial documentation on how to use build and run the components in docker

## 1.2.2 -- January 2020

* Update dependencies to latest versions.

## 1.2.1 -- January 2020

* Update dependencies to latest versions.

## 1.2.0 -- December 2019

* Update to latest version of cardano-ledger, ouroboros-network,
  ouroboros-consensus, iohk-monitoring-framework, and cardano-shell libs.

## 1.1.0 -- December 2019

* Updated to latest network library version and simpler API

## 1.0.0 -- November 2019

* First release of new explorer based on new Cardano node.
* Syncs chain data from a local node into a PostgreSQL DB.
* Compatible with new Cardano node for Byron era.
