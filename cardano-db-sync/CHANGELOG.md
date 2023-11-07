# Revision history for cardano-db-sync

## Sancho changelog
`sancho-a-b-c` tags in db-sync are a series of incremental integration of conway feautures. These tags can follow the
sanchonet https://book.world.dev.cardano.org/environments.html#sanchonet-testnet.
Their enumeration has the following meaning:<br>
a: schema changes<br>
b: compatible with a respinned sanchonet or node<br>
c: minor changes or fixes<br>

It is advised to resync from genesis when using a new version a or b.

Some useful links (adjust the numbers to the correct tag):

- Changelog: https://github.com/input-output-hk/cardano-db-sync/blob/sancho-2-0-0/cardano-db-sync/CHANGELOG.md#13200
- Schema docs https://github.com/input-output-hk/cardano-db-sync/blob/sancho-2-0-0/doc/schema.md
- Postgres migrations https://github.com/input-output-hk/cardano-db-sync/tree/sancho-2-0-0/schema

In the schema docs, you can search for `13.2` or `Conway` for schema changes from the previous official release.

### sancho-2.2.0
- is compatible with node 8.6-pre
- `governance_action.ratified_epoch` is now populated
- `drep_distr.active_until` is now populated
- Fixed typo `voting_procedure.comittee_voter` to `voting_procedure.committee_voter`
- Added `committee_de_registration.voting_anchor_id` field

### sancho-2.1.0
- Fixed an issue with the size of Committee hash

### sancho-2.0.1
- Workaround an issue where delegation could target unregistered pools

### sancho-2.0.0
- is compatible with node 8.5-pre
- adds the governance epoch param group in table `epoch_param` and their updates through governance proposal in
`param_proposal`
- adds a new field `governance_action.expiration` for the expected expiration of an action.
- populates the `drep_distr` table and `governance.enacted_epoch` that used to remain empty.
- fixes an issue with duplicated entries in `drep_hash` and `voting_anchor`.
- merges drep registration, re-registration and de-registration on the same table `drep_registration` for easy use
- the new field `drep_distr.active_until` is not populated yet.
- change `new_committee` gov actions represantation.

### sancho-1.1.0
- is compatible with node-8.3-pre. There are no schema changes over sancho-1-0-0.

### sancho-1.0.0
The schema is quite close to the [initial design](https://github.com/input-output-hk/cardano-db-sync/blob/conway-schema-design-13.2/doc/schema.md). You may find some differences: ie the `param_proposals` and `epoch_param` are not extended yet,
Some tables/fields are created but are not populated yet: `anchor_offline_data`, `anchor_offline_fetch_error` , `drep_distr` , `governance_action.x_epoch` , `delegation_vote.redeemer_id`

### sancho-0.0.0
It's very early stage and is missing all Conway specific feautures and some Shelley features related to Rewards. Incremental tags will follow.

## 13.2.0.0 - Unreleased
* Uses the cache for the computation of `epoch` table when following
* `epoch_stake` is now computed earlier, before the epoch is reached
* Exception handling and concurrency improved.
* When `--disable-ledger` is used, db-sync won't populate the `tx.deposit` field as it's too expensive
* Support for Conway (see schema docs)
* Added a `--bootstrap-tx-out` flag. Read more at `doc/configuration.md`.
* When `--consumed-tx-out` is enabled, db-sync won't populate the `tx_in` table
* Added flags `--force-tx-in` which bypasses the default behaviour mentioned above
* Migrations from consume to prune are now much faster
* References to `offline` for pool metadata are replaced by `offchain`
* Added a new table `extra_migrations`
* DBSync no longer does any query to the `tx_out` table during syncing
* Added flags `--disable-in-out`, `--disable-shelley`, `--disable-gov`, `--only-gov`

## 13.1.1.3
* Adds compatibility with node-8.0.0 [#1403] and node-8.1.1 [#1455]
* Adds compatibility with the new format of the CostModel in Alonzo genesis file [#1403]
* Adds a procedure which fixes scripts with wrong cbor [Issue #1348]
* Fixes an issue where the `ada_pots` didn't match exactly the epoch boundary [Issue #1258]
* Adds Developmental ghc-9.2 compatibility [#1404]
* Provided experimental options which turn off parts of the schema [#1379]
* Fixed an issue where `epoch` table didn't rollback [#1370]
* Fixed an issue with smash returning `InvalidUrlException` [#1353]
* Takes less snapshots, since big rollbacks are now faster
* When `--disable-ledger` is enabled a `--state-dir` is no longer required
* Added experimental extra migrations, controlled by `consumed-tx-out` and `prune-tx-out`

## 13.1.0.2
* Upgrade dependencies and integrates a fix in ledger internal data structures.
* Integrates Cardano Haskell Packages (CHaP)

## 13.1.0.0
* Avoids rollbacks on restarts, making them way faster [#1190]
* Syncing speed is increased
* Allows to migrate from previous 13.x releases without resync [#1172]
* Creates a procedure that fixes old values related to plutus data [#1214] as the first step of migration [#1278]
* Removes many unique keys that were never used [#1087]
* Delays the creation of most indexes while syncing. They are created when syncing is almost complete [#1293]
* Removed all foreign keys [#1082]
* Reworked the way that rollback works, using reverse indexes.
* Aded a new `reverse_index` table to speed up rollbacks
* Added experimental flags `skip-plutus-data-fix`, `only-plutus-data-fix`, `force-indexes` to `cardano-db-sync`
* Added experimental flags `force-indexes`, `mock-fix` for `cardano-db-tool run-migrations` command
* Fixed Stake Pool metadata fetch error exceeded 512 bytes [#1270]
* Snapshot creation no longer rollbacks to the ledger snapshot file, so it's faster
* Columns `stake_address.tx_id`, `cost_model.block_id` are removed
* Bump iohk-nix to fetch the correct config for preview and preprod respin
* Added new flag `POSTGRES_ARGS` (wih reccomended default values) to `docker-compose.yml` for customization of database settings

## 13.0.5
* Fixed an issue where `StakeAdress` cache was not cleaned up properly and could cause crashes [#1222]
* Fixed an issue where fees for txs with phase 2 failure which din't include the total_collateral fields appeared to be 0 [#1242]
* Fixed an issue where `Datum` were reserialised and could be inserted in a different CBOR format [#1214]
* Improved docker documentation
* Made disable options like `disable-ledger` easily available through docker
* Supported new networks, preview, preprod

## 13.0.4
* Bump to the latest node release.

## 13.0.3
* Integrated the fix for the missused minfee function in ledger (https://github.com/input-output-hk/cardano-ledger/pull/2938)

## 13.0.2
* Integrated the fix of the obsolete check in the new 'Praos' protocol (https://github.com/input-output-hk/ouroboros-network/pull/3891)

## 13.0.1
* Ensure Babbage TxOut decoder can't fail due to malformed Ptr. This bug manifests itself if db-sync is running in the Babbage era and shuts down, it has to re-sync from an Alonzo snapshot, or from Genesis if it doesn't exist. (#1181)

## 13.0.0
* Added `--disable-ledger` flag, which significantly reduces memory usage. Read more at `doc/configuration.md`.
* Reduction in memory usage.
* Reduction in sync-time.
* Added `--disable-cache` flag, which slightly reduces memory usage. Read more at `doc/configuration.md`.
* Renamed `--no-epoch-table` flag to `disable-epoch`. Read more at `doc/configuration.md`.
* Handle `PlutusV1` and `PlutusV2` scripts as seperate script types, removing old `Plutus` type
* Store CBOR serialized `Datum` and `Redeemer` via `bytes` field
* HF integration. Schema changes for new Babbage fields. (#1081).
* Improved logging. It includes some performance statistics.
* Used pulsing rewards coming from new ledger events. Also incremental stake slices.
* Extended the caches.
* `BulkOperations` which affected memory usage and rollbacks are removed.
* Removed "no-store" from metadata caching for smash (#1075)
* Added connection pools for smash, instead of opening a connection for each request. Configurable with `--pool` for admins.
* Reduced the blocks it takes for prometheus block height to be updated.
* Add docker healthchecks for postgres and cardano-node.
* Fix unique keys of `reserves`, `treasury`, `pool_update`, `pool_retire`, `stake_registration`, `stake_deregistration`, `delegation` (#1051).
* Allow to add additional migrations (indexes and views) (#1044).
* Fix smash server error "Pool is retired" for pools that don't exist. (#997)
* Fix duplicate instant rewards (#981).
* Add multiple unit tests.
* `pool_owner` now references `pool_update` instead of `pool_hash` and `tx` (#986).
* Fix handling of StakeRefPtr (#1024).
* Store `requiredSigners` (transaction extra key witnesses).
* outputs, inputs and multi asset outputs are stored in batched grouped by blocks, instead of one by one.
* Fix parameter_proposal.max_block_size (#965).
* Remove plugin System, merges 3 packages `cardano-db-sync-extended`, `cardano-sync` and `cardano-db-sync` into the last.
* Fixes on testnets that fork directly to Shelley (#953).
* Log cabal version, git hash and command line options on startuo (#1166).

## 12.0.2
* Fix PoolOfflineFetchError URL entry (#697).

## 12.0.1
* No changes.

## 12.0.0
* Note that this release requires the database to be dropped and recreated.
* Update `rewardtype` enum (used in `reward` table) to include a pool deposit refund type.
* Include `json` and (raw) `bytes` fields to `script` table.
* Add `cost_model`, `datum` and `redeemer` tables.
* Update `cost_model*` fields of `param_proposal` and `epoch_param` tables to reference `cost_model`
  table.
* Unify SQL types of epoch_stake.epoch_no and epoch.no (#811).
* Fix missing and inconsistent rewards issues (#791, #796, #805. #882, #826, #918, #921, #923, #939,
  #947).
* Handle the empty list case in insertManyUncheckedUnique (#869).
* Add a `multi_asset` table with `policy`, `name` and asset `fingerprint` columns (#868).
* Drop the `policy` and `name` columns of `ma_tx_mint` and `ma_tx_out` tables, replacing those
  columns with a reference to the `multi_asset` table.
* Stop supporting test configs which don't initiate Shelley properly.
* Fix handling of transactions which have contracts which fail second stage validation (#883).
* Update system requirements (#951).

## 11.0.4
* Fix race condition on insertion of pool offline data or error response (#806, #823, #858).

## 11.0.3
* Use same dependencies as 1.30.1 of `cardano-node`.
* Fix race condition on insertion of pool offline data or error response (#806, #823, #831).

## 11.0.2
* Fix schema documentation typo (#799).
* Fix race condition on insertion of pool offline data or error response (#806, #823).
* Fix docker issue (#810).

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
