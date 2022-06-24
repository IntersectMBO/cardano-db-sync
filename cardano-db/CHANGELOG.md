# Revision history for cardano-db

## 13.0.0
* Add `extra_key_witness` table for storing `requiredSigners` (transaction extra key witnesses).
* Drop `Plutus` `ScriptType` in favor of `PlutusV1` and `PlutusV2`

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
* Update system requirements (#951).

## 11.0.4
* Fix race condition on insertion of pool offline data or error response (#806, #823, #858).

## 11.0.3
* Use same dependencies as 1.30.1 of `cardano-node`.

## 11.0.2
* Fix schema documentation typo (#799).

## 11.0.0
* Note that this release requires the database to be dropped and recreated.
* Many changed to support Alonzo release.
* Add Alonzo (smart contracts) related Redeemer and Script tables.
* Add an `epochNo` columm to StakeRegistration/Deregistration tables.
* Add instantaneous rewards to the `Reward` table. This adds two new reward sources (`treasury` and
  `reserves` to the existing `owner` and `member` types) and makes the `poolId` NULLable (treasury
  and reserves rewards are not associated with a pool).
* For the `EpochSyncTime` table change the `seconds` column from `double` to `Word64` and make it
  non-NULLable.
* Fix the uniqueness constraint on the `CollateralTxIn` and `PoolMetadataRef` tables.
* For the `Reward` table, drop the `epochNo` column and relace it with `earnedEpoch` and
  `spendableEpoch`
* Extend `ProtocolParams` and `EpochParams` tables for Alonzo.
* Add `opCertCounter` column to `Block` table.
* Drop `OrphanedRewrd` table (because it was incomplete can cannot be fixed).
* Fix `utxo_view` defined in schema (#744).
* Add `dataHash` field to the `TxOut` table.

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

## 9.0.0
* Note that this release requires the database to be dropped and recreated.
* Requires ghc-8.10.x tp build.
* Documentation updates.
* Add `delegation_slot_no` column to `delegation` table to simplify a query used when populating the
  database.
* Add static (musl64) linux builds.
* Add and populate `ada_pots` table.

## 8.0.0
* Note that this release requires the database to be dropped and recreated.
* Documentation updates.
* Update dependencies.
* Fix typo in database column name (Merkel -> Merkle) (#446).
* `cardano-db-tool` moved to a separate package in this repo.

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
* Rename ParamUpdate table to ParamProposal.
* Fix uniqueness constraint for PoolRetire table (#306).
* Add and populate EpochParam table (for Shelley not Byron).
* Add and populate EpochStake table (#319).
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
* Add the epoch nonce field to the EpochParam table (#332).

## 5.0.2

* Fix handling of unsigned 64bit integer fields (#334, #335).

## 5.0.1

* Update dependencies.

## 5.0.0

* Note that this release requires the database to be dropped and recreated.
* Correct uniqueness constraint on PoolOwner (#251).
* Resurrect the 'cardano-db-tool validate' functionality.
* Rename (correct and clarify) column names in ParamUpdate table.
* Fix calculation of 'deposit' field of Tx table (#249).
* Add a 'fees' column to the 'Epoch' table (#275).
* Add a 'registeredTxId' column to PoolOwner table (#281)

## 4.0.0

* Note that this release requires the database to be dropped and recreated.
* Add and populate 'tx_metadata' table.
* For all transaction certificates, add index within the transaction (#230).
* In 'tx_out' table, store the raw as well as rendered version of 'address' (#223).
* In 'stake_address' table, store the raw as well as rendered version (#224).
* Fix epoch start and end times in epoch table (#242).
* Make 'delegation' and 'pool_retire' tables reference 'pool_hash' table instead of 'pool_update.
* Add database indices on 'tx_out' and 'pool_update'.

## 3.1.0 -- July 2020

* Add a `payment_cred` column to the tx_out table for the benefit of wallet
  implementations (#208)
* Add a `slot_in_epoch` column to the block table. Slot numbers become more
  complicated to calculate once there are multiple eras with different slot
  lengths, so we now store it instead. (#214)
* Support for storing the on-chain parameter update proposals (#217)
* MIR certs now go into their own table(s) rather than the reward table (#218)

## 3.0.0 -- July 2020

* Note that this release requires the database to be dropped and recreated
* Further Shelley schema additions and changes (#173, #174, #199)
* Fix to support very large values of per-epoch aggregate tc outputs (#201)
* Support Word64 column types properly (#203)

## 2.1.0 -- July 2020

* Note that this release requires the database to be dropped and recreated
* The database schema has been adjusted to accept the superset of Byron and
  Shelley era information
* Shelley is still not fully supported
* Remove last hard coded values for slots per epoch

## 2.0.0 -- May 2020

* Note that this release requires the database to be dropped and recreated
* Schema change to removes SQL views previously used by by Cardano GraphQL (#92)
* Add a column to the Tx table to record the order of txs within a block (#94)
* Flatten the DB schema migrations

## 1.5.0 -- April 2020

* Added library API for query to determine if the DB is fully in sync (#36)
* Export the generated DB schema as documentation (#43)
* Update dependencies to latest versions (#39, #78)


## 1.4.0 -- March 2020

* Renamed from cardano-explorer-db to cardano-db
* Source repository renamed to cardano-db-sync
* Extra "epoch" table used by cardano-db-sync-extended
* Update dependencies to latest versions.

## 1.3.0 -- January 2020

* Update dependencies to latest versions.

## 1.2.2 -- January 2020

* Update dependencies to latest versions.
* Allow building with latest version of persistent library.

## 1.2.1 -- January 2020

* Update dependencies to latest versions.

## 1.2.0 -- December 2019

* Update dependencies to latest versions.

## 1.1.0 -- December 2019

* Added SQL views for benefit of GraphQL frontend (#172)
* Added extra DB indexes for improved query performance (#175).
* Adjusted column names for consistency (#176).
* Run all transactions at an isolation level of Serializable (#189, #133)

## 1.0.0 -- November 2019

* Release to support first release of cardano-db-sync node and
  cardano-explorer.
* PostgreSQL data access for explorer-style applications.
* Support for Byron chain data in the DB schema.
