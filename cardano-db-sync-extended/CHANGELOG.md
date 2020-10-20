# Revision history for cardano-db-sync-extended

## 5.0.3

* Workaround fix handling of unsigned 64bit integer fields (#351)

## 5.0.2

* Fix handling of unsigned 64bit integer fields (#334, #335)

## 5.0.1

* Update dependencies.
* Temporary workaround for Unicode NUL character issue (#297).

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

* Schema additions in the base db-sync, see cardano-db 2.0.0 CHANGELOG.md

## 3.0.0 -- July 2020

* Note that this release requires the database to be dropped and recreated.
  It requires cardano-node 1.16 or later.
* Fix to support very large values of per-epoch aggregate tc outputs (#201)

## 2.1.0 -- July 2020

* Note that this release requires the database to be dropped and recreated
* Schema changes, see cardano-db 2.1.0 CHANGELOG.md
* Shelley is still not fully supported
* Fix updating of Epoch table

## 2.0.0 -- May 2020

* Note that this release requires the database to be dropped and recreated
* Schema changes, see cardano-db 2.0.0 CHANGELOG.md
* Be more conservative on when the Epoch table is updated to improve sync speed

## 1.5.0 -- April 2020

* Update dependencies to latest versions (#39, #78)

## 1.4.0 -- March 2020

* New component to support SQL DB backend for cardano-graphql
* Based on cardano-db-sync plugin mechanism
* Extra "epoch" DB table with incrementally maintained view
