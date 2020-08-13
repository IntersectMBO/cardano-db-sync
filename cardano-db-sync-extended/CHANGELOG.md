# Revision history for cardano-db-sync-extended

## 4.0.0 -- August 2020

* Note that this release requires the database to be dropped and recreated.
* Add and populate 'tx_metadata' table.

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
