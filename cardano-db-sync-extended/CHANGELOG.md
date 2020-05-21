# Revision history for cardano-db-sync-extended

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
