# Revision history for cardano-db

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
