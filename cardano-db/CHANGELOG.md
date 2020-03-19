# Revision history for cardano-db

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
