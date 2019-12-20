# Revision history for cardano-explorer-db

## 1.2.0 -- December 2019

* Update dependencies to latest versions.

## 1.1.0 -- December 2019

* Added SQL views for benefit of GraphQL frontend (#172)
* Added extra DB indexes for improved query performance (#175).
* Adjusted column names for consistency (#176).
* Run all transactions at an isolation level of Serializable (#189, #133)

## 1.0.0 -- November 2019

* Release to support first release of cardano-explorer-node and
  cardano-explorer.
* PostgreSQL data access for explorer-style applications.
* Support for Byron chain data in the DB schema.
