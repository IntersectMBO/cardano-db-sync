# Revision history for cardano-db-sync node

## 2.0.0 -- April 2020

* Schema change to removes SQL views previously used by by Cardano GraphQL (#92)

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
