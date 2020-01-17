# Revision history for cardano-explorer-webapi

## 1.2.2 -- January 2020

* Swagger docs https://input-output-hk.github.io/cardano-explorer/
* Fix /api/blocks/txs/{blkHash} endpoint (#195)
* Fix Ada/Lovelace denomination bug (#197)
* Fix JSON rendering for addresses to match old API
* Add validation for genesis address paging (#219)
* Add additional tests (#222, #227)
* Update dependencies to latest versions.

## 1.2.1 -- January 2020

* Update dependencies to latest versions.

## 1.2.0 -- December 2019

* Update dependencies to latest versions.

## 1.1.0 -- December 2019

* Renamed from cardano-explorer to cardano-explorer-webapi (#193).

* Remove unused/unsupported endpoints (#198)

* Provide more info in /api/txs/summary/{txHash} endpoint (#174).

  Specifically, for each transaction replace '(address, coin)' with
  a struct that also contains the transaction '(hash, index)' pair.

* Provide more info about transactions in some endpoints (#174, #131).

  Specifically:
    * /api/blocks/txs/{blockHash}
    * /api/txs/summary/{txHash}

* Add ChainTip info to address endpoints (#177, #130)

* Run all transactions at an isolation level of Serializable (#189, #133)

* Document atomicity of API interactions

* Fix validation error (#191)

* Add additional tests to validiate against the old API (#190)

## 1.0.0 -- November 2019

* First release of new explorer API server based on new cardano-explorer-node.
* New modular design, no longer integrated with the cardano-sl node.
* Gets all data from a local PostgreSQL DB.
* Compatible with the old explorer HTTP API and old web frontend.
* Some small compatible API extensions
