# Revision history for cardano-db-tool

## 11.0.3
* Use same dependencies as 1.30.1 of `cardano-node`.

## 11.0.2
* Fix schema documentation typo (#799).

## 11.0.0
* Alonzo support.
* Database changes as per cardano-db changelog.
* Add a validation to ensure the sum of the AdaPots values per epoch is a constant (#718).
* Add reporting functionalities (balance, teansactions, reward history, latest rewards).

## 10.0.0
* Add helper functionality for state snapshot generation.

## 9.0.0
* No changes for this release.

## 8.0.0
* New package split out of `cardano-db` package so it can use `cardano-db-sync` as a library.
* Add a validation that checks ledger state address balances against database balance.
