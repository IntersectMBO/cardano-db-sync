# Revision history for cardano-db-tool

## 12.0.2
* No changes.

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
* Fix the git SHA hash in the version subcommand (#844).
* Update system requirements (#951).

## 11.0.4
* Fix race condition on insertion of pool offline data or error response (#806, #823, #858).

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
