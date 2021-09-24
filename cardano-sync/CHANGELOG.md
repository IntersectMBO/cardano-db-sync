# Revision history for cardano-sync

## 11.0.3
* Use same dependencies as 1.30.1 of `cardano-node`.

## 11.0.2
* Fix schema documentation typo (#799).
* Fix race condition on insertion of pool offline data or error response (#806, #823).

## 11.0.0
* Alonzo support.
* Database changes as per cardano-db changelog.

## 10.0.1
* Fix docker issue (#686).

## 10.0.0
* Changes required for other changes.

## 9.0.0
* Extract new package `cardano-sync` (which contains only the functionality require to sync the
  chain, and avoids all PostgreSQL dependencies) from `cardano-db-sync`.

