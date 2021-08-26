# Revision history for cardano-sync

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

