# Revision history for cardano-smash-server

## Next
* Added connection pools for smash, instead of opening a connection for each request. Configurable with `--pool` for admins.
* Fix smash server error "Pool is retired" for pool that don't exist. (#997)
* Removed "no-store" from metadata caching (#1075)

## 12.0.2
* Fix PoolOfflineFetchError URL entry (#697).

## 12.0.1
* Fix metadata hash in served JSON.

## 12.0.0
* Port standalone smash to db-sync.
