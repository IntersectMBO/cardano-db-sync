# Pool Offline Data

When a stake pool is registered, a transaction is added to the chain containing the URL of the
pool's offline data.

The data in the transaction specifying the location of the offline data is inserted into the
`PoolMetadataRef` table. `db-sync` then fetches the specified data and inserts it into the
`PoolOfflineData` table.

The basic operation is a follows:

* When the pool registration is found in a tx it is inserted into the `PoolMetadataRef` table.
* Another thread periodically queries the database for `PoolMetadataRef` entries which have either
  not been retrieved or have errored and tries to fetch the data.
* If the new fetch is successful, the result goes in the `PoolOfflineData` table.
* If the fetch fails, an error is recorded in `PoolOfflineFetchError table` to be retried later.
* The retries on fetch fail start at 60 seconds and are progressively longer and eventually plateau
  at a retry every 24 hours.
