# Pool OffChain Data

_(as of 13.2.0.0 Offline was renamed to OffChain)__

When a stake pool is registered, a transaction is added to the chain containing the URL of the
pool's offchain data.

The data in the transaction specifying the location of the offchain data is inserted into the
`PoolMetadataRef` table. `db-sync` then fetches the specified data and inserts it into the
`OffChainPoolData` table.

The basic operation is a follows:

* When the pool registration is found in a tx it is inserted into the `PoolMetadataRef` table.
* Another thread periodically queries the database for `PoolMetadataRef` entries which have either
  not been retrieved or have errored and tries to fetch the data.
* If the new fetch is successful, the result goes in the `OffChainPoolData` table.
* If the fetch fails, an error is recorded in `PoolOffChainFetchError table` to be retried later.
* The retries on fetch fail start at 60 seconds and are progressively longer and eventually plateau
  at a retry every 24 hours.

### http-get-json-metadata

This is an testing/debugging program. It uses *the same code* to fetch the offchain metadata as
`cardano-db-sync`. It is not code copy, this application calls the function in `db-sync` that does
the actual fetch (the function is named `httpGetOffChainData`).

On success, this program will print "Success" and then print the retrieved JSON.

On failure, the error message is printed.

The program can be run from the `cardano-db-sync` git checkout as:
```
> cabal run -- http-get-json-metadata <url>
```

If you also want to check the JSON meta data hash, the program can be run as:
```
> cabal run -- http-get-json-metadata <url> <hash>
```
where the hash is specified as a string of hexadecimal digits.

If you don't know the hash, you can provide an empty string for this field and the program will
print the execpted hash in the error message. Eg:
```
> cabal run -- http-get-json-metadata https://git.io/Jt01O ""
Up to date
Hash mismatch from when fetching metadata from https://git.io/Jt01O. Expected  but got
   54796c0c86228b9c86a1982e76441b293d01b7a9cd1309d43ae95123a8ad8933.
```

You can then run the program again with the provided hash:
```
> cabal run -- http-get-json-metadata https://git.io/Jt01O 54796c0c86228b9c86a1982e76441b293d01b7a9cd1309d43ae95123a8ad8933
Up to date
Success
{"description":"Guaranteed lifetime 1% staking pool. Proceeds go toward crypto education.",
"homepage":"https://cardano.afterschoollabs.io","name":"After School Labs","ticker":"ASLAB"}
```

**Note:** Up until now (2022/09/30) the code to fetch the pool offchain metadata did not check the
"content-type" field in the response headers, but now it does. The rules for how this field is
handled is as follows:

* `application/json`: This is the expected value.
* `text/plain`: Currently accepted.
* `application/octet-stream`: Currently accepted.
* `binary/octet-stream`: Currently accepted.
* `application/binary`: Currently accepted.
* `text/html`: Currently accepted only if it might be JSON (ie first non-whitespace character of
      body is `{`). Some time after 2024/01/01 this will no longer be accepted.
     the future
* missing content-type header: Currently accepted, but some time after 2024/01/01 this will no
     longer be accepted.
* all others: Rejected.

Also note that extra text in the `comtemt-type` header fieled (eg `charset=utf-8`) is ignored.
