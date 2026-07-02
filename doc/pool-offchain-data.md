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

This is a testing/debugging program. It uses *the same code* to fetch the offchain metadata as
`cardano-db-sync`. It is not a code copy, this application calls the functions in `db-sync` that do
the actual fetch (`httpGetOffChainPoolData` and `httpGetOffChainVoteData` in
`Cardano.DbSync.OffChain.Http`).

On a successful network fetch (pool metadata, or governance metadata with the `url` keyword) this
program prints "Success" and then the retrieved JSON. On failure, the error message is printed.
Reading governance metadata from a local file uses a different, more verbose output - see the
governance section below.

It is included in the release tarball alongside `cardano-db-sync`, or it can be run from the
`cardano-db-sync` git checkout with `cabal run`.

#### Pool metadata

With no type keyword, the program fetches and parses pool metadata. The program can be run as:
```
> cabal run -- http-get-json-metadata <url>
```

If you also want to check the JSON metadata hash, the program can be run as:
```
> cabal run -- http-get-json-metadata <url> <hash>
```
where the hash is specified as a string of hexadecimal digits.

If you don't know the hash, you can provide an empty string for this field and the program will
print the expected hash in the error message. Eg:
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

#### Governance (vote) metadata

Adding a type keyword switches the program to governance metadata, parsed according to CIP-100,
CIP-108 and CIP-119. This is the way to check how `db-sync` treats a given anchor before it shows up
in the database. The type keyword selects the anchor type:

* `drep` - DRep registration metadata
* `ga` - governance action metadata
* `vote` - vote metadata
* `committee_dereg` - committee member deregistration metadata
* `const` - constitution metadata
* `other` - any other anchor

Add the `url` keyword to fetch over the network, together with one type keyword:
```
> cabal run -- http-get-json-metadata url drep <url>
> cabal run -- http-get-json-metadata url drep <url> <hash>
```

Without the `url` keyword, a type keyword reads from a local file instead of fetching. The path
takes the place of the URL:
```
> cabal run -- http-get-json-metadata drep ./drep-metadata.json
```
Local-file mode prints a lower-level debug dump rather than the `Success` line used by the network
fetch. In order, it prints: the parsed metadata as an internal record, the same file as a raw JSON
value, the file's computed blake2b-256 hash, any validation warning (`Nothing` if none), and finally
`Is valid JSON: True`. The `Is valid JSON` line is the pass/fail signal here, and the printed hash is
the anchor's hash - useful when you have the file but not the hash. If you pass an expected hash as a
trailing argument, a mismatch is reported in the warning slot.

#### `ipfs://` URLs

Many real governance anchors use an `ipfs://<CID>` URL rather than an `https://` one. The running
`db-sync` daemon resolves these through its `ipfs_gateway` config field (a list of gateway prefixes,
default `["https://ipfs.io/ipfs"]`), but this debug tool passes an *empty* gateway list, so it
cannot resolve `ipfs://` on its own. Fetching one directly fails with:
```
Error Offchain Voting Anchor: No ipfs_gateway provided in the db-sync config
```
To test an `ipfs://` anchor, rewrite it into a plain gateway URL yourself before passing it in -
replace the `ipfs://` scheme with a gateway prefix. The content (and therefore the hash) is the
same, so any hash check still applies:
```
ipfs://QmXdfDSSkR8TYBntMT4nuTL9sE9E44FJnpWVpaY8ZTfwjj
    ->  https://ipfs.io/ipfs/QmXdfDSSkR8TYBntMT4nuTL9sE9E44FJnpWVpaY8ZTfwjj
```
Note that public IPFS gateways are frequently rate-limited or briefly unavailable (HTTP 429 / 504 /
timeout). A failure of that kind is a gateway problem, not a sign that the anchor or its hash is
bad - retry, or try a different gateway prefix, before drawing a conclusion.

#### Content-type handling

The fetch checks the "content-type" field in the response headers. The rules are as follows:

* `application/json`: This is the expected value.
* `application/ld+json`: Accepted.
* `text/plain`: Accepted.
* `application/octet-stream`: Accepted.
* `binary/octet-stream`: Accepted.
* `application/binary`: Accepted.
* `text/html`: Accepted only if it might be JSON (ie first non-whitespace character of body is `{`).
* missing content-type header: Accepted.
* all others: Rejected.

Extra text in the `content-type` header field (eg `charset=utf-8`) is ignored.

### test-http-get-json-metadata

This is a bulk health check for pool metadata. It takes no arguments and uses `PGPASSFILE` to
connect to an existing `db-sync` database. It queries every stored pool metadata reference (skipping
retired pools), re-fetches each URL with the same code as above, and prints a summary of how many
fetches failed in each error category (hash mismatch, timeout, bad content-type, and so on). It
covers pool metadata only.
```
> PGPASSFILE=config/pgpass-mainnet cabal run -- test-http-get-json-metadata
```
