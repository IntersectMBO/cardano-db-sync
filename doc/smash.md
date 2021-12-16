# Stakepool Metadata Aggregation Server (SMASH)

## Overview

Cardano Shelley introduces the concept of stake pools - reliable server nodes that keep an aggregated stake of pool operators and delegators in a single entity. Stake pools are registered on-chain, and their on-chain data (such as information required to calculate rewards) is critical to the operation of the ledger. Stake pools also possess metadata that helps users to make a rational choice of a stake pool to delegate to. This metadata is stored off-chain as it might reflect sensitive content, and such an approach allows for a degree of decentralized censorship. 

On the other hand, off-chain metadata storage prerequisites a challenge of open access by different users. On-chain stake pool registrations contain an URL pointer to the off-chain metadata and a content hash that can be fetched from a specific stake pool. This might cause both performance and privacy issues. Another crucial aspect to address is the stake pool’s “ticker” name, which is the short name a stake pool is recognized by. Ticker names might reflect prominent brands or trademarks which should not be duplicated as this leads to confusion. Stake pool operators (SPOs) running multiple pools might want to use the same metadata for all their pools and then, this might also lead to these pools appearing with the same ticker name. 

## Use cases

A stake pool meta aggregation server (SMASH) is introduced to address metadata performance and privacy issues. Delegators, stake pool operators, exchanges, or wallets can deploy and use SMASH to ensure a higher level of metadata accountability and maintenance. SMASH aggregates metadata from existing stake pools and provides an efficient way to fetch it and store it in a semi-centralized environment. SMASH operators (exchanges, wallets, SPOs) are then enabled to validate and manage this metadata curating it for censorship via the delisting feature.

The first generation of SMASH server has been deployed by Input Output Global (IOG). It aggregates stake pool metadata and offers a list of valid stake pools with reviewed content and ticker names. In the short-term perspective, the Daedalus wallet will allow the selection of the SMASH server. From a list of reviewed stake pools, it will be easier to make a rational choice of the pool to delegate to. IOG will be one of the first servers, and it is expected that more operators will be using SMASH for the same purpose.

Exchanges, for example, can use the same functionality to keep track of stake pool metadata. SMASH will allow an exchange to fetch stake pool metadata and verify its content against the on-chain registered hash. The exchange can then check existing metadata for correctness (size limits, content), create new stake pools manually, and reserve their ticker names. In case there is a stake pool with a duplicated ticker name, counterfeit or offensive content, it will be possible to delist this pool. 

## SMASH Characteristics**

There are two main parts of smash. The part that downloads off-chain metadata and the smash-server.
The first part is integrated in db-sync, while the second works as a low resources standalone
server.

### SMASH

- smash is robust against incorrectly configured or malicious metadata hosting (e.g. timeouts, resource limits);
- verifies metadata content against the on-chain registered hash;
- verifies the size is within the limits, and the content matches the necessary JSON scheme;
- serves requested metadata via an API to wallets and other users in a performant way, including for the case of incremental updates;

### SMASH-server

- smash-server relies on a db-sync database to serve queries;
- cardano-db-sync or cardano-db-sync-extended must run before or during cardano-smash-server, to populate the db tables from the chain as well as the off-chain pool metadata.
- it is not necessary to have a fully synced cardano-db for smash-server. But the queries in this case will return stale results;
- serves the requested metadata preserving the content hash to be verified against the hash from the on-chain registration certificate;
- indicates that metadata is not available in response to requests;
- allows operators to delist stake pools;
- allows operators to configure, check and adjust their chosen policy via an appropriate interface without service interruptions;
- follows typical behaviours, e.g. configured via CLI and/or configuration file, stdout/stderr logging.

## Prerequisites

SMASH relies on:

* The PostgreSQL library. This can be installed on Linux using e.g.  ``apt install libpq-dev``
* The PostgreSQL server. This can be installed on Linux using e.g. ``apt install postgresql``
* The ``cardano-db`` package. This acts as a library for database support, hence, it is not necessary to install it separately.
* The ``cardano-db-sync`` or ``cardano-db-sync-extended`` executable to fetch on-chain and off-chain data.

It is not necessary to install the Cardano wallet or any other Cardano components.

## Metadata

*The metadata that is stored by SMASH is restricted to contain no more than 512 bytes.*

Registered stake pools provide the following metadata:
* owner
* pool name
* pool ticker
* homepage
* pledge address
* short description

SMASH records and serves the following subset of information:
* pool name
* pool ticker
* homepage
* short description

More information about the pool metadata (the `PoolMetaData` record) can be found [here](https://github.com/input-output-hk/cardano-ledger-specs/blob/4458fdba7e2211f63e7f28ecd3f9b55b02eee071/shelley/chain-and-ledger/executable-spec/src/Shelley/Spec/Ledger/TxData.hs#L62) 

Stake pool metadata information can be also found in [The mainnet metadata Design Specification for Delegation and Incentives in Cardano](https://hydra.iohk.io/build/790053/download/1/delegation_design_spec.pdf) section 4.2 Stake Pool Metadata, p.30.


## Installation

### Build SMASH

SMASH can be built and installed from the command line using 

- "cabal" as follows:

```
cabal build cardano-smash-server
cabal install cardano-smash-server
```

- nix

```
nix-build -A cardano-smash-server -o smash-server
```

## How to run SMASH with the Cardano node and db-sync

First, run the Cardano node as a relay, or passive node e.g.:
```
cardano-node --genesis-file genesis.json --socket-path node.socket --config config.yaml
```
After that, run db-sync to populate the tables, e.g.:
```
PGPASSFILE=config/pgpass-mainnet db-sync-node/bin/cardano-db-sync \
    --config config/mainnet-config.yaml \
    --socket-path ../cardano-node/state-node-mainnet/node.socket \
    --state-dir ledger-state/mainnet \
    --schema-dir schema/
```

see https://github.com/input-output-hk/cardano-db-sync/blob/master/doc/building-running.md for more info on how
to run cardano-db-sync.

Finally you can then run ``smash-server`` using e.g:
```
PGPASSFILE=config/pgpass-mainnet cardano-smash-server \
     --config config/mainnet-config.yaml \
     --port 3100 \
     --admins admins.txt
```

Note that the PGPASSFILE files must match, so that the same database is used

## What does smash-server do?

db-sync synchronizes with the blockchain and fetches the pool metadata off the chain. Then, it stores that metadata in the database. smash-server allows people/wallets/clients to check these metadata.

From the HTTP API it supports the fetching of the metadata using:
```
curl --verbose --header "Content-Type: application/json" --request GET http://localhost:3100/api/v1/metadata/<POOL_ID>/<POOL_META_HASH>
```
Delisting can be done as follows (in case you are an administrator):
```
curl --verbose --header "Content-Type: application/json" --request PATCH --data '{"poolId":"<POOL_ID>"}' http://localhost:3100/api/v1/delist
```

You can reserve a ticker:
```
curl --verbose -u <USERNAME>:<PASSWORD> --header "Content-Type: application/json" --request POST --data '{"poolId":"<POOL_HASH>"}' http://localhost:3100/api/v1/tickers/<TICKER_NAME>
```

## How to test this?

Run commands provided in the example:
```
curl --verbose --header "Content-Type: application/json" --request GET http://localhost:3100/api/v1/metadata/5bb8126c7bacc9bab69957ed95abf46bcad346eb421b3688c61bb37f/4b2221a0ac0b0197308323080ba97e3e453f8625393d30f96eebe0fca4cb7335

curl --verbose -u <USERNAME>:<PASSWORD> --header "Content-Type: application/json" --request PATCH --data '{"poolId":"5bb8126c7bacc9bab69957ed95abf46bcad346eb421b3688c61bb37f"}' http://localhost:3100/api/v1/delist
```

## What else is important?

*YOU NEED TO SERVE IT BEHIND HTTPS*
Interacting with a server via a regular HTTP protocol is unsafe since it is using Basic Auth, which is not protected and is visible during such interaction. You need an HTTP server (Apache or Nginx e.g) that can be configured post installation to directly point to the application port.

## How to get the Swagger/OpenAPI info?

First, run the application. Then, go to the localhost and copy the content into an [editor](https://editor.swagger.io/).

## Delisting feature

If you find a pool hash that has been inserted, like in our example:
```
curl -X GET -v http://localhost:3100/api/v1/metadata/8517fa7042cb9494818861c53c87780b4975c0bd402e3ed85168aa66/4b2221a0ac0b0197308323080ba97e3e453f8625393d30f96eebe0fca4cb7335 | jq .
```

You can delist a pool by sending a PATCH on the delist endpoint.

```
curl -u <USERNAME>:<PASSWORD> -X PATCH -v http://localhost:3100/api/v1/delist -H 'content-type: application/json' -d '{"poolId": "8517fa7042cb9494818861c53c87780b4975c0bd402e3ed85168aa66"}'
```

Try fetching the pool:
```
curl -X GET -v http://localhost:3100/api/v1/metadata/8517fa7042cb9494818861c53c87780b4975c0bd402e3ed85168aa66/4b2221a0ac0b0197308323080ba97e3e453f8625393d30f96eebe0fca4cb7335 | jq .
```

If you have made a mistake by delisting the wrong pool id, you can whitelist it (the example with the Basic Auth):
```
curl -u <USERNAME>:<PASSWORD> -X PATCH -v http://localhost:3100/api/v1/enlist -H 'content-type: application/json' -d '{"poolId": "8517fa7042cb9494818861c53c87780b4975c0bd402e3ed85168aa66"}'
```

Try fetching the pool:
```
curl -X GET -v http://localhost:3100/api/v1/metadata/8517fa7042cb9494818861c53c87780b4975c0bd402e3ed85168aa66/4b2221a0ac0b0197308323080ba97e3e453f8625393d30f96eebe0fca4cb7335 | jq .
```

## Basic Auth and DB

You need to provide username and passwords with the --admins <FILE> option. The file should contain lines in the form of <username>,<password>. These credentials are used for the secure endpoints (pool delisting and enlisting, ticker reserving, policies fetching)


## How to insert the reserved ticker name?

smash server admins can reserve tickers to specific pool. This can be done with the following command:

```
curl --verbose --header "Content-Type: application/json" --request POST --data '{"poolHash":"2560993cf1b6f3f1ebde429f062ce48751ed6551c2629ce62e4e169f140a3524"}' http://localhost:3100/api/v1/tickers/SALAD
```

The ticker does not need to be currently used by the pool. It can be reserved for the future or to avoid using similar names (e.g. IOG vs I0G)

If another pool uses or starts using the same ticker name, its metadata will not be returned through the "metadata" endpoint, as if it is delisted.

## Checking the pool rejection errors

Currently there is a way to check if there are any errors while trying to download the pool metadata. It could be that the hash is wrong, that the server URL return 404, or something else.
This is a nice way to check what went wrong.

If you have a specific pool id you want to check, you can add that pool id (`c0b0e43213a8c898e373928fbfc3df81ee77c0df7dadc3ad6e5bae17`) in there:
```
http://localhost:3100/api/v1/errors/062693863e0bcf9f619238f020741381d4d3748aae6faf1c012e80e7
```

**This shows the last 10 errors from your pool id. There might be more then just 10 errors, but you have to filter the query using the time parameter**.
You can filter just the ones you want by using a date you want to filter from, like this (using DD.MM.YYYY):
```
http://localhost:3100/api/v1/errors/6b6164af70861c5537cc9c8e50fdae35139ca2c8c6fbb42e8b7e6bfb?fromDate=13.10.2020
```

The returned list consists of objects that contain:
- time - the time formatted in `DD.MM.YYYY. HH:MM:SS`
- utcTime - the time formatted in the standard UTCTime format for any clients
- poolId - the pool id of the pool
- poolHash - the hash of the pool metadata
- cause - what is the cause of the error and why is it failing
- retryCount - the number of times we retried to fetch the offline metadata

## Pool unregistrations

It is possible that a pool unregisters, in which case all it's metadata will be unavailable. You can check what pools have unregistered by:
```
curl --verbose --header "Content-Type: application/json" http://localhost:3100/api/v1/retired
```


