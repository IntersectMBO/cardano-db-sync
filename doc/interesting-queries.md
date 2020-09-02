# Interesting SQL queries

The following is a set of example SQL queries that can be run against the `db-sync` database.

These queries are run using the `psql` executable distributed with PostgreSQL. Connecting to the
database can be done from the `cardano-db-sync` git checkout using:
```
PGPASSFILE=config/pgpass psql cexplorer
```

Some of these queries have Haskell/Esqueleto equivalents in the file [Query.hs][Query.hs] and where
they exist, the names of those queries will be included in parentheses.

### Chain meta data (`queryMeta`)
```
cexplorer=# select * from meta ;
 id |     start_time      | network_name
----+---------------------+--------------
  1 | 2017-09-23 21:44:51 | mainnet
(1 row)

```

### Current total supply of Ada (`queryTotalSupply`)

Note: 1 ADA == 1,000,000 Lovelace

This just queries the UTxO set for unspent transaction outputs.

Currently (as of 2020/04/10) this should be the initial genesis supply minus transaction fees so far.
```
cexplorer=# select sum (value) / 1000000 as current_supply from tx_out as tx_outer where
              not exists
                ( select tx_out.id from tx_out inner join tx_in
                    on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
                    where tx_outer.id = tx_out.id
                  ) ;
    current_supply
----------------------
 31112120630.27526800

```

### Slot number of the most recent block (`queryLatestSlotNo`)
```
cexplorer=# select slot_no from block where block_no is not null
              order by block_no desc limit 1 ;
 slot_no
---------
 4011090
(1 row)

```

### Size of the cexplorer database
```
cexplorer=# select pg_size_pretty (pg_database_size ('cexplorer'));
 pg_size_pretty
----------------
 4067 MB
(1 row)
```

### Current valid pools
In general the database is operated on in an append only manner. Pool certificates can
be updated so that later certificates override earlier ones. In addition pools can
retire. Therefore to get the latest pool registration for every pool that is still
valid:
```
cexplorer=# select * from pool_update
              where registered_tx_id in (select max(registered_tx_id) from pool_update group by reward_addr_id)
              and not exists (select * from pool_retire where pool_retire.hash_id = pool_update.id);

```
To include the pool hash in the query output:
```
cexplorer=# select * from pool_update inner join pool_hash on pool_update.hash_id = pool_hash.id
              where registered_tx_id in (select max(registered_tx_id) from pool_update group by reward_addr_id)
              and not exists (select * from pool_retire where pool_retire.hash_id = pool_update.id);
```

### Transaction fee for specified transaction hash:
```
cexplorer=# select tx.id, tx.fee from tx
              where tx.hash = '\xf9c0997afc8159dbe0568eadf0823112e0cc29cd097c8dc939ff44c372388bc0' ;
   id    |  fee
---------+--------
 1000000 | 172433

```

### Transaction outputs for specified transaction hash:
```
cexplorer=# select tx_out.* from tx_out inner join tx on tx_out.tx_id = tx.id
              where tx.hash = '\xf9c0997afc8159dbe0568eadf0823112e0cc29cd097c8dc939ff44c372388bc0' ;
   id    |  tx_id  | index |         address         |    value     |        address_raw        | payment_cred
---------+---------+-------+-------------------------+--------------+---------------------------+--------------
 2205593 | 1000000 |     1 | DdzFFzCqrh...u6v9fWDrML | 149693067531 | \x82d8185842...1a20a42e6f |
 2205592 | 1000000 |     0 | DdzFFzCqrh...DoV2nEACWf |   8991998000 | \x82d8185842...1a150033dc |

```

### Transaction inputs for specified transaction hash:
```
cexplorer=# select tx_out.* from tx_out
              inner join tx_in on tx_out.tx_id = tx_in.tx_out_id
              inner join tx on tx.id = tx_in.tx_in_id and tx_in.tx_out_index = tx_out.index
              where tx.hash = '\xf9c0997afc8159dbe0568eadf0823112e0cc29cd097c8dc939ff44c372388bc0' ;
   id    | tx_id  | index |        address          |    value     |        address_raw        | payment_cred
---------+--------+-------+-------------------------+--------------+---------------------------+--------------
 2195714 | 996126 |     4 | DdzFFzCqrh...dtq1FQQSCN | 158685237964 | \x82d8185842...1a330b42df |
```

### Transaction withdrawals for specified transaction hash:
Withdrawals are a feature of some transactions of the Shelley era and later.

```
cexplorer=# select withdrawal.* from withdrawal
              inner join tx on withdrawal.tx_id = tx.id
              where tx.hash = '\x0b8c5be678209bb051a02904dd18896a929f9aca8aecd48850939a590175f7e8' ;
  id   | addr_id |  amount   |  tx_id
-------+---------+-----------+---------
 27684 |   30399 | 154619825 | 2788211
```





[Query.hs]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Query.hs
