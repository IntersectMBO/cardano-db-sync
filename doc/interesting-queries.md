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
 id | protocol_const | slot_duration |     start_time      | network_name
----+----------------+---------------+---------------------+--------------
  1 |           2160 |         20000 | 2017-09-23 21:44:51 | mainnet
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

### Estimate current tip slot number based on chain metadata

Note: The slot duration is in milliseconds.

```
cexplorer=# select extract (epoch from (select now() - start_time from meta)) * 1000 /
              slot_duration as est_tip_slot_no from meta ;
 est_tip_slot_no
-----------------
 4011091.0228046
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

[Query.hs]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Query.hs
