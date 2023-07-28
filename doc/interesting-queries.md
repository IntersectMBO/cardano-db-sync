# Interesting SQL queries

The following is a set of example SQL queries that can be run against the `db-sync` database.

These queries are run using the `psql` executable distributed with PostgreSQL. Connecting to the
database can be done from the `cardano-db-sync` git checkout using:
```sh
PGPASSFILE=config/pgpass-mainnet psql cexplorer
```

Some of these queries have Haskell/Esqueleto equivalents in the file [Query.hs][Query.hs] and where
they exist, the names of those queries will be included in parentheses.

### Chain meta data (`queryMeta`)
```sql
select * from meta ;
 id |     start_time      | network_name
----+---------------------+--------------
  1 | 2017-09-23 21:44:51 | mainnet
(1 row)

```

### Current total on-chain supply of Ada (`queryTotalSupply`)

Note: 1 ADA == 1,000,000 Lovelace

This just queries the UTxO set for unspent transaction outputs. It does not include staking rewards
that have have not yet been withdrawn. Before being withdrawn rewards exist in ledger state and not
on-chain.

```sql
select sum (value) / 1000000 as current_supply from tx_out as tx_outer where
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
```sql
select slot_no from block where block_no is not null
    order by block_no desc limit 1 ;
 slot_no
---------
 4011090
(1 row)

```

### Size of the cexplorer database
```sql
select pg_size_pretty (pg_database_size ('cexplorer'));
 pg_size_pretty
----------------
 116 GB
(1 row)
```

### Size of the cexplorer database table
```sql
select pg_size_pretty (pg_total_relation_size ('block'));
 pg_size_pretty
----------------
 2760 MB
(1 row)
```

### Sync progress of `db-sync`

To get a rough estimate of how close to fully synced the database is, we can use the time stamps on
the blocks as follows:

```sql
select
   100 * (extract (epoch from (max (time) at time zone 'UTC')) - extract (epoch from (min (time) at time zone 'UTC')))
      / (extract (epoch from (now () at time zone 'UTC')) - extract (epoch from (min (time) at time zone 'UTC')))
  as sync_percent
  from block ;
   sync_percent
------------------
 97.4357948804029
(1 row)
```
Note: The value returned by this query can be rather misleading as it operates on block time stamps
and early epochs contain much less data (eg Byron era did not have staking) and much fewer
transactions.

To find out how far behind `db-sync` is:
```sql
select now () - max (time) as behind_by from block ;
       behind_by
------------------------
 4 days 20:59:39.134497
(1 row)
```

### Current valid pools
In general the database is operated on in an append only manner. Pool certificates can
be updated so that later certificates override earlier ones. In addition pools can
retire. Therefore to get the latest pool registration for every pool that is still
valid:
```sql
select * from pool_update
    where registered_tx_id in (select max(registered_tx_id) from pool_update group by hash_id)
    and not exists
      ( select * from pool_retire where pool_retire.hash_id = pool_update.hash_id
          and pool_retire.retiring_epoch <= (select max (epoch_no) from block)
      ) ;

```
To include the pool hash in the query output:
```sql
select * from pool_update inner join pool_hash on pool_update.hash_id = pool_hash.id
    where registered_tx_id in (select max(registered_tx_id) from pool_update group by hash_id)
    and not exists
      ( select * from pool_retire where pool_retire.hash_id = pool_update.hash_id
          and pool_retire.retiring_epoch <= (select max (epoch_no) from block)
          ) ;
```

### Get the stake address for a given Shelley address:
```sql
select stake_address.id as stake_address_id, tx_out.address, stake_address.view as stake_address
	from tx_out inner join stake_address on tx_out.stake_address_id = stake_address.id
	where address = 'addr1qx2kd28nq8ac5pr...08ly3tu9sy0f4qd' ;
 stake_address_id |         address                      |           stake_address
------------------+--------------------------------------+-------------------------------------
               42 | addr1qx2kd28nq8ac5p...8ly3tu9sy0f4qd | stake1u9ylzsgxaa6xct...nljg47zctvm3rc
```

### Transaction fee for specified transaction hash:
```sql
select tx.id, tx.fee from tx
    where tx.hash = '\xf9c0997afc8159dbe0568eadf0823112e0cc29cd097c8dc939ff44c372388bc0' ;
   id    |  fee
---------+--------
 1000000 | 172433

```

### Transaction outputs for specified transaction hash:
```sql
select tx_out.* from tx_out inner join tx on tx_out.tx_id = tx.id
    where tx.hash = '\xf9c0997afc8159dbe0568eadf0823112e0cc29cd097c8dc939ff44c372388bc0' ;
   id    |  tx_id  | index |         address         |    value     |        address_raw        | payment_cred
---------+---------+-------+-------------------------+--------------+---------------------------+--------------
 2205593 | 1000000 |     1 | DdzFFzCqrh...u6v9fWDrML | 149693067531 | \x82d8185842...1a20a42e6f |
 2205592 | 1000000 |     0 | DdzFFzCqrh...DoV2nEACWf |   8991998000 | \x82d8185842...1a150033dc |

```

### Transaction inputs for specified transaction hash:
```sql
select tx_out.* from tx_out
    inner join tx_in on tx_out.tx_id = tx_in.tx_out_id
    inner join tx on tx.id = tx_in.tx_in_id and tx_in.tx_out_index = tx_out.index
    where tx.hash = '\xf9c0997afc8159dbe0568eadf0823112e0cc29cd097c8dc939ff44c372388bc0' ;
   id    | tx_id  | index |        address          |    value     |        address_raw        | payment_cred
---------+--------+-------+-------------------------+--------------+---------------------------+--------------
 2195714 | 996126 |     4 | DdzFFzCqrh...dtq1FQQSCN | 158685237964 | \x82d8185842...1a330b42df |
```

### Get per epoch performance statistics (sync time, tx count, etc):
There are many ways this query can be written, but this is the one which so far has the best
performance (runs in a little over 10 minutes at epoch 270):
```sql
select epoch_no, max (sync_secs) as sync_secs, sum (tx_count) as tx_count, sum (sum_tx_size) as sum_tx_size,
    sum (reward_count) as reward_count, sum (stake_count) as stake_count
  from (
    select earned_epoch as epoch_no, 0 as sync_secs, 0 as tx_count, 0 as sum_tx_size, count (reward) as reward_count,
        0 as stake_count from reward group by earned_epoch
    union
    select epoch_no, 0 as sync_secs, 0 as tx_count, 0 as sum_tx_size, 0 as reward_count,
        count (epoch_stake) as stake_count from epoch_stake group by epoch_no
    union
    select epoch_no, 0 as sync_secs, count (tx) as tx_count, sum (tx.size) as tx_sum_size, 0 as reward_count,
        0 as stake_count
      from block inner join tx on tx.block_id = block.id
      where epoch_no is not null
      group by epoch_no
    union
    select no as epoch_no, seconds, 0 as tx_count, 0 as tx_sum_size, 0 as reward_count,
        0 as stake_count
      from epoch_sync_time
  )
  as derived_table group by epoch_no ;

 epoch_no |   sync_secs    | tx_count | sum_tx_size | reward_count | stake_count
----------+----------------+----------+-------------+--------------+-------------
        0 |              0 |       33 |        6093 |            0 |           0
        1 |   28.256384637 |    12870 |     2256995 |            0 |           0
        2 |   19.462634986 |     4292 |      830307 |            0 |           0
        3 |   18.302536512 |     3293 |      658490 |            0 |           0
...
      209 |  177.122253524 |    36916 |    19098427 |            0 |           0
      210 |  188.630659101 |    36267 |    19694637 |            0 |       17305
      211 |  160.841826393 |    29083 |    16330473 |        17988 |       24252
      212 |  146.277991679 |    24691 |    13503603 |        24421 |       30628
...
      268 | 3491.985000071 |   208164 |   127281166 |       527806 |      557805
      269 | 3234.034316171 |   197254 |   118772706 |            0 |      577352
      270 |              0 |   120754 |    70076543 |            0 |      595592
(271 rows)
```

### Transaction withdrawals for specified transaction hash:
Withdrawals are a feature of some transactions of the Shelley era and later.

```sql
select withdrawal.* from withdrawal
    inner join tx on withdrawal.tx_id = tx.id
    where tx.hash = '\x0b8c5be678209bb051a02904dd18896a929f9aca8aecd48850939a590175f7e8' ;
  id   | addr_id |  amount   |  tx_id
-------+---------+-----------+---------
 27684 |   30399 | 154619825 | 2788211
```

### Get the stake distribution for each pool for a given epoch:
Simplest query is:
```sql
select pool_id, sum (amount) from epoch_stake
    where epoch_no = 216 group by pool_id ;

 pool_id |       sum
---------+-----------------
       1 |  25326935163066
       2 | 112825285842751
...
    1172 |       498620686
    1173 |     15024987189
(1114 rows)
```
Or, to use the Bech32 pool identifier instead of the Postgres generated `pool_id` field:
```sql
select pool_hash.view, sum (amount) as lovelace from epoch_stake
    inner join pool_hash on epoch_stake.pool_id = pool_hash.id
    where epoch_no = 216 group by pool_hash.id ;
                           view                           |    lovelace
----------------------------------------------------------+-----------------
 pool10p6wd9k0fwk2zqkqnqr8efyr7gms627ujk9dxgk6majskhawr6r |    789466838780
 pool1vvh72z8dktfy2x965w0yp5psmnyv3845pmm37nerhl6jk6m2njw |   1427697660218
...
 pool1tq03j8aa5myedlr8xj6tltstdsn5eprxq4cd4qr54mhv25unsyk |     50297430457
 pool1nux6acnlx0du7ss9fhg2phjlaqe87l4wcurln5r6f0k8xreluez |   5401615743207
(1114 rows)

```

### Get the delegation history for a specified stake address
```sql
select delegation.active_epoch_no, pool_hash.view from delegation
    inner join stake_address on delegation.addr_id = stake_address.id
    inner join pool_hash on delegation.pool_hash_id = pool_hash.id
    where stake_address.view = 'stake1u8gsndukzghdukmqdsd7r7wd6kvamvjv2pzcgag8v6jd69qfqyl5h'
    order by active_epoch_no asc;
 active_epoch_no |                           view
-----------------+----------------------------------------------------------
             212 | pool1hwlghkwnjsjk8370qt3dvp23d7urwm36f95fmxcz3np2kghknj9
             214 | pool1xxhs2zw5xa4g54d5p62j46nlqzwp8jklqvuv2agjlapwjx9qkg9
             216 | pool1hwlghkwnjsjk8370qt3dvp23d7urwm36f95fmxcz3np2kghknj9
             217 | pool15yyxtkhz64p7a8cnax9l7u82s9t9hdhyxsa3tdm977qhgpnsuhq
(4 rows)
```
This shows that the first delegation became active (ie earning rewards) in epoch 212 and that the
address owner subsequently re-delegated their stake with those re-delegations becoming active in
epochs 214, 216 and 217.


### Get the reward history for a specified stake address

```sql
select reward.earned_epoch, pool_hash.view as delegated_pool, reward.amount as lovelace
    from reward inner join stake_address on reward.addr_id = stake_address.id
    inner join pool_hash on reward.pool_id = pool_hash.id
    where stake_address.view = 'stake1u8gsndukzghdukmqdsd7r7wd6kvamvjv2pzcgag8v6jd69qfqyl5h'
    order by earned_epoch asc ;
 epoch_no |                      delegated_pool                      | lovelace
----------+----------------------------------------------------------+----------
      212 | pool1hwlghkwnjsjk8370qt3dvp23d7urwm36f95fmxcz3np2kghknj9 |  2953284
      213 | pool1hwlghkwnjsjk8370qt3dvp23d7urwm36f95fmxcz3np2kghknj9 |  3333940
      214 | pool1xxhs2zw5xa4g54d5p62j46nlqzwp8jklqvuv2agjlapwjx9qkg9 |  3005843
      215 | pool1xxhs2zw5xa4g54d5p62j46nlqzwp8jklqvuv2agjlapwjx9qkg9 |  3552293
      216 | pool1hwlghkwnjsjk8370qt3dvp23d7urwm36f95fmxcz3np2kghknj9 |  3130673
      217 | pool15yyxtkhz64p7a8cnax9l7u82s9t9hdhyxsa3tdm977qhgpnsuhq | 34339994
      218 | pool15yyxtkhz64p7a8cnax9l7u82s9t9hdhyxsa3tdm977qhgpnsuhq | 30384189
      219 | pool15yyxtkhz64p7a8cnax9l7u82s9t9hdhyxsa3tdm977qhgpnsuhq | 27293239
      220 | pool15yyxtkhz64p7a8cnax9l7u82s9t9hdhyxsa3tdm977qhgpnsuhq | 36947267
      221 | pool15yyxtkhz64p7a8cnax9l7u82s9t9hdhyxsa3tdm977qhgpnsuhq | 27016952
(10 rows)
```

### Get the block number of blocks created in an epoch by a specified pool

```sql
select block.block_no, block.epoch_no, pool_hash.view as pool_view
    from block inner join slot_leader on block.slot_leader_id = slot_leader.id
    inner join pool_hash on slot_leader.pool_hash_id = pool_hash.id
    where block.epoch_no = 220
    and pool_hash.view = 'pool137x32lrkprphrd0aa8x4jqz98z6lc0wawlc88hdjeps4qe408ad' ;
 block_no | epoch_no |                        pool_view
----------+----------+----------------------------------------------------------
  4760198 |      220 | pool137x32lrkprphrd0aa8x4jqz98z6lc0wawlc88hdjeps4qe408ad
  4759847 |      220 | pool137x32lrkprphrd0aa8x4jqz98z6lc0wawlc88hdjeps4qe408ad
(2 rows)
```

### Get the number of blocks created by a specified pool for each epoch

```sql
select block.epoch_no, count (*) as block_count
    from block inner join slot_leader on block.slot_leader_id = slot_leader.id
    inner join pool_hash on slot_leader.pool_hash_id = pool_hash.id
    where pool_hash.view = 'pool1nux6acnlx0du7ss9fhg2phjlaqe87l4wcurln5r6f0k8xreluez'
    group by block.epoch_no, pool_hash.view ;
 epoch_no | block_count
----------+-------------
      212 |           1
      213 |           2
      214 |           2
      215 |           4
....
```

### Get the tx_id, tx_block_id, tx_out_address of a voting registration.
```sql
select tx.id as tx_id, tx.block_id as tx_block_id, tx_out.address as tx_out_address
    from tx inner join tx_out on tx.id = tx_out.tx_id
    where tx.hash = '\x9053a4cf0c6c9fb29792c78e688c5915a02909d0073371d8fff1abba0bed3065';
  tx_id  | tx_block_id |                       tx_out_address
---------+-------------+------------------------------------------------------------
 3192730 |     5083822 | addr1vy6d0htdaa9k8du2262p2ju74s25g6rjyjsc9x2fky9r6jq402r08
....
```

to find your transaction hash of your voter registration:
```sh
cardano-cli transaction txid --tx-file metadata.txsigned
9053a4cf0c6c9fb29792c78e688c5915a02909d0073371d8fff1abba0bed3065
```

### Get the amount delegated by epoch for a specified address
```sql
select stake_address.view as stake_address, epoch_stake.epoch_no, epoch_stake.amount
    from stake_address inner join epoch_stake on stake_address.id = epoch_stake.addr_id
    where stake_address.view = 'stake1u8mt5gqclkq0swmvzx9lvq4jgwsnx9yh030yrxwqwllu0mq2m0l4n' ;
                        stake_address                        | epoch_no |   amount
-------------------------------------------------------------+----------+-------------
 stake1u8mt5gqclkq0swmvzx9lvq4jgwsnx9yh030yrxwqwllu0mq2m0l4n |      211 |  1561003730
 stake1u8mt5gqclkq0swmvzx9lvq4jgwsnx9yh030yrxwqwllu0mq2m0l4n |      212 |  1561003730
...
```

### Get all reward account deposits
Reward payments can come from two places; staking rewards which are in the `reward` table and payments
from the treasury. These can be coalesced into single query via an SQL `union` operations:
```sql
select addr_id, amount, NULL as reward_epoch_no, tx_id as treasury_tx_id from treasury
    union
    select addr_id, amount, earned_epoch as reward_epoch_no, NULL as treasury_tx_id from reward ;
 addr_id |    amount     | reward_epoch_no | treasury_tx_id
---------+---------------+-----------------+----------------
       3 |       1071786 |             216 |
       3 |       1148596 |             212 |
       3 |       1363094 |             215 |
       3 |       1527849 |             214 |
       3 |       2336874 |                 |        3557307
       3 |       2439495 |             213 |
       3 |       3204796 |             241 |
       3 |       3518828 |             221 |
...
```

### Get historical UTxO set for a given timestamp
The UTxO set is dependent on time, this will return it for a given timestamp
```sql
# with const as (select to_timestamp ('2020-10-10 17:00:00', 'YYYY-MM-DD HH24:MI:SS') as effective_time_)
  select tx_out.address as address, tx_out.value as lovelace, generating_block.time as timestamp
    from const
    cross join tx_out
    inner join tx as generating_tx on generating_tx.id = tx_out.tx_id
    inner join block as generating_block on generating_block.id = generating_tx.block_id
    left join tx_in as consuming_input on consuming_input.tx_out_id = generating_tx.id
      and consuming_input.tx_out_index = tx_out.index
    left join tx as consuming_tx on consuming_tx.id = consuming_input.tx_in_id
    left join block as consuming_block on consuming_block.id = consuming_tx.block_id
    where ( -- Ommit outputs from genesis after Allegra hard fork
			const.effective_time_ < '2020-12-16 21:44:00'
			or generating_block.epoch_no is not null
			)
      and const.effective_time_ >= generating_block.time -- Only outputs from blocks generated in the past
      and ( -- Only outputs consumed in the future or unconsumed outputs
		const.effective_time_ <= consuming_block.time or consuming_input.id IS NULL
		) ;

                           address                           |   lovelace    |      timestamp
-------------------------------------------------------------+---------------+---------------------
 Ae2tdPwUPEZFdcW8MaYNxoJJkKmkSwJD5D4AdJPBLLn7PCVMenKMvwtWV8K |       1000000 | 2017-09-23 21:44:51
 Ae2tdPwUPEZ1pRs1gSidtoRGMpJR54UyNrdVDMFxXu2pBkhxitAWhqrGqd9 |  715399000000 | 2017-09-23 21:44:51
 Ae2tdPwUPEZJn5QH1xKqNFmtEfvBXFHJ5RacD1xtR9kcvndvebirJHG7Sam |  380723000000 | 2017-09-23 21:44:51
 Ae2tdPwUPEZBZRPzrsCvbaG3HH89AUtAkBwjeR4pC2WZvXYZ4Ab7J7JNpLz |  469809000000 | 2017-09-23 21:44:51
 Ae2tdPwUPEYyvoxws84ogrBf7CqRjiGaqP6yKeb2gsLZaKPaDWKdxiQn1Pm | 1838066000000 | 2017-09-23 21:44:51
...
```

### Get tagged Genesis addresses
The genesis block contains multiple addresses, this query will tag them with their origin.
```sql
select
    genesis_output.address as address,
    floor (genesis_output.value / 1000000) as ada,
    redemption_block.time as redeemed_at,
    cast ((case
			  when genesis_output.value = 1000000 then 'Test Ada'
		      when genesis_output.address in (
				'Ae2tdPwUPEZKQuZh2UndEoTKEakMYHGNjJVYmNZgJk2qqgHouxDsA5oT83n',
				'Ae2tdPwUPEZGcVv9qJ3KSTx5wk3dHKNn6G3a3eshzqX2y3N9LzL3ZTBEApq',
				'Ae2tdPwUPEZ9dH9VC4iVXZRNYe5HGc73AKVMYHExpgYBmDMkgCUgnJGqqqq'
				) then 'Development Pool'
			  else 'Pre-Sale'
			  end) as varchar) as origin
    from block as genesis_block
    inner join tx as genesis_tx on genesis_tx.block_id = genesis_block.id
    inner join tx_out as genesis_output on genesis_output.tx_id = genesis_tx.id
    left join tx_in as redemption_input on redemption_input.tx_out_id = genesis_tx.id
    left join tx as redemption_tx on redemption_tx.id = redemption_input.tx_in_id
    left join block as redemption_block on redemption_block.id = redemption_tx.block_id
    where genesis_block.epoch_no is null ;

                           address                           |    ada     |     redeemed_at     |      origin
-------------------------------------------------------------+------------+---------------------+------------------
 Ae2tdPwUPEZJkVfTW9cFmxAxsp1WtgV4hde53p5eLccUUFzQu8amyrLHcTL |     385509 | 2017-11-19 06:36:11 | Pre-Sale
 Ae2tdPwUPEZM65iaQ1jYtgmtTSvjrcPXfXcFBineY27wYot6t2ToY8FqkXu |    1923076 | 2017-09-30 16:40:31 | Pre-Sale
 Ae2tdPwUPEZMeLeeSL9Lq2Kr3UhBt1a23KhCMqFWvjYGXxGjkvV9giSQAeH |     832476 | 2017-09-29 07:47:31 | Pre-Sale
 Ae2tdPwUPEYwQuL8cXMVstbEUvfdxwWpjepjKTD9BYQbcXJG8BdLjEpuD8Y |     409116 | 2017-10-08 06:27:31 | Pre-Sale
 Ae2tdPwUPEZApyHh2AfjfSyCRYhwDLoWo91WvmjE9fQKo6VB9BjWk6N7eXv |     384615 | 2017-09-30 03:58:51 | Pre-Sale
...
```

### Get Ada in UTxO locked by scripts
```sql
select sum (value) / 1000000 as script_locked from tx_out as tx_outer where
    tx_outer.address_has_script = true and
    not exists
      ( select tx_out.id from tx_out inner join tx_in
          on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
          where tx_outer.id = tx_out.id
      ) ;
    script_locked
----------------------
 3695300.068246000000
(1 row)

```

### Get information about script tx's
```sql
select tx.id as tx_id, tx.fee as fees, SUM(redeemer.fee) as script_fees, SUM(redeemer.unit_mem) as unit_mem,
       SUM (redeemer.unit_steps) as unit_steps, tx.valid_contract as valid, count(redeemer.id) scripts, tx.script_size
       from tx join redeemer on tx.id = redeemer.tx_id group by tx.id;
 tx_id | fees     |script_fees |unit_mem  |unit_steps | valid|scripts|script_size
 ------+----------+------------+----------+-----------+------+-------+-----------
 11812 |200193089 |  200000000 | 100000000|  100000000| t    |      1|         92
 11909 |  5000000 |    4000000 |   2000000|    2000000| f    |      1|        565
 11931 |  5000000 |    4000000 |   2000000|    2000000| f    |      1|         92
 11983 |255089500 |  204089500 |     39500|  204050000| t    |      1|         92
 11987 |102635800 |   81635800 |     15800|   81620000| t    |      1|         92
 11992 |102635800 |   81635800 |     15800|   81620000| t    |      1|         92
 12004 |822865300 |  791865300 |    195300|  791670000| t    |      1|       4559
 12010 |822865300 |  791865300 |    195300|  791670000| t    |      1|       4559

```

### Get the sum of collatereal input lost
```sql
select SUM(value)/1000000 as lost_amount
  from tx
  join tx_in on tx.id = tx_in.tx_in_id
  join tx_out on tx_in.tx_out_id = tx_out.id
    where tx.valid_contract = false ;
  lost_amount
--------------------
 592003625.28471400

```

### Get all uses of a spend script and how much ada it unlocked from an output
```sql
select tx.id as tx_id, tx_out.value as tx_out_value, redeemer.unit_mem, redeemer.unit_steps, redeemer.fee, redeemer.purpose
  from tx join redeemer on redeemer.tx_id = tx.id
  join tx_in on tx_in.redeemer_id = redeemer.id
  join tx_out on tx_in.tx_out_id = tx_out.tx_id and tx_in.tx_out_index = tx_out.index
    where redeemer.script_hash = '\x8a08f851b22e5c54de087be307eeab3b5c8588a8cea8319867c786e0';
 tx_id | tx_out_value |  unit_mem   | unit_steps  |    fee     | purpose
-------+--------------+-------------+-------------+------------+---------
 10184 |    200000000 |    70000000 |    70000000 |  140000000 | spend
 11680 |   1000000000 |   700000000 |   700000000 | 1400000000 | spend
 11812 |    512000000 |   100000000 |   100000000 |  200000000 | spend
 11983 |    300000000 |       39500 |   204050000 |  204089500 | spend
 11987 |    200000000 |       15800 |    81620000 |   81635800 | spend
 11992 |    120000000 |       15800 |    81620000 |   81635800 | spend

```

### Get all mint scripts
```sql
select redeemer.tx_id as tx_id, redeemer.unit_mem, redeemer.unit_steps, redeemer.fee as redeemer_fee, redeemer.purpose, multi_asset.policy, multi_asset.name, ma_tx_mint.quantity
  from redeemer
    join multi_asset on redeemer.script_hash = multi_asset.policy
    join ma_tx_mint on ma_tx_mint.ident = multi_asset.id and redeemer.tx_id = ma_tx_mint.tx_id
      where purpose = 'mint';
 tx_id  | unit_mem | unit_steps | redeemer_fee | purpose |                           policy                           |           name           | quantity
--------+----------+------------+--------------+---------+------------------------------------------------------------+--------------------------+----------
 572051 |   994524 |  365737701 |        83754 | mint    | \xfda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50 | \x506c75747573436f696e   |      100
 572074 |   994524 |  365737701 |        83754 | mint    | \xfda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50 | \x506c75747573436f696e32 |      100

```

### Get all assets with quantities and count of mints
```sql
 SELECT
    multi_asset.fingerprint,
    multi_asset.policy,
    multi_asset.name,
    a.ident,
    a.cnt mints,
    a.quantity,
    block."time" AS created
   FROM ( SELECT ma_tx_mint.ident,
            sum(ma_tx_mint.quantity::numeric) AS quantity,
            count(*) AS cnt,
            min(ma_tx_mint.tx_id) AS mtx
           FROM ma_tx_mint
          GROUP BY ma_tx_mint.ident) a
     LEFT JOIN multi_asset ON multi_asset.id = a.ident
     LEFT JOIN tx ON tx.id = a.mtx
     LEFT JOIN block ON block.id = tx.block_id;
                      fingerprint                  |                           policy                           |       name       | ident | mints |    quantity    |       created
----------------------------------------------+------------------------------------------------------------+------------------+-------+-------+----------------+---------------------
 asset1jtqefvdycrenq2ly6ct8rwcu5e58va432vj586 | \x476039a0949cf0b22f6a800f56780184c44533887ca6e821007840c3 | \x6e7574636f696e |     1 |     1 |              1 | 2021-02-03 20:20:46
 asset1mu7h997yyvzrppdwjzhpex6u7khrucspxvjjuw | \x69b30e43bc5401bb34d0b12bd06cd9b537f33065aa49df7e8652739d | \x4c51           |     2 |     1 | 21000000000000 | 2021-02-03 20:22:23

```

### Get blocks with 0 transactions and the pools that forged it
```sql
select distinct on(block.hash) block.hash as block_hash , epoch_no, tx_count, pool_hash.hash_raw as pool_hash,
                               pool_update.pledge, pool_update.active_epoch_no, pool_metadata_ref.url, pool_offline_data.ticker_name
  from block join slot_leader on block.slot_leader_id = slot_leader.id
    join pool_hash on slot_leader.pool_hash_id = pool_hash.id
    join pool_update on pool_update.hash_id = pool_hash.id
    left join pool_metadata_ref on pool_update.meta_id = pool_metadata_ref.id
    left join pool_offline_data on pool_offline_data.pmr_id = pool_metadata_ref.id
  where tx_count = 0 and epoch_no > 150
  order by block.hash, pool_update.active_epoch_no desc;


                             block_hash                             | epoch_no | tx_count |                         pool_hash                          |     pledge      | active_epoch_no |                               url                                | ticker_name
--------------------------------------------------------------------+----------+----------+------------------------------------------------------------+-----------------+-----------------+------------------------------------------------------------------+-------------
 \x0000f4b44d1484d7280f087c1df94f068a02e23570e8ed9eb5c0dd980d4c46c1 |      165 |        0 | \xe402f5894b8a7073f198bb0710d6294f2ac354ede2577b5ce15159a4 |     50000000000 |             137 | https://www.canadastakes.ca/metadata/can1-testnet-metadata.json  |
 \x0001715520d185accd550c7b6c0811a2589e778e6408ffcd75b0f83f4f45c2c0 |      153 |        0 | \xacfd479740bde8289885694e69e3494f7dcbcfdca540aead48c5d653 |  64000000000000 |             100 |                                                                  |
 \x00044646beb7d24dce8cd74408c76b9816453a451fc030c42cc9961be55932d9 |      163 |        0 | \x1e2191487bed3de4bf440d5ff80bdae31d7f22798d3a93444302acb3 |  64000000000000 |             100 |                                                                  |
 \x000732a2f62e289930c2779559a975566395dcf0cf32130f3852a6e031d13d61 |      154 |        0 | \xd9dc497e633c2f1c665467e1ed7a93e4f8541bfbabe1cc31818fb20f |   1000000000000 |             131 | https://www.uniquestaking.com/pool/uniq9/1ecf39ad-4f3c-4043      |
```
---

### Query all delegators and some basic info
```sql
 SELECT sa.view AS address,
    nh.view AS delegated_now_poolid,
    d.active_epoch_no delegated_now_epoch,
    oh.view AS delegated_before_poolid,
    od.active_epoch_no AS delegated_before_epoch,
    d.addr_id,
    br."time" AS stake_key_registered
   FROM ( SELECT max(delegation.id) AS id
           FROM delegation
          GROUP BY delegation.addr_id) a
     LEFT JOIN delegation d ON d.id = a.id
     LEFT JOIN tx txn ON txn.id = d.tx_id
     LEFT JOIN block bn ON bn.id = txn.block_id
     LEFT JOIN stake_address sa ON sa.id = d.addr_id
     LEFT JOIN stake_deregistration de ON de.addr_id = d.addr_id AND de.id = (( SELECT max(stake_deregistration.id) AS max
           FROM stake_deregistration
          WHERE stake_deregistration.addr_id = d.addr_id))
     LEFT JOIN stake_registration re ON re.addr_id = d.addr_id AND re.id = (( SELECT max(stake_registration.id) AS max
           FROM stake_registration
          WHERE stake_registration.addr_id = d.addr_id))
     LEFT JOIN delegation od ON od.addr_id = d.addr_id AND od.id = (( SELECT max(delegation.id) AS max
           FROM delegation
          WHERE delegation.addr_id = d.addr_id AND NOT delegation.pool_hash_id = d.pool_hash_id))
     LEFT JOIN tx txo ON txo.id = od.tx_id
     LEFT JOIN block bo ON bo.id = txo.block_id
     LEFT JOIN pool_hash nh ON nh.id = d.pool_hash_id
     LEFT JOIN pool_hash oh ON oh.id = od.pool_hash_id
     LEFT JOIN tx txr ON txr.id = re.tx_id
     LEFT JOIN block br ON br.id = txr.block_id
  WHERE (de.tx_id < re.tx_id OR de.* IS NULL);


                           address                           |                   delegated_now_poolid                   | delegated_now_epoch |                 delegated_before_poolid                  | delegated_before_epoch | addr_id | stake_key_registered
-------------------------------------------------------------+----------------------------------------------------------+---------------------+----------------------------------------------------------+------------------------+---------+----------------------
 stake1u9ylzsgxaa6xctf4juup682ar3juj85n8tx3hthnljg47zctvm3rc | pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy |                 210 |                                                          |                        |       1 | 2020-07-29 22:41:31
 stake1u8mt5gqclkq0swmvzx9lvq4jgwsnx9yh030yrxwqwllu0mq2m0l4n | pool1qqqqzyqf8mlm70883zht60n4q6uqxg4a8x266sewv8ad2grkztl |                 317 | pool1qqqz9vlskay2gv3ec5pyck8c2tq9ty7dpfm60x8shvapguhcemt |                    211 |       3 | 2020-08-06 20:00:11
```

### Find all incorrect entries in `epoch` table

In [issue #1457](https://github.com/input-output-hk/cardano-db-sync/issues/1457) it was pointed out
that some entries in the epoch table were incorrect. These incorrect entries can be found using:
```
select b.epoch_no, e.epoch_blk_count, e.epoch_tx_count, b.block_block_count, b.block_tx_count
  from
    (select no, blk_count as epoch_blk_count, tx_count as epoch_tx_count from epoch) as e,
    (select epoch_no, count (block_no) as block_block_count, sum (tx_count) as block_tx_count
      from block group by epoch_no) as b
    where e.no = b.epoch_no
      and (e.epoch_blk_count != b.block_block_count or e.epoch_tx_count != b.block_tx_count)
    order by b.epoch_no ;
```
There is a shell script in the above issue which finds all incorrect rows in that table and fixes
them.

---


[Query.hs]: https://github.com/input-output-hk/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Query.hs
