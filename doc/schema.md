# Schema Documentation for cardano-db-sync

Schema version: 11.0.3

### `schema_version`

The version of the database schema. Schema versioning is split into three stages as detailed below. This table should only ever have a single row.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `stage_one` | integer (64) | Set up PostgreSQL data types (using SQL 'DOMAIN' statements). |
| `stage_two` | integer (64) | Persistent generated migrations. |
| `stage_three` | integer (64) | Set up database views, indices etc. |

### `pool_hash`

A table for every unique pool key hash. The `id` field of this table is used as foreign keys in other tables.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_raw` | hash28type | The raw bytes of the pool hash. |
| `view` | string | The Bech32 encoding of the pool hash. |

### `slot_leader`

Every unique slot leader (ie an entity that mines a block).

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash28type | The hash of of the block producer identifier. |
| `pool_hash_id` | integer (64) | If the slot leader is a pool, an index into the `PoolHash` table. |
| `description` | string | An auto-generated description of the slot leader. |

### `block`

A table for blocks on the chain.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash32type | The hash identifier of the block. |
| `epoch_no` | uinteger | The epoch number. |
| `slot_no` | uinteger | The slot number. |
| `epoch_slot_no` | uinteger | The slot number within an epoch (resets to zero at the start of each epoch). |
| `block_no` | uinteger | The block number. |
| `previous_id` | integer (64) | The Block table index of the previous block. |
| `slot_leader_id` | integer (64) | The SlotLeader table index of the creator of this block. |
| `size` | uinteger | The block size (in bytes). |
| `time` | timestamp | The block time (UTCTime). |
| `tx_count` | integer (64) | The number of transactions in this block. |
| `proto_major` | uinteger | The block's major protocol number. |
| `proto_minor` | uinteger | The block's major protocol number. |
| `vrf_key` | string | The VRF key of the creator of this block. |
| `op_cert` | hash32type | The hash of the operational certificate of the block producer. |
| `op_cert_counter` | word63type | The value of the counter used to produce the operational certificate. |

### `tx`

A table for transactions within a block on the chain.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash32type | The hash identifier of the transaction. |
| `block_id` | integer (64) | The Block table index of the block that contains this transaction. |
| `block_index` | uinteger | The index of this transaction with the block (zero based). |
| `out_sum` | lovelace | The sum of the transaction outputs (in Lovelace). |
| `fee` | lovelace | The fees paid for this transaction. |
| `deposit` | integer (64) | Deposit (or deposit refund) in this transaction. Deposits are positive, refunds negative. |
| `size` | uinteger | The size of the transaction in bytes. |
| `invalid_before` | word64type | Transaction in invalid before this slot number. |
| `invalid_hereafter` | word64type | Transaction in invalid at or after this slot number. |
| `valid_contract` | boolean | False if the contract is invalid. True if the contract is valid or there is no contract. |
| `script_size` | uinteger | The sum of the script sizes (in bytes) of scripts in the transaction. |

### `stake_address`

A table of unique stake addresses. Can be an actual address or a script hash.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_raw` | addr29type | The raw bytes of the stake address hash. |
| `view` | string | The Bech32 encoded version of the stake address. |
| `script_hash` | hash28type | The script hash, in case this address is locked by a script. |
| `registered_tx_id` | integer (64) | The Tx table index of the transaction in which this address was registered. |

### `tx_out`

A table for transaction outputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the transaction that contains this transaction output. |
| `index` | txindex | The index of this transaction output with the transaction. |
| `address` | string | The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era. |
| `address_raw` | blob | The raw binary address. |
| `address_has_script` | boolean | Flag which shows if this address is locked by a script. |
| `payment_cred` | hash28type | The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash. |
| `stake_address_id` | integer (64) | The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses). |
| `value` | lovelace | The output value (in Lovelace) of the transaction output. |
| `data_hash` | hash32type | The hash of the transaction output datum. (NULL for Txs without scripts). |

### `tx_in`

A table for transaction inputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_in_id` | integer (64) | The Tx table index of the transaction that contains this transaction input |
| `tx_out_id` | integer (64) | The Tx table index of the transaction that contains this transaction output. |
| `tx_out_index` | txindex | The index within the transaction outputs. |
| `redeemer_id` | integer (64) | The Redeemer table index which is used to validate this input. |

### `collateral_tx_in`

A table for transaction collateral inputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_in_id` | integer (64) | The Tx table index of the transaction that contains this transaction input |
| `tx_out_id` | integer (64) | The Tx table index of the transaction that contains this transaction output. |
| `tx_out_index` | txindex | The index within the transaction outputs. |

### `meta`

A table containing metadata about the chain. There will probably only ever be one row in this table.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `start_time` | timestamp | The start time of the network. |
| `network_name` | string | The network name. |

### `epoch`

Aggregation of data within an epoch.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `out_sum` | word128type | The sum of the transaction output values (in Lovelace) in this epoch. |
| `fees` | lovelace | The sum of the fees (in Lovelace) in this epoch. |
| `tx_count` | uinteger | The number of transactions in this epoch. |
| `blk_count` | uinteger | The number of blocks in this epoch. |
| `no` | uinteger | The epoch number. |
| `start_time` | timestamp | The epoch start time. |
| `end_time` | timestamp | The epoch end time. |

### `ada_pots`

A table with all the different types of total balances (Shelley only).
The treasury and rewards fields will be correct for the whole epoch, but all other fields change block by block.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `slot_no` | uinteger | The slot number where this AdaPots snapshot was taken. |
| `epoch_no` | uinteger | The epoch number where this AdaPots snapshot was taken. |
| `treasury` | lovelace | The amount (in Lovelace) in the treasury pot. |
| `reserves` | lovelace | The amount (in Lovelace) in the reserves pot. |
| `rewards` | lovelace | The amount (in Lovelace) in the rewards pot. |
| `utxo` | lovelace | The amount (in Lovelace) in the UTxO set. |
| `deposits` | lovelace | The amount (in Lovelace) in the deposit pot. |
| `fees` | lovelace | The amount (in Lovelace) in the fee pot. |
| `block_id` | integer (64) | The Block table index of the block for which this snapshot was taken. |

### `pool_metadata_ref`

An on-chain reference to off-chain pool metadata.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `pool_id` | integer (64) | The PoolHash table index of the pool for this reference. |
| `url` | string | The URL for the location of the off-chain data. |
| `hash` | hash32type | The expected hash for the off-chain data. |
| `registered_tx_id` | integer (64) | The Tx table index of the transaction in which provided this metadata reference. |

### `pool_update`

An on-chain pool update.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_id` | integer (64) | The PoolHash table index of the pool this update refers to. |
| `cert_index` | integer (32) | The index of this pool update within the certificates of this transaction. |
| `vrf_key_hash` | hash32type | The hash of the pool's VRF key. |
| `pledge` | lovelace | The amount (in Lovelace) the pool owner pledges to the pool. |
| `reward_addr` | addr29type | The pool's rewards address. |
| `active_epoch_no` | integer (64) | The epoch number where this update becomes active. |
| `meta_id` | integer (64) | The PoolMetadataRef table index this pool update refers to. |
| `margin` | double | The margin (as a percentage) this pool charges. |
| `fixed_cost` | lovelace | The fixed per epoch fee (in ADA) this pool charges. |
| `registered_tx_id` | integer (64) | The Tx table index of the transaction in which provided this pool update. |

### `pool_owner`

A table containing pool owners.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the pool owner's stake address. |
| `pool_hash_id` | integer (64) | The PoolHash table index for the pool. |
| `registered_tx_id` | integer (64) | The Tx table index of the transaction where this pool owner was registered. |

### `pool_retire`

A table containing information about pools retiring.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_id` | integer (64) | The PoolHash table index of the pool this retirement refers to. |
| `cert_index` | integer (32) | The index of this pool retirement within the certificates of this transaction. |
| `announced_tx_id` | integer (64) | The Tx table index of the transaction where this pool retirement was announced. |
| `retiring_epoch` | uinteger | The epoch where this pool retires. |

### `pool_relay`



* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `update_id` | integer (64) | The PoolUpdate table index this PoolRelay entry refers to. |
| `ipv4` | string | The IPv4 address of the relay (NULLable). |
| `ipv6` | string | The IPv6 address of the relay (NULLable). |
| `dns_name` | string | The DNS name of the relay (NULLable). |
| `dns_srv_name` | string | The DNS service name of the relay (NULLable). |
| `port` | integer (32) | The port number of relay (NULLable). |

### `stake_registration`

A table containing stake address registrations.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address. |
| `cert_index` | integer (32) | The index of this stake registration within the certificates of this transaction. |
| `epoch_no` | uinteger | The epoch in which the registration took place. |
| `tx_id` | integer (64) | The Tx table index of the transaction where this stake address was registered. |

### `stake_deregistration`

A table containing stake address deregistrations.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address. |
| `cert_index` | integer (32) | The index of this stake deregistration within the certificates of this transaction. |
| `epoch_no` | uinteger | The epoch in which the deregistration took place. |
| `tx_id` | integer (64) | The Tx table index of the transaction where this stake address was deregistered. |
| `redeemer_id` | integer (64) | The Redeemer table index that is related with this certificate. |

### `delegation`

A table containing delegations from a stake address to a stake pool.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address. |
| `cert_index` | integer (32) | The index of this delegation within the certificates of this transaction. |
| `pool_hash_id` | integer (64) | The PoolHash table index for the pool being delegated to. |
| `active_epoch_no` | integer (64) | The epoch number where this delegation becomes active. |
| `tx_id` | integer (64) | The Tx table index of the transaction that contained this delegation. |
| `slot_no` | uinteger | The slot number of the block that contained this delegation. |
| `redeemer_id` | integer (64) | The Redeemer table index that is related with this certificate. |

### `tx_metadata`

A table for metadata attached to a transaction.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `key` | word64type | The metadata key (a Word64/unsigned 64 bit number). |
| `json` | jsonb | The JSON payload if it can be decoded as JSON. |
| `bytes` | bytea | The raw bytes of the payload. |
| `tx_id` | integer (64) | The Tx table index of the transaction where this metadata was included. |

### `reward`

A table for rewards earned by staking. The rewards earned in epoch `N` are added in this table when they are calculated during epoch `N + 1` but are not spendable until after the start of epoch `N + 2`.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address that earned the reward. |
| `type` | rewardtype | The source of the rewards; pool `member`, pool `owner`, `treasury` or `reserve` payment. |
| `amount` | lovelace | The reward amount (in Lovelace). |
| `earned_epoch` | integer (64) | The epoch in which the reward was earned. |
| `spendable_epoch` | integer (64) |  |
| `pool_id` | integer (64) | The PoolHash table index for the pool the stake address was delegated to when the reward is earned. Will be NULL for payments from the treasury or the reserves. |

### `withdrawal`

A table for withdrawals from a reward account.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address for which the withdrawal is for. |
| `amount` | lovelace | The withdrawal amount (in Lovelace). |
| `redeemer_id` | integer (64) | The Redeemer table index that is related with this withdrawal. |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this withdrawal. |

### `epoch_stake`

A table containing the epoch stake distribution for each epoch.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address for this EpochStake entry. |
| `pool_id` | integer (64) | The PoolHash table index for the pool this entry is delegated to. |
| `amount` | lovelace | The amount (in Lovelace) being staked. |
| `epoch_no` | integer (64) | The epoch number. |

### `treasury`

A table for payments from the treasury to a StakeAddress.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address for this Treasury entry. |
| `cert_index` | integer (32) | The index of this payment certificate within the certificates of this transaction. |
| `amount` | int65type | The payment amount (in Lovelace). |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this payment. |

### `reserve`

A table for payments from the reserves to a StakeAddress.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address for this Treasury entry. |
| `cert_index` | integer (32) | The index of this payment certificate within the certificates of this transaction. |
| `amount` | int65type | The payment amount (in Lovelace). |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this payment. |

### `pot_transfer`

A table containing transfers between the reserves pot and the treasury pot.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `cert_index` | integer (32) | The index of this transfer certificate within the certificates of this transaction. |
| `treasury` | int65type | The amount (in Lovelace) the treasury balance changes by. |
| `reserves` | int65type | The amount (in Lovelace) the reserves balance changes by. |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this transfer. |

### `epoch_sync_time`

A table containing the time required to fully sync an epoch.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `no` | integer (64) | The epoch number for this sync time. |
| `seconds` | word63type | The time (in seconds) required to sync this epoch (may be NULL for an epoch that was already partially synced when `db-sync` was started). |
| `state` | syncstatetype | The sync state when the sync time is recorded (either 'lagging' or 'following'). |

### `ma_tx_mint`

A table containing Multi-Asset mint events.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `policy` | hash28type | The MultiAsset policy hash. |
| `name` | asset32type | The MultiAsset name. |
| `quantity` | int65type | The amount of the Multi Asset to mint (can be negative to "burn" assets). |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this minting event. |

### `ma_tx_out`

A table containing Multi-Asset transaction outputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `policy` | hash28type | The MultiAsset policy hash. |
| `name` | asset32type | The MultiAsset name. |
| `quantity` | word64type | The Multi Asset transaction output amount (denominated in the Multi Asset). |
| `tx_out_id` | integer (64) | The TxOut table index for the transaction that this Multi Asset transaction output. |

### `redeemer`

A table containing redeemers. A redeemer is provided for all items that are validated by a script.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index that contains this redeemer. |
| `unit_mem` | word63type | The budget in Memory to run a script. |
| `unit_steps` | word63type | The budget in Cpu steps to run a script. |
| `fee` | lovelace | The budget in fees to run a script. The fees depend on the ExUnits and the current prices. |
| `purpose` | scriptpurposetype | What kind pf validation this redeemer is used for. It can be one of 'spend', 'mint', 'cert', 'reward'. |
| `index` | uinteger | The index of the redeemer pointer in the transaction. |
| `script_hash` | hash28type | The script hash this redeemer is used for. |

### `script`

A table containing scripts available in the blockchain, found in witnesses or auxdata of transactions.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index for the transaction where this script first became available. |
| `hash` | hash28type | The Hash of the Script. |
| `type` | scripttype | The type of the script. This is currenttly either 'timelock' or 'plutus'. |
| `serialised_size` | uinteger | The size of the CBOR serialised script, if it is a Plutus script. |

### `param_proposal`

A table containing block chain parameter change proposals.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `epoch_no` | uinteger | The epoch for which this parameter proposal in intended to become active. |
| `key` | hash28type | The hash of the crypto key used to sign this proposal. |
| `min_fee_a` | word64type | The 'a' parameter to calculate the minimum transaction fee. |
| `min_fee_b` | word64type | The 'b' parameter to calculate the minimum transaction fee. |
| `max_block_size` | word64type | The maximum block size (in bytes). |
| `max_tx_size` | word64type | The maximum transaction size (in bytes). |
| `max_bh_size` | word64type | The maximum block header size (in bytes). |
| `key_deposit` | lovelace | The amount (in Lovelace) require for a deposit to register a StakeAddress. |
| `pool_deposit` | lovelace | The amount (in Lovelace) require for a deposit to register a stake pool. |
| `max_epoch` | word64type | The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for. |
| `optimal_pool_count` | word64type | The optimal number of stake pools. |
| `influence` | double | The influence of the pledge on a stake pool's probability on minting a block. |
| `monetary_expand_rate` | double | The monetary expansion rate. |
| `treasury_growth_rate` | double | The treasury growth rate. |
| `decentralisation` | double | The decentralisation parameter (1 fully centralised, 0 fully decentralised). |
| `entropy` | hash32type | The 32 byte string of extra random-ness to be added into the protocol's entropy pool. |
| `protocol_major` | uinteger | The protocol major number. |
| `protocol_minor` | uinteger | The protocol minor number. |
| `min_utxo_value` | lovelace | The minimum value of a UTxO entry. |
| `min_pool_cost` | lovelace | The minimum pool cost. |
| `coins_per_utxo_word` | lovelace | The cost per UTxO word. |
| `cost_models` | string | The per language cost models. |
| `price_mem` | double | The per word cost of script memory usage. |
| `price_step` | double | The cost of script execution step usage. |
| `max_tx_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single transaction. |
| `max_tx_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single transaction. |
| `max_block_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single block. |
| `max_block_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single block. |
| `max_val_size` | word64type | The maximum Val size. |
| `collateral_percent` | uinteger | The percentage of the txfee which must be provided as collateral when including non-native scripts. |
| `max_collateral_inputs` | uinteger | The maximum number of collateral inputs allowed in a transaction. |
| `registered_tx_id` | integer (64) | The Tx table index for the transaction that contains this parameter proposal. |

### `epoch_param`

The accepted protocol parameters for an epoch.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `epoch_no` | uinteger | The first epoch for which these parameters are valid. |
| `min_fee_a` | uinteger | The 'a' parameter to calculate the minimum transaction fee. |
| `min_fee_b` | uinteger | The 'b' parameter to calculate the minimum transaction fee. |
| `max_block_size` | uinteger | The maximum block size (in bytes). |
| `max_tx_size` | uinteger | The maximum transaction size (in bytes). |
| `max_bh_size` | uinteger | The maximum block header size (in bytes). |
| `key_deposit` | lovelace | The amount (in Lovelace) require for a deposit to register a StakeAddress. |
| `pool_deposit` | lovelace | The amount (in Lovelace) require for a deposit to register a stake pool. |
| `max_epoch` | uinteger | The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for. |
| `optimal_pool_count` | uinteger | The optimal number of stake pools. |
| `influence` | double | The influence of the pledge on a stake pool's probability on minting a block. |
| `monetary_expand_rate` | double | The monetary expansion rate. |
| `treasury_growth_rate` | double | The treasury growth rate. |
| `decentralisation` | double | The decentralisation parameter (1 fully centralised, 0 fully decentralised). |
| `entropy` | hash32type | The 32 byte string of extra random-ness to be added into the protocol's entropy pool. |
| `protocol_major` | uinteger | The protocol major number. |
| `protocol_minor` | uinteger | The protocol minor number. |
| `min_utxo_value` | lovelace | The minimum value of a UTxO entry. |
| `min_pool_cost` | lovelace | The minimum pool cost. |
| `nonce` | hash32type | The nonce value for this epoch. |
| `coins_per_utxo_word` | lovelace | The cost per UTxO word. |
| `cost_models` | string | The per language cost models. |
| `price_mem` | double | The per word cost of script memory usage. |
| `price_step` | double | The cost of script execution step usage. |
| `max_tx_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single transaction. |
| `max_tx_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single transaction. |
| `max_block_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single block. |
| `max_block_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single block. |
| `max_val_size` | word64type | The maximum Val size. |
| `collateral_percent` | uinteger | The percentage of the txfee which must be provided as collateral when including non-native scripts. |
| `max_collateral_inputs` | uinteger | The maximum number of collateral inputs allowed in a transaction. |
| `block_id` | integer (64) | The Block table index for the first block where these parameters are valid. |

### `pool_offline_data`

The pool offline (ie not on chain) for a stake pool.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `pool_id` | integer (64) | The PoolHash table index for the pool this offline data refers. |
| `ticker_name` | string | The pool's ticker name (as many as 5 characters). |
| `hash` | hash32type | The hash of the offline data. |
| `json` | jsonb | The payload as JSON. |
| `bytes` | bytea | The raw bytes of the payload. |
| `pmr_id` | integer (64) | The PoolMetadataRef table index for this offline data. |

### `pool_offline_fetch_error`

A table containing pool offline data fetch errors.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `pool_id` | integer (64) | The PoolHash table index for the pool this offline fetch error refers. |
| `fetch_time` | timestamp | The UTC time stamp of the error. |
| `pmr_id` | integer (64) | The PoolMetadataRef table index for this offline data. |
| `fetch_error` | string | The text of the error. |
| `retry_count` | uinteger | The number of retries. |

### `epoch_reward_total_received`

This table is used to help validate the accounting of rewards. It contains the total reward  amount for each epoch received from the ledger state and includes orphaned rewards that  are later removed from the Reward table.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `earned_epoch` | uinteger | The epoch in which the reward was earned. |
| `amount` | lovelace | The total rewards for the epoch in Lovelace. |

### `reserved_pool_ticker`

A table containing a managed list of reserved ticker names.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `name` | string | The ticker name. |
| `pool_id` | integer (64) | The PoolHash table index for the pool that has reserved this name. |

### `admin_user`

A table listing all admin users (for maintaining the SMASH related data).

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `username` | string | The user name. |
| `password` | string | The password. |


