Resolving dependencies...
Up to date
# Schema Documentation for cardano-db-sync

Schema version: 13.1.1.3 (from branch **kderme/stake-dist-master** which may not accurately reflect the version number)
**Note:** This file is auto-generated from the documentation in cardano-db/src/Cardano/Db/Schema.hs by the command `cabal run -- gen-schema-docs doc/schema.md`. This document should only be updated during the release process and updated on the release branch.

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

A table for every unique pool key hash. The existance of an entry doesn't mean the pool is registered or in fact that is was ever registered.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_raw` | hash28type | The raw bytes of the pool hash. |
| `view` | string | The Bech32 encoding of the pool hash. |

### `slot_leader`

Every unique slot leader (ie an entity that mines a block). It could be a pool or a leader defined in genesis.

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
| `epoch_no` | word31type | The epoch number. |
| `slot_no` | word63type | The slot number. |
| `epoch_slot_no` | word31type | The slot number within an epoch (resets to zero at the start of each epoch). |
| `block_no` | word31type | The block number. |
| `previous_id` | integer (64) | The Block table index of the previous block. |
| `slot_leader_id` | integer (64) | The SlotLeader table index of the creator of this block. |
| `size` | word31type | The block size (in bytes). Note, this size value is not expected to be the same as the sum of the tx sizes due to the fact that txs being stored in segwit format and oddities in the CBOR encoding. |
| `time` | timestamp | The block time (UTCTime). |
| `tx_count` | integer (64) | The number of transactions in this block. |
| `proto_major` | word31type | The block's major protocol number. |
| `proto_minor` | word31type | The block's major protocol number. |
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
| `block_index` | word31type | The index of this transaction with the block (zero based). |
| `out_sum` | lovelace | The sum of the transaction outputs (in Lovelace). |
| `fee` | lovelace | The fees paid for this transaction. |
| `deposit` | integer (64) | Deposit (or deposit refund) in this transaction. Deposits are positive, refunds negative. |
| `size` | word31type | The size of the transaction in bytes. |
| `invalid_before` | word64type | Transaction in invalid before this slot number. |
| `invalid_hereafter` | word64type | Transaction in invalid at or after this slot number. |
| `valid_contract` | boolean | False if the contract is invalid. True if the contract is valid or there is no contract. |
| `script_size` | word31type | The sum of the script sizes (in bytes) of scripts in the transaction. |

### `reverse_index`

A table for reverse indexes for the minimum input output and multi asset output related with this block. New in v13.1

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `block_id` | integer (64) | The Block table index related with these indexes |
| `min_ids` | string | The Reverse indexes associated with this block, as Text separated by : |

### `stake_address`

A table of unique stake addresses. Can be an actual address or a script hash.  The existance of an entry doesn't mean the address is registered or in fact that is was ever registered.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_raw` | addr29type | The raw bytes of the stake address hash. |
| `view` | string | The Bech32 encoded version of the stake address. |
| `script_hash` | hash28type | The script hash, in case this address is locked by a script. |

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
| `inline_datum_id` | integer (64) | The inline datum of the output, if it has one. New in v13. |
| `reference_script_id` | integer (64) | The reference script of the output, if it has one. New in v13. |

### `collateral_tx_out`

A table for transaction collateral outputs. New in v13.

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
| `multi_assets_descr` | string | This is a description of the multiassets in collateral output. Since the output is not really created, we don't need to add them in separate tables. |
| `inline_datum_id` | integer (64) | The inline datum of the output, if it has one. New in v13. |
| `reference_script_id` | integer (64) | The reference script of the output, if it has one. New in v13. |

### `tx_in`

A table for transaction inputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_in_id` | integer (64) | The Tx table index of the transaction that contains this transaction input. |
| `tx_out_id` | integer (64) | The Tx table index of the transaction that contains the referenced transaction output. |
| `tx_out_index` | txindex | The index within the transaction outputs. |
| `redeemer_id` | integer (64) | The Redeemer table index which is used to validate this input. |

### `collateral_tx_in`

A table for transaction collateral inputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_in_id` | integer (64) | The Tx table index of the transaction that contains this transaction input |
| `tx_out_id` | integer (64) | The Tx table index of the transaction that contains the referenced transaction output. |
| `tx_out_index` | txindex | The index within the transaction outputs. |

### `reference_tx_in`

A table for reference transaction inputs. New in v13.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_in_id` | integer (64) | The Tx table index of the transaction that contains this transaction input |
| `tx_out_id` | integer (64) | The Tx table index of the transaction that contains the referenced output. |
| `tx_out_index` | txindex | The index within the transaction outputs. |

### `meta`

A table containing metadata about the chain. There will probably only ever be one row in this table.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `start_time` | timestamp | The start time of the network. |
| `network_name` | string | The network name. |
| `version` | string |  |

### `epoch`

Aggregation of data within an epoch.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `out_sum` | word128type | The sum of the transaction output values (in Lovelace) in this epoch. |
| `fees` | lovelace | The sum of the fees (in Lovelace) in this epoch. |
| `tx_count` | word31type | The number of transactions in this epoch. |
| `blk_count` | word31type | The number of blocks in this epoch. |
| `no` | word31type | The epoch number. |
| `start_time` | timestamp | The epoch start time. |
| `end_time` | timestamp | The epoch end time. |

### `ada_pots`

A table with all the different types of total balances (Shelley only).
The treasury and rewards fields will be correct for the whole epoch, but all other fields change block by block.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `slot_no` | word63type | The slot number where this AdaPots snapshot was taken. |
| `epoch_no` | word31type | The epoch number where this AdaPots snapshot was taken. |
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
| `url` | varchar | The URL for the location of the off-chain data. |
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
| `reward_addr_id` | integer (64) | The StakeAddress table index of this pool's rewards address. New in v13: Replaced reward_addr. |
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
| `pool_update_id` | integer (64) | The PoolUpdate table index for the pool. New in v13. |

### `pool_retire`

A table containing information about pools retiring.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_id` | integer (64) | The PoolHash table index of the pool this retirement refers to. |
| `cert_index` | integer (32) | The index of this pool retirement within the certificates of this transaction. |
| `announced_tx_id` | integer (64) | The Tx table index of the transaction where this pool retirement was announced. |
| `retiring_epoch` | word31type | The epoch where this pool retires. |

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
| `epoch_no` | word31type | The epoch in which the registration took place. |
| `tx_id` | integer (64) | The Tx table index of the transaction where this stake address was registered. |

### `stake_deregistration`

A table containing stake address deregistrations.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address. |
| `cert_index` | integer (32) | The index of this stake deregistration within the certificates of this transaction. |
| `epoch_no` | word31type | The epoch in which the deregistration took place. |
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
| `slot_no` | word63type | The slot number of the block that contained this delegation. |
| `redeemer_id` | integer (64) | The Redeemer table index that is related with this certificate. |
| `deposit` | lovelace | The deposit that may appear at the certificate. New in 13.2-Conway |

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

A table for earned rewards. It includes 5 types of rewards. The rewards are inserted incrementally and this procedure is finalised when the spendable epoch comes. Before the epoch comes, some entries may be missing.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address that earned the reward. |
| `type` | rewardtype | The source of the rewards; pool `member`, pool `leader`, `treasury` or `reserves` payment and pool deposits `refunds` |
| `amount` | lovelace | The reward amount (in Lovelace). |
| `earned_epoch` | integer (64) | The epoch in which the reward was earned. For `pool` and `leader` rewards spendable in epoch `N`, this will be `N - 2`, for `treasury` and `reserves` `N - 1` and for `refund` N. |
| `spendable_epoch` | integer (64) | The epoch in which the reward is actually distributed and can be spent. |
| `pool_id` | integer (64) | The PoolHash table index for the pool the stake address was delegated to when the reward is earned or for the pool that there is a deposit refund. Will be NULL for payments from the treasury or the reserves. |

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

A table containing the epoch stake distribution for each epoch. This is inserted incrementally in the first blocks of the epoch. The stake distribution is extracted from the `set` snapshot of the ledger. See Shelley specs Sec. 11.2 for more details.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address for this EpochStake entry. |
| `pool_id` | integer (64) | The PoolHash table index for the pool this entry is delegated to. |
| `amount` | lovelace | The amount (in Lovelace) being staked. |
| `epoch_no` | word31type | The epoch number. |

### `epoch_stake_progress`

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `epoch_no` | word31type |  |
| `completed` | boolean |  |

### `treasury`

A table for payments from the treasury to a StakeAddress. Note: Before protocol version 5.0 (Alonzo) if more than one payment was made to a stake address in a single epoch, only the last payment was kept and earlier ones removed. For protocol version 5.0 and later, they are summed and produce a single reward with type `treasury`.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address for this Treasury entry. |
| `cert_index` | integer (32) | The index of this payment certificate within the certificates of this transaction. |
| `amount` | int65type | The payment amount (in Lovelace). |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this payment. |

### `reserve`

A table for payments from the reserves to a StakeAddress. Note: Before protocol version 5.0 (Alonzo) if more than one payment was made to a stake address in a single epoch, only the last payment was kept and earlier ones removed. For protocol version 5.0 and later, they are summed and produce a single reward with type `reserves`

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

### `multi_asset`

A table containing all the unique policy/name pairs along with a CIP14 asset fingerprint

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `policy` | hash28type | The MultiAsset policy hash. |
| `name` | asset32type | The MultiAsset name. |
| `fingerprint` | string | The CIP14 fingerprint for the MultiAsset. |

### `ma_tx_mint`

A table containing Multi-Asset mint events.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `ident` | integer (64) | The MultiAsset table index specifying the asset. |
| `quantity` | int65type | The amount of the Multi Asset to mint (can be negative to "burn" assets). |
| `tx_id` | integer (64) | The Tx table index for the transaction that contains this minting event. |

### `ma_tx_out`

A table containing Multi-Asset transaction outputs.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `ident` | integer (64) | The MultiAsset table index specifying the asset. |
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
| `fee` | lovelace | The budget in fees to run a script. The fees depend on the ExUnits and the current prices. Is null when --disable-ledger is enabled. New in v13: became nullable. |
| `purpose` | scriptpurposetype | What kind pf validation this redeemer is used for. It can be one of 'spend', 'mint', 'cert', 'reward'. |
| `index` | word31type | The index of the redeemer pointer in the transaction. |
| `script_hash` | hash28type | The script hash this redeemer is used for. |
| `redeemer_data_id` | integer (64) | The data related to this redeemer. New in v13: renamed from datum_id. |

### `script`

A table containing scripts available, found in witnesses, inlined in outputs (reference outputs) or auxdata of transactions.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index for the transaction where this script first became available. |
| `hash` | hash28type | The Hash of the Script. |
| `type` | scripttype | The type of the script. This is currenttly either 'timelock' or 'plutus'. |
| `json` | jsonb | JSON representation of the timelock script, null for other script types |
| `bytes` | bytea | CBOR encoded plutus script data, null for other script types |
| `serialised_size` | word31type | The size of the CBOR serialised script, if it is a Plutus script. |

### `datum`

A table containing Plutus Datum, found in witnesses or inlined in outputs

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash32type | The Hash of the Datum |
| `tx_id` | integer (64) | The Tx table index for the transaction where this script first became available. |
| `value` | jsonb | The actual data in JSON format (detailed schema) |
| `bytes` | bytea | The actual data in CBOR format |

### `redeemer_data`

A table containing Plutus Redeemer Data. These are always referenced by at least one redeemer. New in v13: split from datum table.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash32type | The Hash of the Plutus Data |
| `tx_id` | integer (64) | The Tx table index for the transaction where this script first became available. |
| `value` | jsonb | The actual data in JSON format (detailed schema) |
| `bytes` | bytea | The actual data in CBOR format |

### `extra_key_witness`

A table containing transaction extra key witness hashes.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash28type | The hash of the witness. |
| `tx_id` | integer (64) | The id of the tx this witness belongs to. |

### `param_proposal`

A table containing block chain parameter change proposals.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `epoch_no` | word31type | The epoch for which this parameter proposal in intended to become active. Changed in 13.2-Conway to nullable is always null in Conway era. |
| `key` | hash28type | The hash of the crypto key used to sign this proposal. Changed in 13.2-Conway to nullable is always null in Conway era. |
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
| `protocol_major` | word31type | The protocol major number. |
| `protocol_minor` | word31type | The protocol minor number. |
| `min_utxo_value` | lovelace | The minimum value of a UTxO entry. |
| `min_pool_cost` | lovelace | The minimum pool cost. |
| `coins_per_utxo_size` | lovelace | For Alonzo this is the cost per UTxO word. For Babbage and later per UTxO byte. New in v13: Renamed from coins_per_utxo_word. |
| `cost_model_id` | integer (64) | The CostModel table index for the proposal. |
| `price_mem` | double | The per word cost of script memory usage. |
| `price_step` | double | The cost of script execution step usage. |
| `max_tx_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single transaction. |
| `max_tx_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single transaction. |
| `max_block_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single block. |
| `max_block_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single block. |
| `max_val_size` | word64type | The maximum Val size. |
| `collateral_percent` | word31type | The percentage of the txfee which must be provided as collateral when including non-native scripts. |
| `max_collateral_inputs` | word31type | The maximum number of collateral inputs allowed in a transaction. |
| `registered_tx_id` | integer (64) | The Tx table index for the transaction that contains this parameter proposal. |

### `epoch_param`

The accepted protocol parameters for an epoch.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `epoch_no` | word31type | The first epoch for which these parameters are valid. |
| `min_fee_a` | word31type | The 'a' parameter to calculate the minimum transaction fee. |
| `min_fee_b` | word31type | The 'b' parameter to calculate the minimum transaction fee. |
| `max_block_size` | word31type | The maximum block size (in bytes). |
| `max_tx_size` | word31type | The maximum transaction size (in bytes). |
| `max_bh_size` | word31type | The maximum block header size (in bytes). |
| `key_deposit` | lovelace | The amount (in Lovelace) require for a deposit to register a StakeAddress. |
| `pool_deposit` | lovelace | The amount (in Lovelace) require for a deposit to register a stake pool. |
| `max_epoch` | word31type | The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for. |
| `optimal_pool_count` | word31type | The optimal number of stake pools. |
| `influence` | double | The influence of the pledge on a stake pool's probability on minting a block. |
| `monetary_expand_rate` | double | The monetary expansion rate. |
| `treasury_growth_rate` | double | The treasury growth rate. |
| `decentralisation` | double | The decentralisation parameter (1 fully centralised, 0 fully decentralised). |
| `extra_entropy` | hash32type | The 32 byte string of extra random-ness to be added into the protocol's entropy pool. New in v13: renamed from entopy. |
| `protocol_major` | word31type | The protocol major number. |
| `protocol_minor` | word31type | The protocol minor number. |
| `min_utxo_value` | lovelace | The minimum value of a UTxO entry. |
| `min_pool_cost` | lovelace | The minimum pool cost. |
| `nonce` | hash32type | The nonce value for this epoch. |
| `coins_per_utxo_size` | lovelace | For Alonzo this is the cost per UTxO word. For Babbage and later per UTxO byte. New in v13: Renamed from coins_per_utxo_word. |
| `cost_model_id` | integer (64) | The CostModel table index for the params. |
| `price_mem` | double | The per word cost of script memory usage. |
| `price_step` | double | The cost of script execution step usage. |
| `max_tx_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single transaction. |
| `max_tx_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single transaction. |
| `max_block_ex_mem` | word64type | The maximum number of execution memory allowed to be used in a single block. |
| `max_block_ex_steps` | word64type | The maximum number of execution steps allowed to be used in a single block. |
| `max_val_size` | word64type | The maximum Val size. |
| `collateral_percent` | word31type | The percentage of the txfee which must be provided as collateral when including non-native scripts. |
| `max_collateral_inputs` | word31type | The maximum number of collateral inputs allowed in a transaction. |
| `block_id` | integer (64) | The Block table index for the first block where these parameters are valid. |

### `cost_model`

CostModel for EpochParam and ParamProposal.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash` | hash32type | The hash of cost model. It ensures uniqueness of entries. New in v13. |
| `costs` | jsonb | The actual costs formatted as json. |

### `extra_migrations`

Extra optional migrations. New in 13.2.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `token` | string |  |
| `description` | string | A description of the migration |

### `drep_hash`

A table for every unique drep key hash. The existance of an entry doesn't mean the DRep is registered or in fact that is was ever registered. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `raw` | hash28type | The raw bytes of the DRep. |
| `view` | string | A description of the DRep hash. This only exists for AlwaysAbstain and AlwaysNoConfidence. |
| `has_script` | boolean | Flag which shows if this DRep credentials are a script hash |

### `delegation_vote`

A table containing delegations from a stake address to a stake pool. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `addr_id` | integer (64) | The StakeAddress table index for the stake address. |
| `cert_index` | integer (32) | The index of this delegation within the certificates of this transaction. |
| `drep_hash_id` | integer (64) | The DrepHash table index for the pool being delegated to. |
| `active_epoch_no` | integer (64) | The epoch number where this delegation becomes active. |
| `tx_id` | integer (64) | The Tx table index of the transaction that contained this delegation. |
| `redeemer_id` | integer (64) | The Redeemer table index that is related with this certificate. TODO: can vote redeemers index these delegations? |

### `committee_registration`

A table for every committee hot key registration. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this certificate. |
| `cert_index` | integer (32) | The index of this registration within the certificates of this transaction. |
| `cold_key` | addr29type | The registered cold hey hash. TODO: should this reference DrepHashId or some separate hash table? |
| `hot_key` | addr29type | The registered hot hey hash |

### `committee_de_registration`

A table for every committee key de-registration. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this certificate. |
| `cert_index` | integer (32) | The index of this deregistration within the certificates of this transaction. |
| `hot_key` | addr29type | The deregistered hot key hash |

### `drep_registration`

A table for every DRep registration. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this certificate. |
| `cert_index` | integer (32) | The index of this registration within the certificates of this transaction. |
| `deposit` | lovelace |  |
| `drep_hash_id` | integer (64) | The registered cold hey hash. TODO: should this reference DrepHashId or some separate hash table? |

### `drep_de_registration`

A table for every DRep de-registration. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this certificate. |
| `cert_index` | integer (32) | The index of this deregistration within the certificates of this transaction. |
| `deposit` | lovelace |  |
| `drep_hash_id` | integer (64) | The deregistered drep hash |

### `voting_anchor`

A table for every Anchor that appears on Governance Actions. These are pointers to offchain metadata. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this anchor. This only exists to facilitate rollbacks |
| `url` | varchar | A URL to a JSON payload of metadata |
| `data_hash` | blob | A hash of the contents of the metadata URL |

### `governance_action`

A table for proposed GovernanceAction, aka ProposalProcedure, GovAction or GovProposal. At most one of the ratified/enacted/dropped/expired epoch field can be non-null, indicating the current state of the proposal. This table may be referenced by TreasuryWithdrawal or NewCommittee. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this certificate. |
| `index` | integer (64) | The index of this proposal procedure within its transaction. |
| `deposit` | lovelace | The deposit amount payed for this proposal. |
| `return_address` | integer (64) | The StakeAddress index of the reward address to receive the deposit when it is repaid. |
| `voting_anchor_id` | integer (64) | The Anchor table index related to this proposal. |
| `type` | govactiontype | Can be one of ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, NewCommittee, NewConstitution, InfoAction |
| `description` | string | A Text describing the content of this GovernanceAction in a readable way. |
| `param_proposal` | integer (64) | If this is a param proposal action, this has the index of the param_proposal table. |
| `ratified_epoch` | word31type | If not null, then this proposal has been ratified at the specfied epoch. |
| `enacted_epoch` | word31type | If not null, then this proposal has been enacted at the specfied epoch. |
| `dropped_epoch` | word31type | If not null, then this proposal has been enacted at the specfied epoch. |
| `expired_epoch` | word31type | If not null, then this proposal has been enacted at the specfied epoch. |

### `treasury_withdrawal`

A table for all treasury withdrawals proposed on a GovernanceAction. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `governance_action_id` | integer (64) | The GovernanceAction table index for this withdrawal.Multiple TreasuryWithdrawal may reference the same GovernanceAction. |
| `stake_address_id` | integer (64) | The address that benefits from this withdrawal. |
| `amount` | lovelace | The amount for this withdrawl. |

### `new_committee`

A table for new committee proposed on a GovernanceAction. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `governance_action_id` | integer (64) | The GovernanceAction table index for this new committee. |
| `quorum` | double | The proposed quorum. |
| `members` | string | The members of the committee. This is now given in a text as a description, but may change. TODO: Conway. |

### `voting_procedure`

A table for voting procedures, aka GovVote. A Vote can be Yes No or Abstain. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `tx_id` | integer (64) | The Tx table index of the tx that includes this VotingProcedure. |
| `index` | integer (32) | The index of this VotingProcedure within this transaction. |
| `governance_action_id` | integer (64) | The index of the GovernanceAction that this vote targets. |
| `voter_role` | voterrole | The role of the voter. Can be one of ConstitutionalCommittee, DRep, SPO. |
| `comittee_voter` | blob |  |
| `drep_voter` | integer (64) |  |
| `pool_voter` | integer (64) |  |
| `vote` | vote | The Vote. Can be one of Yes, No, Abstain. |
| `voting_anchor_id` | integer (64) | The VotingAnchor table index associated with this VotingProcedure. |

### `anchor_offline_data`

The table with the off chain metadata related to Vote Anchors. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `voting_anchor_id` | integer (64) | The VotingAnchor table index this offline data refers. |
| `hash` | blob | The hash of the offline data. |
| `json` | jsonb | The payload as JSON. |
| `bytes` | bytea | The raw bytes of the payload. |

### `anchor_offline_fetch_error`

Errors while fetching or validating offline Voting Anchor metadata. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `voting_anchor_id` | integer (64) | The VotingAnchor table index this offline fetch error refers. |
| `fetch_error` | string | The text of the error. |
| `retry_count` | word31type | The number of retries. |

### `drep_distr`

The table for the distribution of voting power per DRep per. Currently this has a single entry per DRep and doesn't show every delegator. This may change. New in 13.2-Conway.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_id` | integer (64) | The DrepHash table index that this distribution entry has information about. |
| `amount` | integer (64) | The total amount of voting power this DRep is delegated. |
| `epoch_no` | word31type | The epoch no this distribution is about. |

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
| `retry_count` | word31type | The number of retries. |

### `reserved_pool_ticker`

A table containing a managed list of reserved ticker names.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `name` | string | The ticker name. |
| `pool_hash` | hash28type | The hash of the pool that owns this ticker. |

### `delisted_pool`

A table containing pools that have been delisted.

* Primary Id: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `hash_raw` | hash28type | The pool hash |


