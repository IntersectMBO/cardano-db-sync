# Configuration

The initial design of db-sync was a one size fits all approach. It served as a general-purpose
backend for Cardano applications, including light wallets, explorers, etc. Over time, many new
features have been added, including historic rewards with Shelley, scripts with Allegra, multiassets
with Mary, Plutus scripts and redeemers with Alonzo, stake pool metadata with the integration of
SMASH, etc.

Most application use only a small fraction of these features. Therefore, db-sync offers options that
turn off some of these features, especially the most expensive ones in terms of performance.

## Configuration File

Starting with db-sync 13.3.0.0, customizing features is done in the standard db-sync configuration
file (`db-sync-config.json` or `db-sync-config.yaml`).

## Simple Example

Below is a sample `insert_options` section that shows all the defaults:

```
{
  // <-- Rest of configuration -->
  // ...

  "insert_options": {
    "tx_out": {
      "value": "enable"
    },
    "ledger": "enable",
    "shelley": {
      "enable": true
    },
    "multi_asset": {
      "enable": true
    },
    "metadata": {
      "enable": true
    },
    "plutus": {
      "enable": true
    },
    "governance": "enable",
    "offchain_pool_data": "enable",
    "json_type": "text"
  }
}
```

## Properties

`insert_options` may contain the following elements:

| Property                                    | Type       | Required |
| :------------------------------------------ | :--------- | :------  |
| [preset](#preset)                           | `enum`     | Optional |
| [tx\_out](#tx-out)                          | `object`   | Optional |
| [ledger](#ledger)                           | `enum`     | Optional |
| [shelley](#shelley)                         | `object`   | Optional |
| [multi\_asset](#multi-asset)                | `object`   | Optional |
| [metadata](#metadata)                       | `object`   | Optional |
| [plutus](#plutus)                           | `object`   | Optional |
| [governance](#governance)                   | `enum`     | Optional |
| [offchain\_pool\_data](#offchain-pool-data) | `enum`     | Optional |

### Preset

Preset is an aggregate setting that overrides all other properties. For example, setting
preset to `"full"` will enable all insert options.

`preset`

 * Type: `string`

**enum**: The value of this property must be equal to one of the following values:

| Value           | Explanation                                                  |
| :-----------    | :----------------------------------------------------------- |
| `"full"`        | Enable all options                                           |
| `"only_utxo"`   | Only load `block`, `tx`, `tx_out` and `ma_tx_out`            |
| `"only_gov"`    | Disable most data except governance data.                    |
| `"disable_all"` | Only load `block`, `tx` and data related to the ledger state |

**Full**

This is equivalent to enabling all other settings.

**Only UTxO**

This is equivalent to setting:

```
"tx_out": {
  "value": "bootstrap"
},
"ledger": "ignore",
"shelley": {
  "enable": false
},
"plutus": {
  "enable": false
},
"governance": "disable",
"offchain_pool_data": "disable"
```

Initially populates only a few tables, like `block` and `tx`. It maintains a ledger state but
doesn't use any of its data. When syncing is completed, it loads the whole UTxO set from the ledger
to the `tx_out` and `ma_tx_out` tables.  After that db-sync can be restarted with `ledger` set to
`"disable"` to continue syncing without maintaining the ledger

**Only Gov**

This is equivalent to setting:

```
"tx_out": {
  "value": "disable"
},
"ledger": "disable",
"shelley": {
  "enable": false
},
"multi_asset": {
  "enable": false
},
"plutus": {
  "enable": false
},
"governance": "enable",
"offchain_pool_data": "disable"

```

Disables most data except `block`, `tx`, and governance data.

**Disable All**

This is equivalent to setting:

```
"tx_out": {
  "value": "disable"
},
"ledger": "disable",
"shelley": {
  "enable": false
},
"multi_asset": {
  "enable": false
},
"plutus": {
  "enable": false
},
"governance": "disable",
"offchain_pool_data": "disable"
```

Disables almost all data except `block` and `tx` tables.

### Tx Out

`tx_out`

 * Type: `object`

Tx Out Properties:

| Property                      | Type      | Required |
| :---------------------------- | :-------- | :------- |
| [value](#value)               | `string`  | Optional |
| [force\_tx\_in](#force-tx-in) | `boolean` | Optional |

#### Value

`tx_out.value`

 * Type: `string`

**enum**: the value of this property must be equal to one of the following values:

| Value         | Explanation                                                             |
| :------------ | :---------------------------------------------------------------------- |
| `"enable"`    | Enable all tx inputs and outputs                                        |
| `"disable"`   | Disable tx inputs and outputs                                           |
| `"consumed"`  | Adds a new field `tx_out (consumed_by_tx_id)`                           |
| `"prune"`     | Periodically prune the consumed tx_out table                            |
| `"bootstrap"` | Prune consumed `tx_out` table, delays writing UTxOs until fully synched |

**Enable**

Enable all inputs and outputs.

**Disable**

Disable inputs and outputs. With this flag:

 * `tx_in` table is left empty
 * `tx_out` table is left empty
 * `ma_tx_out` table is left empty
 * `tx.fee` has a wrong value 0
 * `redeemer.script_hash` is left Null

It's similar to `"bootstrap"` except the UTxO is never populated. However, after using this flag
db-sync can be stopped and restarted with `"bootstrap"` to load the UTxO from the ledger.

**Consumed**

Adds a new field `tx_out (consumed_by_tx_id)` and populates it accordingly. This allows users to
query the tx_out table for unspent outputs directly, without the need to join with the `tx_in`
table.  If this is set once, then it must be always be set on following executions of db-sync,
unless `prune-tx-out` is used instead.

**Prune**

Periodically prunes the consumed `tx_out` table. This allows users to query for utxo without having
to maintain the whole `tx_out` table. Deletes to `tx_out` are propagated to `ma_tx_out` through
foreign keys. If this is set once, then it must be always set on subsequent executions of
db-sync. Failure to do this can result in crashes and db-sync currently has no way to detect it.

**Bootstrap**

Results in a similar db schema as using `"prune"`, except it syncs faster. The difference is that
instead of inserting/updating/deleting outputs, it delays the insertion of UTxO until the tip of the
chain. By doing so, it avoid costly db operations for the majority of outputs, that are eventually
consumed and as a result deleted. UTxO are eventually inserted in bulk from the ledger state.  The
initial implementation of the feautures assumes `ledger` is set to `"enable"` , since the ledger
state is used. The networks needs to be in Babbage or Conway era for this to work. The following
fields are left empty:

 * `tx.fee` has a wrong value 0
 * `redeemer.script_hash` is left Null

Until the ledger state migration happens, any restart requires this setting. After completion, this
can be changed.

#### Force Tx In

`tx_out.force_tx_in`

 * Type: `boolean`

### Ledger

One of the db-sync features that uses the most resources is that it maintains a ledger state and
replays all the ledger rules. This is the only way to get historic reward details and other data
that is not included in the blocks (ie. historic stake distribution, ada pots, epoch parameters,
etc).

`ledger`

 * Type: `string`

**enum**: The value of this property must be equal to one of the following values:

| Value      | Explanation |
| :----      | :---------- |
| `"enable"` | Maintain ledger state and replay all ledger rules    |
| `"disable"`| Do not maintain a ledger state                       |
| `"ignore"` | Maintain ledger state, but don't use any of its data |

**Enable**

Maintain ledger state and replay all ledger rules.

**Disable**

Turn off ledger state and significantly reduce memory usage (by up to 10GB on mainnet) and sync
time. Another benefit of this option is that there are no rollbacks on startup, which tend to take
quite some time, since there are no ledger snapshots maintained on disk.

When this flag is enabled:

 * `redeemer.fee` is left null
 * `reward` table is left empty
 * `epoch_stake` table is left empty
 * `ada_pots` table is left empty
 * `epoch_param` table is left empty
 * `tx.deposit` is left null (too expensive to calculate without the ledger)
 * `drep_distr` is left empty
 * `governance_action.x_epoch` is left null
 * `governance_action.expiration` is left null

Warning: Running db-sync with this setting and restarting it with a different one will cause crashes
and should be avoided.

Warning: If this setting is used with the `--state-dir` command line option, an error will occur.

Released snapshots are compatible with this option. Since the snapshots are created with `ledger`
enabled, there still can be some minor inconsistencies. The above data may exist up to the
slot/epoch of the snapshot creation and can be missing afterward. To fix this, when db-sync is
initiated with `ledger` disabled, it will automatically remove this data.

Warning: This will irreversibly delete data from existing snapshots.

Here are the exact queries db-sync with this flag will run after restoring a snapshot:

```sql
update redeemer set fee = null;
delete from reward;
delete from epoch_stake;
delete from ada_pots;
delete from epoch_param;
```

**Ignore**

Maintains the ledger state, but doesn't use any of its data, except to load UTxO. To be used with
`tx_out` set to `"bootstrap"`

### Shelley

`shelley`

 * Type: `object`

Shelley Properties:

| Property          | Type      | Required |
| :---------------- | :-------- | :------- |
| [enable](#enable) | `boolean` | Optional |

#### Enable

Enables or disables data related to shelley: all certificates, withdrawals, and param
proposals. Does not control `epoch_stake` and `rewards`, For this check `ledger`.

`shelley.enable`

 * Type: `boolean`

### Multi Asset

`multi_asset`

 * Type: `object`

Multi Asset Properties:

| Property            | Type      | Required |
| :------------------ | :-------- | :------- |
| [enable](#enable-1) | `boolean` | Optional |

#### Enable

Enables or disables multi assets tables and entries.

`multi_asset.enable`

 * Type: `boolean`

### Metadata

`metadata`

 * Type: `object`

Metadata Properties:

| Property            | Type      | Required |
| :------------------ | :-------- | :------- |
| [enable](#enable-2) | `boolean` | Optional |
| [keys](#keys)       | `array`   | Optional |

#### Enable

Enables or disables the `tx_metadata` table.

`metadata.enable`

 * Type: `boolean`

#### Keys

If set, only keep metadata with the specified keys.

`metadata.keys`

 * Type: `integer[]`

### Plutus

`plutus`

 * Type: `object`

Plutus Properties:

| Property            | Type      | Required |
| :------------------ | :-------- | :------- |
| [enable](#enable-3) | `boolean` | Optional |

#### Enable

Enables or disables most tables and entries related to plutus and scripts.

`plutus.enable`

 * Type: `boolean`

### Governance

`governance`

 * Type: `string`

**enum**: The value of this property must be equal to one of the following values:

| Value      | Explanation                            |
| :--------- | :------------------------------------- |
| `"enable"` | Enable all data related to governance  |
| `"disable"`| Disable all data related to governance |

### Offchain Pool Data

`offchain_pool_data`

 * Type: `string`

**enum**: The value of this property must be equal to one of the following values:

| Value      | Explanation                               |
| :--------- | :---------------------------------------- |
| `"enable"` | Enables fetching offchain metadata.       |
| `"disable"`| Disables fetching pool offchain metadata. |
