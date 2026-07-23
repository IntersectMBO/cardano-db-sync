# An Ideal API for Consensus and the Ledger

This is written from the point of view of a consumer (ie `cardano-db-sync`) of data from the
consensus and ledger layers. It describes the problems I, as the main dev on `db-sync`, see
in what we have now and then gives top level details of how I think consensus and ledger
should provide to the consumer like `db-sync`.

### Problems with What we Have Now

Consensus (along with the networking code) and ledger-specs are developed in two separate
Git repositories and neither has a well thought out or evolved API. Instead they simply expose
their internals. The different era's (Shelley, Allegra, Mary etc) all have their own data
types which are unified using type families. Unfortunately, with type families, changes in the
library can cause particularly obtuse error messages in client code. However useful type
families might be for developing the code for the different eras, having to deal with the
different types, even when unified using type families, only makes things more difficult for
clients like `db-sync`.

In order to populate it's database `db-sync` currently requires:

* A `LocalChainSync` client to get blocks from the blockchain.
* A ledger state to get information that is not recorded on chain (rewards, stake distribution
  etc).
* A ledger state query to get block information that is not on the blockchain (block time stamp,
  epoch number, slot within epoch etc).
* Data obtained as an aggregation query on data already in the database (sum of tx inputs,
  deposit value in a transaction etc).

For the case of database aggregation queries, it is not a problem for things like populating the
epoch table, but is a significant performance hit when calculating things like the deposit amount
for every transaction.

It should also be noted that because `db-sync` has to insert data into PostgreSQL, it will likely
be the first Cardano component to hit issues where its performance cannot keep up with a new
block arriving every 20 seconds. Anything that can reduce the amount of computation `db-sync`
needs to do improves its performance.

To summarise, the problems with the current approach:

* There are four different mechanisms to get all the information needed by the database.
* `db-sync` needs to maintain a copy of ledger state that is identical to the copy of the
  ledger state in the `node`. With `db-sync`, that means applying a block to an existing ledger
  state is done twice; once in the `node` and once in `db-sync`. The amount of code required to
  maintain ledger state is significant and basically duplicates code in `consensus`.
* Some data that goes into the database (eg the `deposit` field of the `tx` table) must be
  calculated using a database query. This needs to be done for every transaction in every
  block and is not a cheap operation.


### What Data is Needed?

The data needed by `db-sync` but not actually part of the blockchain (not necessarily an
exhaustive list):

* UTC time stamp for each block (currently calculated with a local state query).
* Epoch number and slot number within an epoch (currently calculated with a local state query).
* The rewards for each epoch (extracted from the ledger state).
* The stake distribution for each epoch (extracted from the ledger state).
* Sum of the transaction input amounts (this is needed to calculate the deposit which currently
  requires a relatively complex database query).
* Refunds of pool deposits after a pool is de-registered (currently not available).
* Rewards (tagged as either good or orphaned) so that `db-sync` does not have to emulate what
  `ledger-specs` does (currently not available).

### An Ideal API for a `db-sync`-like Consumer

The ideal API for a `db-sync` like consumer would not require the consumer to maintain its
own ledger state. Instead, the API would provide two things:

* Enhanced or annotated blocks (these are not blocks as they would appear on the blockchain),
  with addition information like a UTC time stamp, current era, epoch number, slot within
  an epoch etc. These annotated blocks are era independent.
* Enhanced/annotated blocks would not contain the blockchain version of blocks, but an enhanced
  block with things like the deposit value for each transaction. Like the annotated blocks,
  annotated transactions would also be era independent.
* Ledger events notifying of things that are difficult to obtain just by looking at the current
  block. This would include things like:

  ```
  data LedgerEvent
    = LedgerNewEra !Era
    | LedgerNewEpoch !EpochNo
    | LedgerRewards !EpochNo !Rewards
    | LedgerStakeDist !EpochNo !StakeDist
    | LedgerBlock !AnnotatedBlock
  ````

  Rewards and stake distribution could be calculated incrementally by the ledger and the partial
  results could be passed to `db-sync` as they become available.

With regards to rewards, there are currently two sources; rewards from minting blocks and
instantaneous rewards. Currently these are two separate data structures, but are only distributed
at epoch boundaries. In this new API, it would make sense to merge these two, with each separate
reward being annotated by its source (eg pool owner, pool member, instantaneous treasury and
instantaneous reserve payment).

There would then be something like the `LocalChainSync` protocol that passes `LedgerEvent`
objects over the connection rather than the current blockchain version of the blocks.

It is important to note that the `cardano-wallet` *also* maintains its own copy of ledger state and
*also* uses the local state query mechanism. Providing this new API would mean that both `db-sync`
and `cardano-wallet` can remove large chunks of relatively complex code (and probably tests) to
replace it with the new `LedgerEvent` date from the node.

Similarly, both the `cardano-wallet` and `db-sync` already have an era independent block type
(for `db-sync` this is Shelley/Allegra/Mary/Alonzo only) with conversion functions to convert
from the era specific block/tx types to the generic block/tx types. Having `ledger-specs`
provide this generic block type, reduces the amount of code `cardano-wallet` and `db-sync` needs
to maintain.

### Potential Problems Implementing the New API

On startup of `db-sync` it needs to negotiate with the `node` on the most recent valid block.
In the worst case, `db-sync` will need to sync from genesis or sync from some point for which
the `node` does not have the required ledger state. This means that `node` will need to
reconstruct the required ledger state.


### Annotated Data Structures

The idea here is that a single block type captures the super set of Byron, Shelley, Allegra,
Mary, Alonzo and and future era. Fields that are not valid for all eras can be encoded as
`Maybe`s or lists, so that fields that are not valid would be `Nothing` or `[]`.

There are two main data structures required, `AnnotatedBlock` and `AnnotatedTx` which can be
sketched out as follows:

```
data AnnotatedBlock = AnnotatedBlock
  { abEra :: !Era
  , abEpochNo :: !EpochNo
  , abSlotNo :: !SlotNo
  , abEpockSlot :: !Word   	    -- The slot within the epoch (starts at 0 for first slot of each epoch
  , abTimeStamp :: !UTCTime     -- The slot number converted to UTCTime
  , abEpochSize :: !EpochSize   -- Number of slots in current epoch
  -- All fields in the superset of all block types
  , abTxs :: [AnnotatedTx]
  }

data AnnotatedTx = AnnotatedTx
  { atInputSum :: !Coin         -- Sum of the tx inputs
  , atOutSum :: !Coin           -- Sum of the tx outputs
  , txFees :: !Coin
  -- All fields in the superset of all tx types
  }
```
