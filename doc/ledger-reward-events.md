# Ledger Reward Events

This is an ambit claim of what data the ledger events should provide from the point of view of
`db-sync`.

## Prologue

In the context of `db-sync` and the `ledger`, the word "reward" is slightly misleading and should
not be narrowly defined as only being payments of staking rewards, but defined much more broadly as
any and all payments to a stake address. None of these payments are recorded on the block chain as
part of regular transactions.

Payments to stake addresses are made for one of following reasons:
* Staking rewards for being a member of a stake pool.
* Staking rewards for being an owner of a stake pool.
* MIR payments from the treasury.
* MIR payments from the reserves.
* Refunds of the stake pool deposit payment when a pool is de-registered.

Currently, the ledger provides the following reward related events:
* `LEDeltaReward`
* `LEMirTransfer`
* `LERetiredPools`
* `LETotalRewards`

These events are currently only used by `db-sync` but it is my understanding that in the future,
other programs will also start using these.


## The Desired Ledger Event Functionality

* Every event should carry an `EpochNo` field to make debugging and validation on the `db-sync` side
  easier.
* A single `LETotalRewards` event should be emitted for every epoch in the Shelley era and later,
  even if there are no rewards for that epoch (in which case the event reward map will be `mempty`).
* `LETotalRewards` should include all payments to stake addresses for a given epoch. That means
  all staking rewards (member and owner), all MIR payments and all stake pool deposit refunds.
* `LEDeltaReward` should only ever contain pool membership or pool ownership rewards.
* `LEDeltaReward` may contain rewards to stake addresses that have been de-registered.
* The sum of all `LEDeltaReward`, `LEMirTransfer` and `LERetiredPools` amounts for an epoch should
  always be the same as the sum of `LETotalRewards` event amounts for that epoch.
* `LETotalRewards` will not contain rewards to stake addresses that have been de-registered.
* The `LETotalRewards` must be the last reward related event for a given epoch to be emitted from
  the ledger.
* For the Shelley Era (ie after Byron and before Mary), for the `LETotalRewards` event, all staking
  addresses should only contain one staking reward amount (if an address receives both an ownership
  and a membership reward, the later will be dropped).
