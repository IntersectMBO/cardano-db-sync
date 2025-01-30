module Cardano.Db.Schema.BaseSchema where

-- deriving instance Eq (Unique EpochSyncTime)

-- schemaDocs :: ![EntityDef]
-- schemaDocs =
--   document entityDefs $ do
--     SchemaVersion --^ do
--       "The version of the database schema. Schema versioning is split into three stages as detailed\
--       \ below. This table should only ever have a single row."
--       SchemaVersionStageOne # "Set up PostgreSQL data types (using SQL 'DOMAIN' statements)."
--       SchemaVersionStageTwo # "Persistent generated migrations."
--       SchemaVersionStageThree # "Set up database views, indices etc."

--     PoolHash --^ do
--       "A table for every unique pool key hash.\
--       \ The existance of an entry doesn't mean the pool is registered or in fact that is was ever registered."
--       PoolHashHashRaw # "The raw bytes of the pool hash."
--       PoolHashView # "The Bech32 encoding of the pool hash."

--     SlotLeader --^ do
--       "Every unique slot leader (ie an entity that mines a block). It could be a pool or a leader defined in genesis."
--       SlotLeaderHash # "The hash of of the block producer identifier."
--       SlotLeaderPoolHashId # "If the slot leader is a pool, an index into the `PoolHash` table."
--       SlotLeaderDescription # "An auto-generated description of the slot leader."

--     Block --^ do
--       "A table for blocks on the chain."
--       BlockHash # "The hash identifier of the block."
--       BlockEpochNo # "The epoch number."
--       BlockSlotNo # "The slot number."
--       BlockEpochSlotNo # "The slot number within an epoch (resets to zero at the start of each epoch)."
--       BlockBlockNo # "The block number."
--       BlockPreviousId # "The Block table index of the previous block."
--       BlockSlotLeaderId # "The SlotLeader table index of the creator of this block."
--       BlockSize # "The block size (in bytes). Note, this size value is not expected to be the same as the sum of the tx sizes due to the fact that txs being stored in segwit format and oddities in the CBOR encoding."
--       BlockTime # "The block time (UTCTime)."
--       BlockTxCount # "The number of transactions in this block."
--       BlockProtoMajor # "The block's major protocol number."
--       BlockProtoMinor # "The block's major protocol number."
--       -- Shelley specific
--       BlockVrfKey # "The VRF key of the creator of this block."
--       BlockOpCert # "The hash of the operational certificate of the block producer."
--       BlockOpCertCounter # "The value of the counter used to produce the operational certificate."

--     Tx --^ do
--       "A table for transactions within a block on the chain."
--       TxHash # "The hash identifier of the transaction."
--       TxBlockId # "The Block table index of the block that contains this transaction."
--       TxBlockIndex # "The index of this transaction with the block (zero based)."
--       TxOutSum # "The sum of the transaction outputs (in Lovelace)."
--       TxFee # "The fees paid for this transaction."
--       TxDeposit # "Deposit (or deposit refund) in this transaction. Deposits are positive, refunds negative."
--       TxSize # "The size of the transaction in bytes."
--       TxInvalidBefore # "Transaction in invalid before this slot number."
--       TxInvalidHereafter # "Transaction in invalid at or after this slot number."
--       TxValidContract # "False if the contract is invalid. True if the contract is valid or there is no contract."
--       TxScriptSize # "The sum of the script sizes (in bytes) of scripts in the transaction."

--     TxCbor --^ do
--       "A table holding raw CBOR encoded transactions."
--       TxCborTxId # "The Tx table index of the transaction encoded in this table."
--       TxCborBytes # "CBOR encoded transaction."

--     ReverseIndex --^ do
--       "A table for reverse indexes for the minimum input output and multi asset output related with\
--       \ this block. New in v13.1"
--       ReverseIndexBlockId # "The Block table index related with these indexes"
--       ReverseIndexMinIds # "The Reverse indexes associated with this block, as Text separated by :"

--     StakeAddress --^ do
--       "A table of unique stake addresses. Can be an actual address or a script hash. \
--       \ The existance of an entry doesn't mean the address is registered or in fact that is was ever registered."
--       StakeAddressHashRaw # "The raw bytes of the stake address hash."
--       StakeAddressView # "The Bech32 encoded version of the stake address."
--       StakeAddressScriptHash # "The script hash, in case this address is locked by a script."

--     TxIn --^ do
--       "A table for transaction inputs."
--       TxInTxInId # "The Tx table index of the transaction that contains this transaction input."
--       TxInTxOutId # "The Tx table index of the transaction that contains the referenced transaction output."
--       TxInTxOutIndex # "The index within the transaction outputs."
--       TxInRedeemerId # "The Redeemer table index which is used to validate this input."

--     CollateralTxIn --^ do
--       "A table for transaction collateral inputs."
--       CollateralTxInTxInId # "The Tx table index of the transaction that contains this transaction input"
--       CollateralTxInTxOutId # "The Tx table index of the transaction that contains the referenced transaction output."
--       CollateralTxInTxOutIndex # "The index within the transaction outputs."

--     ReferenceTxIn --^ do
--       "A table for reference transaction inputs. New in v13."
--       ReferenceTxInTxInId # "The Tx table index of the transaction that contains this transaction input"
--       ReferenceTxInTxOutId # "The Tx table index of the transaction that contains the referenced output."
--       ReferenceTxInTxOutIndex # "The index within the transaction outputs."

--     Meta --^ do
--       "A table containing metadata about the chain. There will probably only ever be one row in this table."
--       MetaStartTime # "The start time of the network."
--       MetaNetworkName # "The network name."

--     Epoch --^ do
--       "Aggregation of data within an epoch."
--       EpochOutSum # "The sum of the transaction output values (in Lovelace) in this epoch."
--       EpochFees # "The sum of the fees (in Lovelace) in this epoch."
--       EpochTxCount # "The number of transactions in this epoch."
--       EpochBlkCount # "The number of blocks in this epoch."
--       EpochNo # "The epoch number."
--       EpochStartTime # "The epoch start time."
--       EpochEndTime # "The epoch end time."

--     AdaPots --^ do
--       "A table with all the different types of total balances (Shelley only).\n\
--       \The treasury and rewards fields will be correct for the whole epoch, but all other \
--       \fields change block by block."
--       AdaPotsSlotNo # "The slot number where this AdaPots snapshot was taken."
--       AdaPotsEpochNo # "The epoch number where this AdaPots snapshot was taken."
--       AdaPotsTreasury # "The amount (in Lovelace) in the treasury pot."
--       AdaPotsReserves # "The amount (in Lovelace) in the reserves pot."
--       AdaPotsRewards # "The amount (in Lovelace) in the rewards pot."
--       AdaPotsUtxo # "The amount (in Lovelace) in the UTxO set."
--       AdaPotsDepositsStake # "The amount (in Lovelace) in the obligation pot coming from stake key and pool deposits. Renamed from deposits in 13.3."
--       AdaPotsDepositsDrep # "The amount (in Lovelace) in the obligation pot coming from drep registrations deposits. New in 13.3."
--       AdaPotsDepositsProposal # "The amount (in Lovelace) in the obligation pot coming from governance proposal deposits. New in 13.3."
--       AdaPotsFees # "The amount (in Lovelace) in the fee pot."
--       AdaPotsBlockId # "The Block table index of the block for which this snapshot was taken."

--     PoolMetadataRef --^ do
--       "An on-chain reference to off-chain pool metadata."
--       PoolMetadataRefPoolId # "The PoolHash table index of the pool for this reference."
--       PoolMetadataRefUrl # "The URL for the location of the off-chain data."
--       PoolMetadataRefHash # "The expected hash for the off-chain data."
--       PoolMetadataRefRegisteredTxId # "The Tx table index of the transaction in which provided this metadata reference."

--     PoolUpdate --^ do
--       "An on-chain pool update."
--       PoolUpdateHashId # "The PoolHash table index of the pool this update refers to."
--       PoolUpdateCertIndex # "The index of this pool update within the certificates of this transaction."
--       PoolUpdateVrfKeyHash # "The hash of the pool's VRF key."
--       PoolUpdatePledge # "The amount (in Lovelace) the pool owner pledges to the pool."
--       PoolUpdateRewardAddrId # "The StakeAddress table index of this pool's rewards address. New in v13: Replaced reward_addr."
--       PoolUpdateActiveEpochNo # "The epoch number where this update becomes active."
--       PoolUpdateMetaId # "The PoolMetadataRef table index this pool update refers to."
--       PoolUpdateMargin # "The margin (as a percentage) this pool charges."
--       PoolUpdateFixedCost # "The fixed per epoch fee (in ADA) this pool charges."
--       PoolUpdateDeposit # "The deposit payed for this pool update. Null for reregistrations."
--       PoolUpdateRegisteredTxId # "The Tx table index of the transaction in which provided this pool update."

--     PoolOwner --^ do
--       "A table containing pool owners."
--       PoolOwnerAddrId # "The StakeAddress table index for the pool owner's stake address."
--       PoolOwnerPoolUpdateId # "The PoolUpdate table index for the pool. New in v13."

--     PoolRetire --^ do
--       "A table containing information about pools retiring."
--       PoolRetireHashId # "The PoolHash table index of the pool this retirement refers to."
--       PoolRetireCertIndex # "The index of this pool retirement within the certificates of this transaction."
--       PoolRetireAnnouncedTxId # "The Tx table index of the transaction where this pool retirement was announced."
--       PoolRetireRetiringEpoch # "The epoch where this pool retires."

--     PoolRelay --^ do
--       PoolRelayUpdateId # "The PoolUpdate table index this PoolRelay entry refers to."
--       PoolRelayIpv4 # "The IPv4 address of the relay (NULLable)."
--       PoolRelayIpv6 # "The IPv6 address of the relay (NULLable)."
--       PoolRelayDnsName # "The DNS name of the relay (NULLable)."
--       PoolRelayDnsSrvName # "The DNS service name of the relay (NULLable)."
--       PoolRelayPort # "The port number of relay (NULLable)."

--     StakeRegistration --^ do
--       "A table containing stake address registrations."
--       StakeRegistrationAddrId # "The StakeAddress table index for the stake address."
--       StakeRegistrationCertIndex # "The index of this stake registration within the certificates of this transaction."
--       StakeRegistrationEpochNo # "The epoch in which the registration took place."
--       StakeRegistrationTxId # "The Tx table index of the transaction where this stake address was registered."

--     StakeDeregistration --^ do
--       "A table containing stake address deregistrations."
--       StakeDeregistrationAddrId # "The StakeAddress table index for the stake address."
--       StakeDeregistrationCertIndex # "The index of this stake deregistration within the certificates of this transaction."
--       StakeDeregistrationEpochNo # "The epoch in which the deregistration took place."
--       StakeDeregistrationTxId # "The Tx table index of the transaction where this stake address was deregistered."
--       StakeDeregistrationRedeemerId # "The Redeemer table index that is related with this certificate."

--     Delegation --^ do
--       "A table containing delegations from a stake address to a stake pool."
--       DelegationAddrId # "The StakeAddress table index for the stake address."
--       DelegationCertIndex # "The index of this delegation within the certificates of this transaction."
--       DelegationPoolHashId # "The PoolHash table index for the pool being delegated to."
--       DelegationActiveEpochNo # "The epoch number where this delegation becomes active."
--       DelegationTxId # "The Tx table index of the transaction that contained this delegation."
--       DelegationSlotNo # "The slot number of the block that contained this delegation."
--       DelegationRedeemerId # "The Redeemer table index that is related with this certificate."

--     TxMetadata --^ do
--       "A table for metadata attached to a transaction."
--       TxMetadataKey # "The metadata key (a Word64/unsigned 64 bit number)."
--       TxMetadataJson # "The JSON payload if it can be decoded as JSON."
--       TxMetadataBytes # "The raw bytes of the payload."
--       TxMetadataTxId # "The Tx table index of the transaction where this metadata was included."

--     Reward --^ do
--       "A table for earned staking rewards. After 13.2 release it includes only 3 types of rewards: member, leader and refund, \
--       \ since the other 2 types have moved to a separate table instant_reward.\
--       \ The rewards are inserted incrementally and\
--       \ this procedure is finalised when the spendable epoch comes. Before the epoch comes, some entries\
--       \ may be missing. The `reward.id` field has been removed and it only appears on docs due to a bug."
--       RewardAddrId # "The StakeAddress table index for the stake address that earned the reward."
--       RewardType # "The type of the rewards"
--       RewardAmount # "The reward amount (in Lovelace)."
--       RewardEarnedEpoch
--         # "The epoch in which the reward was earned. For `pool` and `leader` rewards spendable in epoch `N`, this will be\
--           \ `N - 2`, `refund` N."
--       RewardSpendableEpoch # "The epoch in which the reward is actually distributed and can be spent."
--       RewardPoolId
--         # "The PoolHash table index for the pool the stake address was delegated to when\
--           \ the reward is earned or for the pool that there is a deposit refund."

--     RewardRest --^ do
--       "A table for rewards which are not correlated to a pool. It includes 3 types of rewards: reserves, treasury and proposal_refund.\
--       \ Instant rewards are depredated after Conway.\
--       \ The `reward.id` field has been removed and it only appears on docs due to a bug.\
--       \ New in 13.2"
--       RewardRestAddrId # "The StakeAddress table index for the stake address that earned the reward."
--       RewardRestType # "The type of the rewards."
--       RewardRestAmount # "The reward amount (in Lovelace)."
--       RewardRestEarnedEpoch
--         # "The epoch in which the reward was earned. For rewards spendable in epoch `N`, this will be\
--           \ `N - 1`."
--       RewardRestSpendableEpoch # "The epoch in which the reward is actually distributed and can be spent."

--     Withdrawal --^ do
--       "A table for withdrawals from a reward account."
--       WithdrawalAddrId # "The StakeAddress table index for the stake address for which the withdrawal is for."
--       WithdrawalAmount # "The withdrawal amount (in Lovelace)."
--       WithdrawalTxId # "The Tx table index for the transaction that contains this withdrawal."
--       WithdrawalRedeemerId # "The Redeemer table index that is related with this withdrawal."

--     EpochStake --^ do
--       "A table containing the epoch stake distribution for each epoch. This is inserted incrementally in the first blocks of the previous epoch.\
--       \ The stake distribution is extracted from the `set` snapshot of the ledger. See Shelley specs Sec. 11.2 for more details."
--       EpochStakeAddrId # "The StakeAddress table index for the stake address for this EpochStake entry."
--       EpochStakePoolId # "The PoolHash table index for the pool this entry is delegated to."
--       EpochStakeAmount # "The amount (in Lovelace) being staked."
--       EpochStakeEpochNo # "The epoch number."

--     EpochStakeProgress --^ do
--       "A table which shows when the epoch_stake for an epoch is complete"
--       EpochStakeProgressEpochNo # "The related epoch"
--       EpochStakeProgressCompleted # "True if completed. If not completed the entry won't exist or more rarely be False."

--     Treasury --^ do
--       "A table for payments from the treasury to a StakeAddress. Note: Before protocol version 5.0\
--       \ (Alonzo) if more than one payment was made to a stake address in a single epoch, only the\
--       \ last payment was kept and earlier ones removed. For protocol version 5.0 and later, they\
--       \ are summed and produce a single reward with type `treasury`."
--       TreasuryAddrId # "The StakeAddress table index for the stake address for this Treasury entry."
--       TreasuryCertIndex # "The index of this payment certificate within the certificates of this transaction."
--       TreasuryAmount # "The payment amount (in Lovelace)."
--       TreasuryTxId # "The Tx table index for the transaction that contains this payment."

--     Reserve --^ do
--       "A table for payments from the reserves to a StakeAddress. Note: Before protocol version 5.0\
--       \ (Alonzo) if more than one payment was made to a stake address in a single epoch, only the\
--       \ last payment was kept and earlier ones removed. For protocol version 5.0 and later, they\
--       \ are summed and produce a single reward with type `reserves`"
--       ReserveAddrId # "The StakeAddress table index for the stake address for this Treasury entry."
--       ReserveCertIndex # "The index of this payment certificate within the certificates of this transaction."
--       ReserveAmount # "The payment amount (in Lovelace)."
--       ReserveTxId # "The Tx table index for the transaction that contains this payment."

--     PotTransfer --^ do
--       "A table containing transfers between the reserves pot and the treasury pot."
--       PotTransferCertIndex # "The index of this transfer certificate within the certificates of this transaction."
--       PotTransferTreasury # "The amount (in Lovelace) the treasury balance changes by."
--       PotTransferReserves # "The amount (in Lovelace) the reserves balance changes by."
--       PotTransferTxId # "The Tx table index for the transaction that contains this transfer."

--     EpochSyncTime --^ do
--       "A table containing the time required to fully sync an epoch."
--       EpochSyncTimeNo # "The epoch number for this sync time."
--       EpochSyncTimeSeconds
--         # "The time (in seconds) required to sync this epoch (may be NULL for an epoch\
--           \ that was already partially synced when `db-sync` was started)."
--       EpochSyncTimeState # "The sync state when the sync time is recorded (either 'lagging' or 'following')."

--     MultiAsset --^ do
--       "A table containing all the unique policy/name pairs along with a CIP14 asset fingerprint"
--       MultiAssetPolicy # "The MultiAsset policy hash."
--       MultiAssetName # "The MultiAsset name."
--       MultiAssetFingerprint # "The CIP14 fingerprint for the MultiAsset."

--     MaTxMint --^ do
--       "A table containing Multi-Asset mint events."
--       MaTxMintIdent # "The MultiAsset table index specifying the asset."
--       MaTxMintQuantity # "The amount of the Multi Asset to mint (can be negative to \"burn\" assets)."
--       MaTxMintTxId # "The Tx table index for the transaction that contains this minting event."

--     Redeemer --^ do
--       "A table containing redeemers. A redeemer is provided for all items that are validated by a script."
--       RedeemerTxId # "The Tx table index that contains this redeemer."
--       RedeemerUnitMem # "The budget in Memory to run a script."
--       RedeemerUnitSteps # "The budget in Cpu steps to run a script."
--       RedeemerFee
--         # "The budget in fees to run a script. The fees depend on the ExUnits and the current prices.\
--           \ Is null when --disable-ledger is enabled. New in v13: became nullable."
--       RedeemerPurpose # "What kind pf validation this redeemer is used for. It can be one of 'spend', 'mint', 'cert', 'reward', `voting`, `proposing`"
--       RedeemerIndex # "The index of the redeemer pointer in the transaction."
--       RedeemerScriptHash # "The script hash this redeemer is used for."
--       RedeemerRedeemerDataId # "The data related to this redeemer. New in v13: renamed from datum_id."

--     Script --^ do
--       "A table containing scripts available, found in witnesses, inlined in outputs (reference outputs) or auxdata of transactions."
--       ScriptTxId # "The Tx table index for the transaction where this script first became available."
--       ScriptHash # "The Hash of the Script."
--       ScriptType # "The type of the script. This is currenttly either 'timelock' or 'plutus'."
--       ScriptJson # "JSON representation of the timelock script, null for other script types"
--       ScriptBytes # "CBOR encoded plutus script data, null for other script types"
--       ScriptSerialisedSize # "The size of the CBOR serialised script, if it is a Plutus script."

--     Datum --^ do
--       "A table containing Plutus Datum, found in witnesses or inlined in outputs"
--       DatumHash # "The Hash of the Datum"
--       DatumTxId # "The Tx table index for the transaction where this script first became available."
--       DatumValue # "The actual data in JSON format (detailed schema)"
--       DatumBytes # "The actual data in CBOR format"

--     RedeemerData --^ do
--       "A table containing Plutus Redeemer Data. These are always referenced by at least one redeemer. New in v13: split from datum table."
--       RedeemerDataHash # "The Hash of the Plutus Data"
--       RedeemerDataTxId # "The Tx table index for the transaction where this script first became available."
--       RedeemerDataValue # "The actual data in JSON format (detailed schema)"
--       RedeemerDataBytes # "The actual data in CBOR format"

--     ExtraKeyWitness --^ do
--       "A table containing transaction extra key witness hashes."
--       ExtraKeyWitnessHash # "The hash of the witness."
--       ExtraKeyWitnessTxId # "The id of the tx this witness belongs to."

--     ParamProposal --^ do
--       "A table containing block chain parameter change proposals."
--       ParamProposalEpochNo
--         # "The epoch for which this parameter proposal in intended to become active.\
--           \ Changed in 13.2-Conway to nullable is always null in Conway era."
--       ParamProposalKey
--         # "The hash of the crypto key used to sign this proposal.\
--           \ Changed in 13.2-Conway to nullable is always null in Conway era."
--       ParamProposalMinFeeA # "The 'a' parameter to calculate the minimum transaction fee."
--       ParamProposalMinFeeB # "The 'b' parameter to calculate the minimum transaction fee."
--       ParamProposalMaxBlockSize # "The maximum block size (in bytes)."
--       ParamProposalMaxTxSize # "The maximum transaction size (in bytes)."
--       ParamProposalMaxBhSize # "The maximum block header size (in bytes)."
--       ParamProposalKeyDeposit # "The amount (in Lovelace) require for a deposit to register a StakeAddress."
--       ParamProposalPoolDeposit # "The amount (in Lovelace) require for a deposit to register a stake pool."
--       ParamProposalMaxEpoch # "The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for."
--       ParamProposalOptimalPoolCount # "The optimal number of stake pools."
--       ParamProposalInfluence # "The influence of the pledge on a stake pool's probability on minting a block."
--       ParamProposalMonetaryExpandRate # "The monetary expansion rate."
--       ParamProposalTreasuryGrowthRate # "The treasury growth rate."
--       ParamProposalDecentralisation # "The decentralisation parameter (1 fully centralised, 0 fully decentralised)."
--       ParamProposalEntropy # "The 32 byte string of extra random-ness to be added into the protocol's entropy pool."
--       ParamProposalProtocolMajor # "The protocol major number."
--       ParamProposalProtocolMinor # "The protocol minor number."
--       ParamProposalMinUtxoValue # "The minimum value of a UTxO entry."
--       ParamProposalMinPoolCost # "The minimum pool cost."
--       ParamProposalCoinsPerUtxoSize # "For Alonzo this is the cost per UTxO word. For Babbage and later per UTxO byte. New in v13: Renamed from coins_per_utxo_word."
--       ParamProposalCostModelId # "The CostModel table index for the proposal."
--       ParamProposalPriceMem # "The per word cost of script memory usage."
--       ParamProposalPriceStep # "The cost of script execution step usage."
--       ParamProposalMaxTxExMem # "The maximum number of execution memory allowed to be used in a single transaction."
--       ParamProposalMaxTxExSteps # "The maximum number of execution steps allowed to be used in a single transaction."
--       ParamProposalMaxBlockExMem # "The maximum number of execution memory allowed to be used in a single block."
--       ParamProposalMaxBlockExSteps # "The maximum number of execution steps allowed to be used in a single block."
--       ParamProposalMaxValSize # "The maximum Val size."
--       ParamProposalCollateralPercent # "The percentage of the txfee which must be provided as collateral when including non-native scripts."
--       ParamProposalMaxCollateralInputs # "The maximum number of collateral inputs allowed in a transaction."
--       ParamProposalRegisteredTxId # "The Tx table index for the transaction that contains this parameter proposal."
--       ParamProposalPvtMotionNoConfidence # "Pool Voting threshold for motion of no-confidence. New in 13.2-Conway."
--       ParamProposalPvtCommitteeNormal # "Pool Voting threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       ParamProposalPvtCommitteeNoConfidence # "Pool Voting threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       ParamProposalPvtHardForkInitiation # "Pool Voting threshold for hard-fork initiation. New in 13.2-Conway."
--       ParamProposalDvtMotionNoConfidence # "DRep Vote threshold for motion of no-confidence. New in 13.2-Conway."
--       ParamProposalDvtCommitteeNormal # "DRep Vote threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       ParamProposalDvtCommitteeNoConfidence # "DRep Vote threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       ParamProposalDvtUpdateToConstitution # "DRep Vote threshold for update to the Constitution. New in 13.2-Conway."
--       ParamProposalDvtHardForkInitiation # "DRep Vote threshold for hard-fork initiation. New in 13.2-Conway."
--       ParamProposalDvtPPNetworkGroup # "DRep Vote threshold for protocol parameter changes, network group. New in 13.2-Conway."
--       ParamProposalDvtPPEconomicGroup # "DRep Vote threshold for protocol parameter changes, economic group. New in 13.2-Conway."
--       ParamProposalDvtPPTechnicalGroup # "DRep Vote threshold for protocol parameter changes, technical group. New in 13.2-Conway."
--       ParamProposalDvtPPGovGroup # "DRep Vote threshold for protocol parameter changes, governance group. New in 13.2-Conway."
--       ParamProposalDvtTreasuryWithdrawal # "DRep Vote threshold for treasury withdrawal. New in 13.2-Conway."
--       ParamProposalCommitteeMinSize # "Minimal constitutional committee size. New in 13.2-Conway."
--       ParamProposalCommitteeMaxTermLength # "Constitutional committee term limits. New in 13.2-Conway."
--       ParamProposalGovActionLifetime # "Governance action expiration. New in 13.2-Conway."
--       ParamProposalGovActionDeposit # "Governance action deposit. New in 13.2-Conway."
--       ParamProposalDrepDeposit # "DRep deposit amount. New in 13.2-Conway."
--       ParamProposalDrepActivity # "DRep activity period. New in 13.2-Conway."

--     EpochParam --^ do
--       "The accepted protocol parameters for an epoch."
--       EpochParamEpochNo # "The first epoch for which these parameters are valid."
--       EpochParamMinFeeA # "The 'a' parameter to calculate the minimum transaction fee."
--       EpochParamMinFeeB # "The 'b' parameter to calculate the minimum transaction fee."
--       EpochParamMaxBlockSize # "The maximum block size (in bytes)."
--       EpochParamMaxTxSize # "The maximum transaction size (in bytes)."
--       EpochParamMaxBhSize # "The maximum block header size (in bytes)."
--       EpochParamKeyDeposit # "The amount (in Lovelace) require for a deposit to register a StakeAddress."
--       EpochParamPoolDeposit # "The amount (in Lovelace) require for a deposit to register a stake pool."
--       EpochParamMaxEpoch # "The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for."
--       EpochParamOptimalPoolCount # "The optimal number of stake pools."
--       EpochParamInfluence # "The influence of the pledge on a stake pool's probability on minting a block."
--       EpochParamMonetaryExpandRate # "The monetary expansion rate."
--       EpochParamTreasuryGrowthRate # "The treasury growth rate."
--       EpochParamDecentralisation # "The decentralisation parameter (1 fully centralised, 0 fully decentralised)."
--       EpochParamExtraEntropy # "The 32 byte string of extra random-ness to be added into the protocol's entropy pool. New in v13: renamed from entopy."
--       EpochParamProtocolMajor # "The protocol major number."
--       EpochParamProtocolMinor # "The protocol minor number."
--       EpochParamMinUtxoValue # "The minimum value of a UTxO entry."
--       EpochParamMinPoolCost # "The minimum pool cost."
--       EpochParamNonce # "The nonce value for this epoch."
--       EpochParamCoinsPerUtxoSize # "For Alonzo this is the cost per UTxO word. For Babbage and later per UTxO byte. New in v13: Renamed from coins_per_utxo_word."
--       EpochParamCostModelId # "The CostModel table index for the params."
--       EpochParamPriceMem # "The per word cost of script memory usage."
--       EpochParamPriceStep # "The cost of script execution step usage."
--       EpochParamMaxTxExMem # "The maximum number of execution memory allowed to be used in a single transaction."
--       EpochParamMaxTxExSteps # "The maximum number of execution steps allowed to be used in a single transaction."
--       EpochParamMaxBlockExMem # "The maximum number of execution memory allowed to be used in a single block."
--       EpochParamMaxBlockExSteps # "The maximum number of execution steps allowed to be used in a single block."
--       EpochParamMaxValSize # "The maximum Val size."
--       EpochParamCollateralPercent # "The percentage of the txfee which must be provided as collateral when including non-native scripts."
--       EpochParamMaxCollateralInputs # "The maximum number of collateral inputs allowed in a transaction."
--       EpochParamBlockId # "The Block table index for the first block where these parameters are valid."
--       EpochParamPvtMotionNoConfidence # "Pool Voting threshold for motion of no-confidence. New in 13.2-Conway."
--       EpochParamPvtCommitteeNormal # "Pool Voting threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       EpochParamPvtCommitteeNoConfidence # "Pool Voting threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       EpochParamPvtHardForkInitiation # "Pool Voting threshold for hard-fork initiation. New in 13.2-Conway."
--       EpochParamDvtMotionNoConfidence # "DRep Vote threshold for motion of no-confidence. New in 13.2-Conway."
--       EpochParamDvtCommitteeNormal # "DRep Vote threshold for new committee/threshold (normal state). New in 13.2-Conway."
--       EpochParamDvtCommitteeNoConfidence # "DRep Vote threshold for new committee/threshold (state of no-confidence). New in 13.2-Conway."
--       EpochParamDvtUpdateToConstitution # "DRep Vote threshold for update to the Constitution. New in 13.2-Conway."
--       EpochParamDvtHardForkInitiation # "DRep Vote threshold for hard-fork initiation. New in 13.2-Conway."
--       EpochParamDvtPPNetworkGroup # "DRep Vote threshold for protocol parameter changes, network group. New in 13.2-Conway."
--       EpochParamDvtPPEconomicGroup # "DRep Vote threshold for protocol parameter changes, economic group. New in 13.2-Conway."
--       EpochParamDvtPPTechnicalGroup # "DRep Vote threshold for protocol parameter changes, technical group. New in 13.2-Conway."
--       EpochParamDvtPPGovGroup # "DRep Vote threshold for protocol parameter changes, governance group. New in 13.2-Conway."
--       EpochParamDvtTreasuryWithdrawal # "DRep Vote threshold for treasury withdrawal. New in 13.2-Conway."
--       EpochParamCommitteeMinSize # "Minimal constitutional committee size. New in 13.2-Conway."
--       EpochParamCommitteeMaxTermLength # "Constitutional committee term limits. New in 13.2-Conway."
--       EpochParamGovActionLifetime # "Governance action expiration. New in 13.2-Conway."
--       EpochParamGovActionDeposit # "Governance action deposit. New in 13.2-Conway."
--       EpochParamDrepDeposit # "DRep deposit amount. New in 13.2-Conway."
--       EpochParamDrepActivity # "DRep activity period. New in 13.2-Conway."

--     CostModel --^ do
--       "CostModel for EpochParam and ParamProposal."
--       CostModelHash # "The hash of cost model. It ensures uniqueness of entries. New in v13."
--       CostModelCosts # "The actual costs formatted as json."

--     PoolStat --^ do
--       "Stats per pool and per epoch."
--       PoolStatPoolHashId # "The pool_hash_id reference."
--       PoolStatEpochNo # "The epoch number."
--       PoolStatNumberOfBlocks # "Number of blocks created on the previous epoch."
--       PoolStatNumberOfDelegators # "Number of delegators in the mark snapshot."
--       PoolStatStake # "Total stake in the mark snapshot."
--       PoolStatVotingPower # "Voting power of the SPO."

--     EpochState --^ do
--       "Table with governance (and in the future other) stats per epoch."
--       EpochStateCommitteeId # "The reference to the current committee."
--       EpochStateNoConfidenceId # "The reference to the current gov_action_proposal of no confidence. TODO: This remains NULL."
--       EpochStateConstitutionId # "The reference to the current constitution. Should never be null."
--       EpochStateEpochNo # "The epoch in question."

--     ExtraMigrations --^ do
--       "Extra optional migrations. New in 13.2."
--       ExtraMigrationsDescription # "A description of the migration"

--     DrepHash --^ do
--       "A table for every unique drep key hash.\
--       \ The existance of an entry doesn't mean the DRep is registered.\
--       \ New in 13.2-Conway."
--       DrepHashRaw # "The raw bytes of the DRep."
--       DrepHashView # "The human readable encoding of the Drep."
--       DrepHashHasScript # "Flag which shows if this DRep credentials are a script hash"

--     CommitteeHash --^ do
--       "A table for all committee credentials hot or cold"
--       CommitteeHashRaw # "The key or script hash"
--       CommitteeHashHasScript # "Flag which shows if this credential is a script hash"

--     DelegationVote --^ do
--       "A table containing delegations from a stake address to a stake pool. New in 13.2-Conway."
--       DelegationVoteAddrId # "The StakeAddress table index for the stake address."
--       DelegationVoteCertIndex # "The index of this delegation within the certificates of this transaction."
--       DelegationVoteDrepHashId # "The DrepHash table index for the pool being delegated to."
--       DelegationVoteTxId # "The Tx table index of the transaction that contained this delegation."
--       DelegationVoteRedeemerId # "The Redeemer table index that is related with this certificate. TODO: can vote redeemers index these delegations?"

--     CommitteeRegistration --^ do
--       "A table for every committee hot key registration. New in 13.2-Conway."
--       CommitteeRegistrationTxId # "The Tx table index of the tx that includes this certificate."
--       CommitteeRegistrationCertIndex # "The index of this registration within the certificates of this transaction."
--       CommitteeRegistrationColdKeyId # "The reference to the registered cold key hash id"
--       CommitteeRegistrationHotKeyId # "The reference to the registered hot key hash id"

--     CommitteeDeRegistration --^ do
--       "A table for every committee key de-registration. New in 13.2-Conway."
--       CommitteeDeRegistrationTxId # "The Tx table index of the tx that includes this certificate."
--       CommitteeDeRegistrationCertIndex # "The index of this deregistration within the certificates of this transaction."
--       CommitteeDeRegistrationColdKeyId # "The reference to the the deregistered cold key hash id"
--       CommitteeDeRegistrationVotingAnchorId # "The Voting anchor reference id"

--     DrepRegistration --^ do
--       "A table for DRep registrations, deregistrations or updates. Registration have positive deposit values, deregistrations have negative and\
--       \ updates have null. Based on this distinction, for a specific DRep, getting the latest entry gives its registration state. New in 13.2-Conway."
--       DrepRegistrationTxId # "The Tx table index of the tx that includes this certificate."
--       DrepRegistrationCertIndex # "The index of this registration within the certificates of this transaction."
--       DrepRegistrationDeposit # "The deposits payed if this is an initial registration."
--       DrepRegistrationDrepHashId # "The Drep hash index of this registration."

--     VotingAnchor --^ do
--       "A table for every Anchor that appears on Governance Actions. These are pointers to offchain metadata. \
--       \ The tuple of url and hash is unique. New in 13.2-Conway."
--       VotingAnchorBlockId # "The Block table index of the tx that includes this anchor. This only exists to facilitate rollbacks"
--       VotingAnchorDataHash # "A hash of the contents of the metadata URL"
--       VotingAnchorUrl # "A URL to a JSON payload of metadata"
--       VotingAnchorType # "The type of the anchor. It can be gov_action, drep, other, vote, committee_dereg, constitution"

--     GovActionProposal --^ do
--       "A table for proposed GovActionProposal, aka ProposalProcedure, GovAction or GovProposal.\
--       \ This table may be referenced\
--       \ by TreasuryWithdrawal or NewCommittee. New in 13.2-Conway."
--       GovActionProposalTxId # "The Tx table index of the tx that includes this certificate."
--       GovActionProposalIndex # "The index of this proposal procedure within its transaction."
--       GovActionProposalPrevGovActionProposal # "The previous related GovActionProposal. This is null for "
--       GovActionProposalDeposit # "The deposit amount payed for this proposal."
--       GovActionProposalReturnAddress # "The StakeAddress index of the reward address to receive the deposit when it is repaid."
--       GovActionProposalVotingAnchorId # "The Anchor table index related to this proposal."
--       GovActionProposalType # "Can be one of ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, NewCommittee, NewConstitution, InfoAction"
--       GovActionProposalDescription # "A Text describing the content of this GovActionProposal in a readable way."
--       GovActionProposalParamProposal # "If this is a param proposal action, this has the index of the param_proposal table."
--       GovActionProposalRatifiedEpoch # "If not null, then this proposal has been ratified at the specfied epoch."
--       GovActionProposalEnactedEpoch # "If not null, then this proposal has been enacted at the specfied epoch."
--       GovActionProposalExpiredEpoch # "If not null, then this proposal has been expired at the specfied epoch."
--       GovActionProposalDroppedEpoch
--         # "If not null, then this proposal has been dropped at the specfied epoch. A proposal is dropped when it's \
--           \expired or enacted or when one of its dependencies is expired."
--       GovActionProposalExpiration # "Shows the epoch at which this governance action will expire."

--     TreasuryWithdrawal --^ do
--       "A table for all treasury withdrawals proposed on a GovActionProposal. New in 13.2-Conway."
--       TreasuryWithdrawalGovActionProposalId
--         # "The GovActionProposal table index for this withdrawal.\
--           \Multiple TreasuryWithdrawal may reference the same GovActionProposal."
--       TreasuryWithdrawalStakeAddressId # "The address that benefits from this withdrawal."
--       TreasuryWithdrawalAmount # "The amount for this withdrawl."

--     Committee --^ do
--       "A table for new committee proposed on a GovActionProposal. New in 13.2-Conway."
--       CommitteeGovActionProposalId # "The GovActionProposal table index for this new committee. This can be null for genesis committees."
--       CommitteeQuorumNumerator # "The proposed quorum nominator."
--       CommitteeQuorumDenominator # "The proposed quorum denominator."

--     CommitteeMember --^ do
--       "A table for members of the committee. A committee can have multiple members. New in 13.3-Conway."
--       CommitteeMemberCommitteeId # "The reference to the committee"
--       CommitteeMemberCommitteeHashId # "The reference to the committee hash"
--       CommitteeMemberExpirationEpoch # "The epoch this member expires"

--     Constitution --^ do
--       "A table for constitution attached to a GovActionProposal. New in 13.2-Conway."
--       ConstitutionGovActionProposalId # "The GovActionProposal table index for this constitution."
--       ConstitutionVotingAnchorId # "The ConstitutionVotingAnchor table index for this constitution."
--       ConstitutionScriptHash # "The Script Hash. It's associated script may not be already inserted in the script table."

--     VotingProcedure --^ do
--       "A table for voting procedures, aka GovVote. A Vote can be Yes No or Abstain. New in 13.2-Conway."
--       VotingProcedureTxId # "The Tx table index of the tx that includes this VotingProcedure."
--       VotingProcedureIndex # "The index of this VotingProcedure within this transaction."
--       VotingProcedureGovActionProposalId # "The index of the GovActionProposal that this vote targets."
--       VotingProcedureVoterRole # "The role of the voter. Can be one of ConstitutionalCommittee, DRep, SPO."
--       VotingProcedureCommitteeVoter # "A reference to the hot key committee hash entry that voted"
--       VotingProcedureDrepVoter # "A reference to the drep hash entry that voted"
--       VotingProcedurePoolVoter # "A reference to the pool hash entry that voted"
--       VotingProcedureVote # "The Vote. Can be one of Yes, No, Abstain."
--       VotingProcedureVotingAnchorId # "The VotingAnchor table index associated with this VotingProcedure."
--       VotingProcedureInvalid # "TODO: This is currently not implemented and always stays null. Not null if the vote is invalid."

--     OffChainVoteData --^ do
--       "The table with the offchain metadata related to Vote Anchors. It accepts metadata in a more lenient way than what's\
--       \ decribed in CIP-100. New in 13.2-Conway."
--       OffChainVoteDataVotingAnchorId # "The VotingAnchor table index this offchain data refers."
--       OffChainVoteDataHash # "The hash of the offchain data."
--       OffChainVoteDataLanguage # "The langauge described in the context of the metadata. Described in CIP-100. New in 13.3-Conway."
--       OffChainVoteDataJson # "The payload as JSON."
--       OffChainVoteDataBytes # "The raw bytes of the payload."
--       OffChainVoteDataWarning # "A warning that occured while validating the metadata."
--       OffChainVoteDataIsValid
--         # "False if the data is found invalid. db-sync leaves this field null \
--           \since it normally populates off_chain_vote_fetch_error for invalid data. \
--           \It can be used manually to mark some metadata invalid by clients."

--     OffChainVoteGovActionData --^ do
--       "The table with offchain metadata for Governance Actions. Implementes CIP-108. New in 13.3-Conway."
--       OffChainVoteGovActionDataOffChainVoteDataId # "The vote metadata table index this offchain data belongs to."
--       OffChainVoteGovActionDataTitle # "The title"
--       OffChainVoteGovActionDataAbstract # "The abstract"
--       OffChainVoteGovActionDataMotivation # "The motivation"
--       OffChainVoteGovActionDataRationale # "The rationale"

--     OffChainVoteDrepData --^ do
--       "The table with offchain metadata for Drep Registrations. Implementes CIP-119. New in 13.3-Conway."
--       OffChainVoteDrepDataOffChainVoteDataId # "The vote metadata table index this offchain data belongs to."
--       OffChainVoteDrepDataPaymentAddress # "The payment address"
--       OffChainVoteDrepDataGivenName # "The name. This is the only mandatory field"
--       OffChainVoteDrepDataObjectives # "The objectives"
--       OffChainVoteDrepDataMotivations # "The motivations"
--       OffChainVoteDrepDataQualifications # "The qualifications"

--     OffChainVoteAuthor --^ do
--       "The table with offchain metadata authors, as decribed in CIP-100. New in 13.3-Conway."
--       OffChainVoteAuthorOffChainVoteDataId # "The OffChainVoteData table index this offchain data refers."
--       OffChainVoteAuthorName # "The name of the author."
--       OffChainVoteAuthorWitnessAlgorithm # "The witness algorithm used by the author."
--       OffChainVoteAuthorPublicKey # "The public key used by the author."
--       OffChainVoteAuthorSignature # "The signature of the author."
--       OffChainVoteAuthorWarning # "A warning related to verifying this metadata."

--     OffChainVoteReference --^ do
--       "The table with offchain metadata references, as decribed in CIP-100. New in 13.3-Conway."
--       OffChainVoteReferenceOffChainVoteDataId # "The OffChainVoteData table index this entry refers."
--       OffChainVoteReferenceLabel # "The label of this vote reference."
--       OffChainVoteReferenceUri # "The uri of this vote reference."
--       OffChainVoteReferenceHashDigest
--         # "The hash digest of this vote reference, as described in CIP-108. \
--           \This only appears for governance action metadata."
--       OffChainVoteReferenceHashAlgorithm
--         # "The hash algorithm of this vote reference, as described in CIP-108. \
--           \This only appears for governance action metadata."

--     OffChainVoteExternalUpdate --^ do
--       "The table with offchain metadata external updates, as decribed in CIP-100. New in 13.3-Conway."
--       OffChainVoteExternalUpdateOffChainVoteDataId # "The OffChainVoteData table index this entry refers."
--       OffChainVoteExternalUpdateTitle # "The title of this external update."
--       OffChainVoteExternalUpdateUri # "The uri of this external update."

--     OffChainVoteFetchError --^ do
--       "Errors while fetching or validating offchain Voting Anchor metadata. New in 13.2-Conway."
--       OffChainVoteFetchErrorVotingAnchorId # "The VotingAnchor table index this offchain fetch error refers."
--       OffChainVoteFetchErrorFetchError # "The text of the error."
--       OffChainVoteFetchErrorRetryCount # "The number of retries."

--     DrepDistr --^ do
--       "The table for the distribution of voting power per DRep per. Currently this has a single entry per DRep\
--       \ and doesn't show every delegator. This may change. New in 13.2-Conway."
--       DrepDistrHashId # "The DrepHash table index that this distribution entry has information about."
--       DrepDistrAmount # "The total amount of voting power this DRep is delegated."
--       DrepDistrEpochNo # "The epoch no this distribution is about."
--       DrepDistrActiveUntil # "The epoch until which this drep is active. TODO: This currently remains null always. "

--     OffChainPoolData --^ do
--       "The pool offchain (ie not on chain) for a stake pool."
--       OffChainPoolDataPoolId # "The PoolHash table index for the pool this offchain data refers."
--       OffChainPoolDataTickerName # "The pool's ticker name (as many as 5 characters)."
--       OffChainPoolDataHash # "The hash of the offchain data."
--       OffChainPoolDataJson # "The payload as JSON."
--       OffChainPoolDataBytes # "The raw bytes of the payload."
--       OffChainPoolDataPmrId # "The PoolMetadataRef table index for this offchain data."

--     OffChainPoolFetchError --^ do
--       "A table containing pool offchain data fetch errors."
--       OffChainPoolFetchErrorPoolId # "The PoolHash table index for the pool this offchain fetch error refers."
--       OffChainPoolFetchErrorFetchTime # "The UTC time stamp of the error."
--       OffChainPoolFetchErrorPmrId # "The PoolMetadataRef table index for this offchain data."
--       OffChainPoolFetchErrorFetchError # "The text of the error."
--       OffChainPoolFetchErrorRetryCount # "The number of retries."

--     ReservedPoolTicker --^ do
--       "A table containing a managed list of reserved ticker names."
--       ReservedPoolTickerName # "The ticker name."
--       ReservedPoolTickerPoolHash # "The hash of the pool that owns this ticker."

--     DelistedPool --^ do
--       "A table containing pools that have been delisted."
--       DelistedPoolHashRaw # "The pool hash"
