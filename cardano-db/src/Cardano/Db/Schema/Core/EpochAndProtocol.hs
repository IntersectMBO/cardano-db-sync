{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.EpochAndProtocol where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Ids
import Cardano.Db.Types (
  DbLovelace(..),
  dbInt65Decoder,
  dbInt65Encoder,
  dbLovelaceEncoder,
  maybeDbWord64Encoder,
  maybeDbWord64Decoder,
  dbLovelaceDecoder,
  word128Decoder,
  word128Encoder,
  syncStateDecoder,
  syncStateEncoder, DbWord64, SyncState, DbInt65, HasDbInfo (..)
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word16, Word64)
import Data.Functor.Contravariant
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E

-----------------------------------------------------------------------------------------------------------------------------------
-- EPOCH AND PROTOCOL PARAMETER
-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch
Description: The Epoch table is an aggregation of data in the 'Block' table, but is kept in this form
  because having it as a 'VIEW' is incredibly slow and inefficient.
  The 'outsum' type in the PostgreSQL world is 'bigint >= 0' so it will error out if an
  overflow (sum of tx outputs in an epoch) is detected. 'maxBound :: !Int` is big enough to
  hold 204 times the total Lovelace distribution. The chance of that much being transacted
  in a single epoch is relatively low.
-}
data Epoch = Epoch
  { epoch_Id :: !EpochId
  , epoch_OutSum :: !Word128          -- sqltype=word128type
  , epoch_Fees :: !DbLovelace         -- sqltype=lovelace
  , epoch_TxCount :: !Word64          -- sqltype=word31type
  , epoch_BlkCount :: !Word64         -- sqltype=word31type
  , epoch_No :: !Word64               -- sqltype=word31type
  , epoch_StartTime :: !UTCTime       -- sqltype=timestamp
  , epoch_EndTime :: !UTCTime         -- sqltype=timestamp
  } deriving (Eq, Show, Generic)

instance HasDbInfo Epoch

epochDecoder :: D.Row Epoch
epochDecoder =
  Epoch
    <$> idDecoder EpochId -- epochId
    <*> D.column (D.nonNullable word128Decoder) -- epoch_OutSum
    <*> dbLovelaceDecoder -- epoch_Fees
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epoch_TxCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epoch_BlkCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epoch_No
    <*> D.column (D.nonNullable D.timestamptz) -- epoch_StartTime
    <*> D.column (D.nonNullable D.timestamptz) -- epoch_EndTime

epochEncoder :: E.Params Epoch
epochEncoder =
  mconcat
    [ epoch_Id >$< idEncoder getEpochId
    , epoch_OutSum >$< E.param (E.nonNullable word128Encoder)
    , epoch_Fees >$< dbLovelaceEncoder
    , epoch_TxCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epoch_BlkCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epoch_No >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epoch_StartTime >$< E.param (E.nonNullable E.timestamptz)
    , epoch_EndTime >$< E.param (E.nonNullable E.timestamptz)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_param
Description: Stores parameters relevant to each epoch, such as the number of slots per epoch or the block size limit.
-}
data EpochParam = EpochParam
  { epochParam_Id :: !EpochParamId
  , epochParam_EpochNo :: !Word64               -- sqltype=word31type
  , epochParam_MinFeeA :: !Word64               -- sqltype=word31type
  , epochParam_MinFeeB :: !Word64               -- sqltype=word31type
  , epochParam_MaxBlockSize :: !Word64          -- sqltype=word31type
  , epochParam_MaxTxSize :: !Word64             -- sqltype=word31type
  , epochParam_MaxBhSize :: !Word64             -- sqltype=word31type
  , epochParam_KeyDeposit :: !DbLovelace        -- sqltype=lovelace
  , epochParam_PoolDeposit :: !DbLovelace       -- sqltype=lovelace
  , epochParam_MaxEpoch :: !Word64              -- sqltype=word31type
  , epochParam_OptimalPoolCount :: !Word64      -- sqltype=word31type
  , epochParam_Influence :: !Double
  , epochParam_MonetaryExpandRate :: !Double
  , epochParam_TreasuryGrowthRate :: !Double
  , epochParam_Decentralisation :: !Double
  , epochParam_ProtocolMajor :: !Word16         -- sqltype=word31type
  , epochParam_ProtocolMinor :: !Word16         -- sqltype=word31type
  , epochParam_MinUtxoValue :: !DbLovelace      -- sqltype=lovelace
  , epochParam_MinPoolCost :: !DbLovelace       -- sqltype=lovelace

  , epochParam_Nonce :: !(Maybe ByteString)       -- sqltype=hash32type

  , epochParam_CoinsPerUtxoSize :: !(Maybe DbLovelace) -- sqltype=lovelace
  , epochParam_CostModelId :: !(Maybe CostModelId)   -- noreference
  , epochParam_PriceMem :: !(Maybe Double)
  , epochParam_PriceStep :: !(Maybe Double)
  , epochParam_MaxTxExMem :: !(Maybe DbWord64)    -- sqltype=word64type
  , epochParam_MaxTxExSteps :: !(Maybe DbWord64)  -- sqltype=word64type
  , epochParam_MaxBlockExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParam_MaxBlockExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParam_MaxValSize :: !(Maybe DbWord64)    -- sqltype=word64type
  , epochParam_CollateralPercent :: !(Maybe Word16) -- sqltype=word31type
  , epochParam_MaxCollateralInputs :: !(Maybe Word16) -- sqltype=word31type
  , epochParam_BlockId :: !BlockId                 -- noreference -- The first block where these parameters are valid.
  , epochParam_ExtraEntropy :: !(Maybe ByteString) -- sqltype=hash32type
  , epochParam_PvtMotionNoConfidence :: !(Maybe Double)
  , epochParam_PvtCommitteeNormal :: !(Maybe Double)
  , epochParam_PvtCommitteeNoConfidence :: !(Maybe Double)
  , epochParam_PvtHardForkInitiation :: !(Maybe Double)
  , epochParam_PvtppSecurityGroup :: !(Maybe Double)

  , epochParam_DvtMotionNoConfidence :: !(Maybe Double)
  , epochParam_DvtCommitteeNormal :: !(Maybe Double)
  , epochParam_DvtCommitteeNoConfidence :: !(Maybe Double)
  , epochParam_DvtUpdateToConstitution :: !(Maybe Double)
  , epochParam_DvtHardForkInitiation :: !(Maybe Double)
  , epochParam_DvtPPNetworkGroup :: !(Maybe Double)
  , epochParam_DvtPPEconomicGroup :: !(Maybe Double)
  , epochParam_DvtPPTechnicalGroup :: !(Maybe Double)
  , epochParam_DvtPPGovGroup :: !(Maybe Double)
  , epochParam_DvtTreasuryWithdrawal :: !(Maybe Double)

  , epochParam_CommitteeMinSize :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParam_CommitteeMaxTermLength :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParam_GovActionLifetime :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParam_GovActionDeposit :: !(Maybe DbWord64)  -- sqltype=word64type
  , epochParam_DrepDeposit :: !(Maybe DbWord64)       -- sqltype=word64type
  , epochParam_DrepActivity :: !(Maybe DbWord64)      -- sqltype=word64type
  , epochParam_MinFeeRefScriptCostPerByte :: !(Maybe Double)
  } deriving (Eq, Show, Generic)

instance HasDbInfo EpochParam

epochParamDecoder :: D.Row EpochParam
epochParamDecoder =
  EpochParam
    <$> idDecoder EpochParamId -- epochParam_Id
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_EpochNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_MinFeeA
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_MinFeeB
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_MaxBlockSize
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_MaxTxSize
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_MaxBhSize
    <*> dbLovelaceDecoder -- epochParam_KeyDeposit
    <*> dbLovelaceDecoder -- epochParam_PoolDeposit
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_MaxEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParam_OptimalPoolCount
    <*> D.column (D.nonNullable D.float8) -- epochParam_Influence
    <*> D.column (D.nonNullable D.float8) -- epochParam_MonetaryExpandRate
    <*> D.column (D.nonNullable D.float8) -- epochParam_TreasuryGrowthRate
    <*> D.column (D.nonNullable D.float8) -- epochParam_Decentralisation
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- epochParam_ProtocolMajor
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- epochParam_ProtocolMinor
    <*> dbLovelaceDecoder -- epochParam_MinUtxoValue
    <*> dbLovelaceDecoder -- epochParam_MinPoolCost
    <*> D.column (D.nullable D.bytea) -- epochParam_Nonce
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- epochParam_CoinsPerUtxoSize
    <*> maybeIdDecoder CostModelId -- epochParam_CostModelId
    <*> D.column (D.nullable D.float8) -- epochParam_PriceMem
    <*> D.column (D.nullable D.float8) -- epochParam_PriceStep
    <*> maybeDbWord64Decoder -- epochParam_MaxTxExMem
    <*> maybeDbWord64Decoder -- epochParam_MaxTxExSteps
    <*> maybeDbWord64Decoder -- epochParam_MaxBlockExMem
    <*> maybeDbWord64Decoder -- epochParam_MaxBlockExSteps
    <*> maybeDbWord64Decoder -- epochParam_MaxValSize
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- epochParam_CollateralPercent
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- epochParam_MaxCollateralInputs
    <*> idDecoder BlockId -- epochParam_BlockId
    <*> D.column (D.nullable D.bytea) -- epochParam_ExtraEntropy
    <*> D.column (D.nullable D.float8) -- epochParam_PvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParam_PvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- epochParam_PvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParam_PvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- epochParam_PvtppSecurityGroup
    <*> D.column (D.nullable D.float8) -- epochParam_DvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParam_DvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- epochParam_DvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParam_DvtUpdateToConstitution
    <*> D.column (D.nullable D.float8) -- epochParam_DvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- epochParam_DvtPPNetworkGroup
    <*> D.column (D.nullable D.float8) -- epochParam_DvtPPEconomicGroup
    <*> D.column (D.nullable D.float8) -- epochParam_DvtPPTechnicalGroup
    <*> D.column (D.nullable D.float8) -- epochParam_DvtPPGovGroup
    <*> D.column (D.nullable D.float8) -- epochParam_DvtTreasuryWithdrawal
    <*> maybeDbWord64Decoder -- epochParam_CommitteeMinSize
    <*> maybeDbWord64Decoder -- epochParam_CommitteeMaxTermLength
    <*> maybeDbWord64Decoder -- epochParam_GovActionLifetime
    <*> maybeDbWord64Decoder -- epochParam_GovActionDeposit
    <*> maybeDbWord64Decoder -- epochParam_DrepDeposit
    <*> maybeDbWord64Decoder -- epochParam_DrepActivity
    <*> D.column (D.nullable D.float8) -- epochParam_MinFeeRefScriptCostPerByte

epochParamEncoder :: E.Params EpochParam
epochParamEncoder =
  mconcat
    [ epochParam_Id >$< idEncoder getEpochParamId
    , epochParam_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_MinFeeA >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_MinFeeB >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_MaxBlockSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_MaxTxSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_MaxBhSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_KeyDeposit >$< dbLovelaceEncoder
    , epochParam_PoolDeposit >$< dbLovelaceEncoder
    , epochParam_MaxEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_OptimalPoolCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParam_Influence >$< E.param (E.nonNullable E.float8)
    , epochParam_MonetaryExpandRate >$< E.param (E.nonNullable E.float8)
    , epochParam_TreasuryGrowthRate >$< E.param (E.nonNullable E.float8)
    , epochParam_Decentralisation >$< E.param (E.nonNullable E.float8)
    , epochParam_ProtocolMajor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , epochParam_ProtocolMinor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , epochParam_MinUtxoValue >$< dbLovelaceEncoder
    , epochParam_MinPoolCost >$< dbLovelaceEncoder
    , epochParam_Nonce >$< E.param (E.nullable E.bytea)
    , epochParam_CoinsPerUtxoSize >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , epochParam_CostModelId >$< maybeIdEncoder getCostModelId
    , epochParam_PriceMem >$< E.param (E.nullable E.float8)
    , epochParam_PriceStep >$< E.param (E.nullable E.float8)
    , epochParam_MaxTxExMem >$< maybeDbWord64Encoder
    , epochParam_MaxTxExSteps >$< maybeDbWord64Encoder
    , epochParam_MaxBlockExMem >$< maybeDbWord64Encoder
    , epochParam_MaxBlockExSteps >$< maybeDbWord64Encoder
    , epochParam_MaxValSize >$< maybeDbWord64Encoder
    , epochParam_CollateralPercent >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , epochParam_MaxCollateralInputs >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , epochParam_BlockId >$< idEncoder getBlockId
    , epochParam_ExtraEntropy >$< E.param (E.nullable E.bytea)
    , epochParam_PvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , epochParam_PvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , epochParam_PvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , epochParam_PvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , epochParam_PvtppSecurityGroup >$< E.param (E.nullable E.float8)
    , epochParam_DvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , epochParam_DvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , epochParam_DvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , epochParam_DvtUpdateToConstitution >$< E.param (E.nullable E.float8)
    , epochParam_DvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , epochParam_DvtPPNetworkGroup >$< E.param (E.nullable E.float8)
    , epochParam_DvtPPEconomicGroup >$< E.param (E.nullable E.float8)
    , epochParam_DvtPPTechnicalGroup >$< E.param (E.nullable E.float8)
    , epochParam_DvtPPGovGroup >$< E.param (E.nullable E.float8)
    , epochParam_DvtTreasuryWithdrawal >$< E.param (E.nullable E.float8)
    , epochParam_CommitteeMinSize >$< maybeDbWord64Encoder
    , epochParam_CommitteeMaxTermLength >$< maybeDbWord64Encoder
    , epochParam_GovActionLifetime >$< maybeDbWord64Encoder
    , epochParam_GovActionDeposit >$< maybeDbWord64Encoder
    , epochParam_DrepDeposit >$< maybeDbWord64Encoder
    , epochParam_DrepActivity >$< maybeDbWord64Encoder
    , epochParam_MinFeeRefScriptCostPerByte >$< E.param (E.nullable E.float8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_state
Description: Contains the state of the blockchain at the end of each epoch, including the committee, constitution, and no-confidence votes.
-}
data EpochState = EpochState
  { epochState_Id :: !EpochStateId
  , epochState_CommitteeId :: !(Maybe CommitteeId)         -- noreference
  , epochState_NoConfidenceId :: !(Maybe GovActionProposalId) -- noreference
  , epochState_ConstitutionId :: !(Maybe ConstitutionId)   -- noreference
  , epochState_EpochNo :: !Word64                        -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance HasDbInfo EpochState

epochStateDecoder :: D.Row EpochState
epochStateDecoder =
  EpochState
    <$> idDecoder EpochStateId -- epochState_Id
    <*> maybeIdDecoder CommitteeId -- epochState_CommitteeId
    <*> maybeIdDecoder GovActionProposalId -- epochState_NoConfidenceId
    <*> maybeIdDecoder ConstitutionId -- epochState_ConstitutionId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochState_EpochNo

epochStateEncoder :: E.Params EpochState
epochStateEncoder =
  mconcat
    [ epochState_Id >$< idEncoder getEpochStateId
    , epochState_CommitteeId >$< maybeIdEncoder getCommitteeId
    , epochState_NoConfidenceId >$< maybeIdEncoder getGovActionProposalId
    , epochState_ConstitutionId >$< maybeIdEncoder getConstitutionId
    , epochState_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_sync_time
Description: Tracks synchronization times for epochs, ensuring nodes are in consensus regarding the current state.
-}
data EpochSyncTime = EpochSyncTime
  { epochSyncTime_Id :: !EpochSyncTimeId
  , epochSyncTime_No :: !Word64         -- sqltype=word31type
  , epochSyncTime_Seconds :: !Word64    -- sqltype=word63type
  , epochSyncTime_State :: !SyncState   -- sqltype=syncstatetype
  } deriving (Show, Eq, Generic)
-- UniqueEpochSyncTime no

instance HasDbInfo EpochSyncTime

epochSyncTimeDecoder :: D.Row EpochSyncTime
epochSyncTimeDecoder =
  EpochSyncTime
    <$> idDecoder EpochSyncTimeId -- epochSyncTime_Id
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochSyncTime_No
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochSyncTime_Seconds
    <*> D.column (D.nonNullable syncStateDecoder) -- epochSyncTime_State

epochSyncTimeEncoder :: E.Params EpochSyncTime
epochSyncTimeEncoder =
  mconcat
    [ epochSyncTime_Id >$< idEncoder getEpochSyncTimeId
    , epochSyncTime_No >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochSyncTime_Seconds >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochSyncTime_State >$< E.param (E.nonNullable syncStateEncoder)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: ada_pots
Description: A table with all the different types of total balances.
  This is only populated for the Shelley and later eras, and only on epoch boundaries.
  The treasury_ and rewards fields will be correct for the whole epoch, but all other
  fields change block by block.
-}
data AdaPots = AdaPots
  { adaPots_Id :: !AdaPotsId
  , adaPots_SlotNo :: !Word64         -- sqltype=word63type
  , adaPots_EpochNo :: !Word64        -- sqltype=word31type
  , adaPots_Treasury :: !DbLovelace   -- sqltype=lovelace
  , adaPots_Reserves :: !DbLovelace   -- sqltype=lovelace
  , adaPots_Rewards :: !DbLovelace    -- sqltype=lovelace
  , adaPots_Utxo :: !DbLovelace       -- sqltype=lovelace
  , adaPots_DepositsStake :: !DbLovelace -- sqltype=lovelace
  , adaPots_Fees :: !DbLovelace       -- sqltype=lovelace
  , adaPots_BlockId :: !BlockId       -- noreference
  , adaPots_DepositsDrep :: !DbLovelace -- sqltype=lovelace
  , adaPots_DepositsProposal :: !DbLovelace -- sqltype=lovelace
  } deriving (Show, Eq, Generic)

instance HasDbInfo AdaPots

adaPotsDecoder :: D.Row AdaPots
adaPotsDecoder =
  AdaPots
    <$> idDecoder AdaPotsId -- adaPots_Id
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- adaPots_SlotNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- adaPots_EpochNo
    <*> dbLovelaceDecoder -- adaPots_Treasury
    <*> dbLovelaceDecoder -- adaPots_Reserves
    <*> dbLovelaceDecoder -- adaPots_Rewards
    <*> dbLovelaceDecoder -- adaPots_Utxo
    <*> dbLovelaceDecoder -- adaPots_DepositsStake
    <*> dbLovelaceDecoder -- adaPots_Fees
    <*> idDecoder BlockId -- adaPots_BlockId
    <*> dbLovelaceDecoder -- adaPots_DepositsDrep
    <*> dbLovelaceDecoder -- adaPots_DepositsProposal

adaPotsEncoder :: E.Params AdaPots
adaPotsEncoder =
  mconcat
    [ adaPots_Id >$< idEncoder getAdaPotsId
    , adaPots_SlotNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , adaPots_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , adaPots_Treasury >$< dbLovelaceEncoder
    , adaPots_Reserves >$< dbLovelaceEncoder
    , adaPots_Rewards >$< dbLovelaceEncoder
    , adaPots_Utxo >$< dbLovelaceEncoder
    , adaPots_DepositsStake >$< dbLovelaceEncoder
    , adaPots_Fees >$< dbLovelaceEncoder
    , adaPots_BlockId >$< idEncoder getBlockId
    , adaPots_DepositsDrep >$< dbLovelaceEncoder
    , adaPots_DepositsProposal >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pot_transfer
Description: Records transfers between different pots (e.g., from the rewards pot to the treasury pot).
-}
data PotTransfer = PotTransfer
  { potTransfer_Id :: !PotTransferId
  , potTransfer_CertIndex :: !Word16
  , potTransfer_Treasury :: !DbInt65    -- sqltype=int65type
  , potTransfer_Reserves :: !DbInt65    -- sqltype=int65type
  , potTransfer_TxId :: !TxId           -- noreference
  } deriving (Show, Eq, Generic)

instance HasDbInfo PotTransfer

potTransferDecoder :: D.Row PotTransfer
potTransferDecoder =
  PotTransfer
    <$> idDecoder PotTransferId -- potTransfer_Id
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- potTransfer_CertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- potTransfer_Treasury
    <*> D.column (D.nonNullable dbInt65Decoder) -- potTransfer_Reserves
    <*> idDecoder TxId -- potTransfer_TxId

potTransferEncoder :: E.Params PotTransfer
potTransferEncoder =
  mconcat
    [ potTransfer_Id >$< idEncoder getPotTransferId
    , potTransfer_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , potTransfer_Treasury >$< E.param (E.nonNullable dbInt65Encoder)
    , potTransfer_Reserves >$< E.param (E.nonNullable dbInt65Encoder)
    , potTransfer_TxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: treasury
Description: Holds funds allocated to the treasury, which can be used for network upgrades or other community initiatives.
-}
data Treasury = Treasury
  { treasury_Id :: !TreasuryId
  , treasury_AddrId :: !StakeAddressId -- noreference
  , treasury_CertIndex :: !Word16
  , treasury_Amount :: !DbInt65        -- sqltype=int65type
  , treasury_TxId :: !TxId             -- noreference
  } deriving (Show, Eq, Generic)

instance HasDbInfo Treasury

treasuryDecoder :: D.Row Treasury
treasuryDecoder =
  Treasury
    <$> idDecoder TreasuryId -- treasury_Id
    <*> idDecoder StakeAddressId -- treasury_AddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- treasury_CertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- treasury_Amount
    <*> idDecoder TxId -- treasury_TxId

treasuryEncoder :: E.Params Treasury
treasuryEncoder =
  mconcat
    [ treasury_Id >$< idEncoder getTreasuryId
    , treasury_AddrId >$< idEncoder getStakeAddressId
    , treasury_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , treasury_Amount >$< E.param (E.nonNullable dbInt65Encoder)
    , treasury_TxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reserve
Description: Stores reserves set aside by the protocol to stabilize the cryptocurrency's value or fund future activities.
-}
data Reserve = Reserve
  { reserve_Id :: !ReserveId
  , reserve_AddrId :: !StakeAddressId  -- noreference
  , reserve_CertIndex :: !Word16
  , reserve_Amount :: !DbInt65         -- sqltype=int65type
  , reserve_TxId :: !TxId              -- noreference
  } deriving (Show, Eq, Generic)

instance HasDbInfo Reserve

reserveDecoder :: D.Row Reserve
reserveDecoder =
  Reserve
    <$> idDecoder ReserveId -- reserve_Id
    <*> idDecoder StakeAddressId -- reserve_AddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- reserve_CertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- reserve_Amount
    <*> idDecoder TxId -- reserve_TxId

reserveEncoder :: E.Params Reserve
reserveEncoder =
  mconcat
    [ reserve_Id >$< idEncoder getReserveId
    , reserve_AddrId >$< idEncoder getStakeAddressId
    , reserve_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , reserve_Amount >$< E.param (E.nonNullable dbInt65Encoder)
    , reserve_TxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: cost_model
Description: Defines the cost model used for estimating transaction fees, ensuring efficient resource allocation on the network.
-}
data CostModel = CostModel
  { costModel_Id :: !CostModelId
  , costModel_Costs :: !Text         -- sqltype=jsonb
  , costModel_Hash :: !ByteString    -- sqltype=hash32type
  } deriving (Eq, Show, Generic)
-- uniqueCostModel  hash

instance HasDbInfo CostModel

costModelDecoder :: D.Row CostModel
costModelDecoder =
  CostModel
    <$> idDecoder CostModelId -- costModel_Id
    <*> D.column (D.nonNullable D.text) -- costModel_Costs
    <*> D.column (D.nonNullable D.bytea) -- costModel_Hash

costModelEncoder :: E.Params CostModel
costModelEncoder =
  mconcat
    [ costModel_Id >$< idEncoder getCostModelId
    , costModel_Costs >$< E.param (E.nonNullable E.text)
    , costModel_Hash >$< E.param (E.nonNullable E.bytea)
    ]
