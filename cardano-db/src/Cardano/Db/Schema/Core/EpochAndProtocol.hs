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
  syncStateEncoder, DbWord64, SyncState, DbInt65
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
  { epochId :: !EpochId
  , epochOutSum :: !Word128          -- sqltype=word128type
  , epochFees :: !DbLovelace         -- sqltype=lovelace
  , epochTxCount :: !Word64          -- sqltype=word31type
  , epochBlkCount :: !Word64         -- sqltype=word31type
  , epochNo :: !Word64               -- sqltype=word31type
  , epochStartTime :: !UTCTime       -- sqltype=timestamp
  , epochEndTime :: !UTCTime         -- sqltype=timestamp
  } deriving (Eq, Show, Generic)

epochDecoder :: D.Row Epoch
epochDecoder =
  Epoch
    <$> idDecoder EpochId -- epochId
    <*> D.column (D.nonNullable word128Decoder) -- epochOutSum
    <*> dbLovelaceDecoder -- epochFees
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochTxCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochBlkCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochNo
    <*> D.column (D.nonNullable D.timestamptz) -- epochStartTime
    <*> D.column (D.nonNullable D.timestamptz) -- epochEndTime

epochEncoder :: E.Params Epoch
epochEncoder =
  mconcat
    [ epochId >$< idEncoder getEpochId
    , epochOutSum >$< E.param (E.nonNullable word128Encoder)
    , epochFees >$< dbLovelaceEncoder
    , epochTxCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochBlkCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochStartTime >$< E.param (E.nonNullable E.timestamptz)
    , epochEndTime >$< E.param (E.nonNullable E.timestamptz)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_param
Description: Stores parameters relevant to each epoch, such as the number of slots per epoch or the block size limit.
-}
data EpochParam = EpochParam
  { epochParamId :: !EpochParamId
  , epochParamEpochNo :: !Word64               -- sqltype=word31type
  , epochParamMinFeeA :: !Word64               -- sqltype=word31type
  , epochParamMinFeeB :: !Word64               -- sqltype=word31type
  , epochParamMaxBlockSize :: !Word64          -- sqltype=word31type
  , epochParamMaxTxSize :: !Word64             -- sqltype=word31type
  , epochParamMaxBhSize :: !Word64             -- sqltype=word31type
  , epochParamKeyDeposit :: !DbLovelace        -- sqltype=lovelace
  , epochParamPoolDeposit :: !DbLovelace       -- sqltype=lovelace
  , epochParamMaxEpoch :: !Word64              -- sqltype=word31type
  , epochParamOptimalPoolCount :: !Word64      -- sqltype=word31type
  , epochParamInfluence :: !Double
  , epochParamMonetaryExpandRate :: !Double
  , epochParamTreasuryGrowthRate :: !Double
  , epochParamDecentralisation :: !Double
  , epochParamExtraEntropy :: !(Maybe ByteString) -- sqltype=hash32type
  , epochParamProtocolMajor :: !Word16         -- sqltype=word31type
  , epochParamProtocolMinor :: !Word16         -- sqltype=word31type
  , epochParamMinUtxoValue :: !DbLovelace      -- sqltype=lovelace
  , epochParamMinPoolCost :: !DbLovelace       -- sqltype=lovelace

  , epochParamNonce :: !(Maybe ByteString)       -- sqltype=hash32type

  , epochParamCoinsPerUtxoSize :: !(Maybe DbLovelace) -- sqltype=lovelace
  , epochParamCostModelId :: !(Maybe CostModelId)   -- noreference
  , epochParamPriceMem :: !(Maybe Double)
  , epochParamPriceStep :: !(Maybe Double)
  , epochParamMaxTxExMem :: !(Maybe DbWord64)    -- sqltype=word64type
  , epochParamMaxTxExSteps :: !(Maybe DbWord64)  -- sqltype=word64type
  , epochParamMaxBlockExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxBlockExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxValSize :: !(Maybe DbWord64)    -- sqltype=word64type
  , epochParamCollateralPercent :: !(Maybe Word16) -- sqltype=word31type
  , epochParamMaxCollateralInputs :: !(Maybe Word16) -- sqltype=word31type
  , epochParamPvtMotionNoConfidence :: !(Maybe Double)
  , epochParamPvtCommitteeNormal :: !(Maybe Double)
  , epochParamPvtCommitteeNoConfidence :: !(Maybe Double)
  , epochParamPvtHardForkInitiation :: !(Maybe Double)
  , epochParamPvtppSecurityGroup :: !(Maybe Double)

  , epochParamDvtMotionNoConfidence :: !(Maybe Double)
  , epochParamDvtCommitteeNormal :: !(Maybe Double)
  , epochParamDvtCommitteeNoConfidence :: !(Maybe Double)
  , epochParamDvtUpdateToConstitution :: !(Maybe Double)
  , epochParamDvtHardForkInitiation :: !(Maybe Double)
  , epochParamDvtPPNetworkGroup :: !(Maybe Double)
  , epochParamDvtPPEconomicGroup :: !(Maybe Double)
  , epochParamDvtPPTechnicalGroup :: !(Maybe Double)
  , epochParamDvtPPGovGroup :: !(Maybe Double)
  , epochParamDvtTreasuryWithdrawal :: !(Maybe Double)

  , epochParamCommitteeMinSize :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamCommitteeMaxTermLength :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamGovActionLifetime :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamGovActionDeposit :: !(Maybe DbWord64)  -- sqltype=word64type
  , epochParamDrepDeposit :: !(Maybe DbWord64)       -- sqltype=word64type
  , epochParamDrepActivity :: !(Maybe DbWord64)      -- sqltype=word64type
  , epochParamMinFeeRefScriptCostPerByte :: !(Maybe Double)
  , epochParamBlockId :: !BlockId                 -- noreference -- The first block where these parameters are valid.
  } deriving (Eq, Show, Generic)

epochParamDecoder :: D.Row EpochParam
epochParamDecoder =
  EpochParam
    <$> idDecoder EpochParamId -- epochParamId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamEpochNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamMinFeeA
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamMinFeeB
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamMaxBlockSize
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamMaxTxSize
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamMaxBhSize
    <*> dbLovelaceDecoder -- epochParamKeyDeposit
    <*> dbLovelaceDecoder -- epochParamPoolDeposit
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamMaxEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamOptimalPoolCount
    <*> D.column (D.nonNullable D.float8) -- epochParamInfluence
    <*> D.column (D.nonNullable D.float8) -- epochParamMonetaryExpandRate
    <*> D.column (D.nonNullable D.float8) -- epochParamTreasuryGrowthRate
    <*> D.column (D.nonNullable D.float8) -- epochParamDecentralisation
    <*> D.column (D.nullable D.bytea) -- epochParamExtraEntropy
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- epochParamProtocolMajor
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- epochParamProtocolMinor
    <*> dbLovelaceDecoder -- epochParamMinUtxoValue
    <*> dbLovelaceDecoder -- epochParamMinPoolCost
    <*> D.column (D.nullable D.bytea) -- epochParamNonce
    <*> D.column (D.nullable $ DbLovelace . fromIntegral <$> D.int8) -- epochParamCoinsPerUtxoSize
    <*> maybeIdDecoder CostModelId -- epochParamCostModelId
    <*> D.column (D.nullable D.float8) -- epochParamPriceMem
    <*> D.column (D.nullable D.float8) -- epochParamPriceStep
    <*> maybeDbWord64Decoder -- epochParamMaxTxExMem
    <*> maybeDbWord64Decoder -- epochParamMaxTxExSteps
    <*> maybeDbWord64Decoder -- epochParamMaxBlockExMem
    <*> maybeDbWord64Decoder -- epochParamMaxBlockExSteps
    <*> maybeDbWord64Decoder -- epochParamMaxValSize
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- epochParamCollateralPercent
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- epochParamMaxCollateralInputs
    <*> D.column (D.nullable D.float8) -- epochParamPvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParamPvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- epochParamPvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParamPvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- epochParamPvtppSecurityGroup
    <*> D.column (D.nullable D.float8) -- epochParamDvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParamDvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- epochParamDvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- epochParamDvtUpdateToConstitution
    <*> D.column (D.nullable D.float8) -- epochParamDvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- epochParamDvtPPNetworkGroup
    <*> D.column (D.nullable D.float8) -- epochParamDvtPPEconomicGroup
    <*> D.column (D.nullable D.float8) -- epochParamDvtPPTechnicalGroup
    <*> D.column (D.nullable D.float8) -- epochParamDvtPPGovGroup
    <*> D.column (D.nullable D.float8) -- epochParamDvtTreasuryWithdrawal
    <*> maybeDbWord64Decoder -- epochParamCommitteeMinSize
    <*> maybeDbWord64Decoder -- epochParamCommitteeMaxTermLength
    <*> maybeDbWord64Decoder -- epochParamGovActionLifetime
    <*> maybeDbWord64Decoder -- epochParamGovActionDeposit
    <*> maybeDbWord64Decoder -- epochParamDrepDeposit
    <*> maybeDbWord64Decoder -- epochParamDrepActivity
    <*> D.column (D.nullable D.float8) -- epochParamMinFeeRefScriptCostPerByte
    <*> idDecoder BlockId -- epochParamBlockId

epochParamEncoder :: E.Params EpochParam
epochParamEncoder =
  mconcat
    [ epochParamId >$< idEncoder getEpochParamId
    , epochParamEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamMinFeeA >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamMinFeeB >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamMaxBlockSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamMaxTxSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamMaxBhSize >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamKeyDeposit >$< dbLovelaceEncoder
    , epochParamPoolDeposit >$< dbLovelaceEncoder
    , epochParamMaxEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamOptimalPoolCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochParamInfluence >$< E.param (E.nonNullable E.float8)
    , epochParamMonetaryExpandRate >$< E.param (E.nonNullable E.float8)
    , epochParamTreasuryGrowthRate >$< E.param (E.nonNullable E.float8)
    , epochParamDecentralisation >$< E.param (E.nonNullable E.float8)
    , epochParamExtraEntropy >$< E.param (E.nullable E.bytea)
    , epochParamProtocolMajor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , epochParamProtocolMinor >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , epochParamMinUtxoValue >$< dbLovelaceEncoder
    , epochParamMinPoolCost >$< dbLovelaceEncoder
    , epochParamNonce >$< E.param (E.nullable E.bytea)
    , epochParamCoinsPerUtxoSize >$< E.param (E.nullable $ fromIntegral . unDbLovelace >$< E.int8)
    , epochParamCostModelId >$< maybeIdEncoder getCostModelId
    , epochParamPriceMem >$< E.param (E.nullable E.float8)
    , epochParamPriceStep >$< E.param (E.nullable E.float8)
    , epochParamMaxTxExMem >$< maybeDbWord64Encoder
    , epochParamMaxTxExSteps >$< maybeDbWord64Encoder
    , epochParamMaxBlockExMem >$< maybeDbWord64Encoder
    , epochParamMaxBlockExSteps >$< maybeDbWord64Encoder
    , epochParamMaxValSize >$< maybeDbWord64Encoder
    , epochParamCollateralPercent >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , epochParamMaxCollateralInputs >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , epochParamPvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , epochParamPvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , epochParamPvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , epochParamPvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , epochParamPvtppSecurityGroup >$< E.param (E.nullable E.float8)
    , epochParamDvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , epochParamDvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , epochParamDvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , epochParamDvtUpdateToConstitution >$< E.param (E.nullable E.float8)
    , epochParamDvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , epochParamDvtPPNetworkGroup >$< E.param (E.nullable E.float8)
    , epochParamDvtPPEconomicGroup >$< E.param (E.nullable E.float8)
    , epochParamDvtPPTechnicalGroup >$< E.param (E.nullable E.float8)
    , epochParamDvtPPGovGroup >$< E.param (E.nullable E.float8)
    , epochParamDvtTreasuryWithdrawal >$< E.param (E.nullable E.float8)
    , epochParamCommitteeMinSize >$< maybeDbWord64Encoder
    , epochParamCommitteeMaxTermLength >$< maybeDbWord64Encoder
    , epochParamGovActionLifetime >$< maybeDbWord64Encoder
    , epochParamGovActionDeposit >$< maybeDbWord64Encoder
    , epochParamDrepDeposit >$< maybeDbWord64Encoder
    , epochParamDrepActivity >$< maybeDbWord64Encoder
    , epochParamMinFeeRefScriptCostPerByte >$< E.param (E.nullable E.float8)
    , epochParamBlockId >$< idEncoder getBlockId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_state
Description: Contains the state of the blockchain at the end of each epoch, including the committee, constitution, and no-confidence votes.
-}
data EpochState = EpochState
  { epochStateId :: !EpochStateId
  , epochStateCommitteeId :: !(Maybe CommitteeId)         -- noreference
  , epochStateNoConfidenceId :: !(Maybe GovActionProposalId) -- noreference
  , epochStateConstitutionId :: !(Maybe ConstitutionId)   -- noreference
  , epochStateEpochNo :: !Word64                        -- sqltype=word31type
  } deriving (Eq, Show, Generic)

epochStateDecoder :: D.Row EpochState
epochStateDecoder =
  EpochState
    <$> idDecoder EpochStateId -- epochStateId
    <*> maybeIdDecoder CommitteeId -- epochStateCommitteeId
    <*> maybeIdDecoder GovActionProposalId -- epochStateNoConfidenceId
    <*> maybeIdDecoder ConstitutionId -- epochStateConstitutionId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStateEpochNo

epochStateEncoder :: E.Params EpochState
epochStateEncoder =
  mconcat
    [ epochStateId >$< idEncoder getEpochStateId
    , epochStateCommitteeId >$< maybeIdEncoder getCommitteeId
    , epochStateNoConfidenceId >$< maybeIdEncoder getGovActionProposalId
    , epochStateConstitutionId >$< maybeIdEncoder getConstitutionId
    , epochStateEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_sync_time
Description: Tracks synchronization times for epochs, ensuring nodes are in consensus regarding the current state.
-}
data EpochSyncTime = EpochSyncTime
  { epochSyncTimeId :: !EpochSyncTimeId
  , epochSyncTimeNo :: !Word64         -- sqltype=word31type
  , epochSyncTimeSeconds :: !Word64    -- sqltype=word63type
  , epochSyncTimeState :: !SyncState   -- sqltype=syncstatetype
  } deriving (Show, Eq, Generic)
-- UniqueEpochSyncTime no

epochSyncTimeDecoder :: D.Row EpochSyncTime
epochSyncTimeDecoder =
  EpochSyncTime
    <$> idDecoder EpochSyncTimeId -- epochSyncTimeId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochSyncTimeNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochSyncTimeSeconds
    <*> D.column (D.nonNullable syncStateDecoder) -- epochSyncTimeState

epochSyncTimeEncoder :: E.Params EpochSyncTime
epochSyncTimeEncoder =
  mconcat
    [ epochSyncTimeId >$< idEncoder getEpochSyncTimeId
    , epochSyncTimeNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochSyncTimeSeconds >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochSyncTimeState >$< E.param (E.nonNullable syncStateEncoder)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: ada_pots
Description: A table with all the different types of total balances.
  This is only populated for the Shelley and later eras, and only on epoch boundaries.
  The treasury and rewards fields will be correct for the whole epoch, but all other
  fields change block by block.
-}
data AdaPots = AdaPots
  { adaPotsId :: !AdaPotsId
  , adaPotsSlotNo :: !Word64         -- sqltype=word63type
  , adaPotsEpochNo :: !Word64        -- sqltype=word31type
  , adaPotsTreasury :: !DbLovelace   -- sqltype=lovelace
  , adaPotsReserves :: !DbLovelace   -- sqltype=lovelace
  , adaPotsRewards :: !DbLovelace    -- sqltype=lovelace
  , adaPotsUtxo :: !DbLovelace       -- sqltype=lovelace
  , adaPotsDepositsStake :: !DbLovelace -- sqltype=lovelace
  , adaPotsDepositsDrep :: !DbLovelace -- sqltype=lovelace
  , adaPotsDepositsProposal :: !DbLovelace -- sqltype=lovelace
  , adaPotsFees :: !DbLovelace       -- sqltype=lovelace
  , adaPotsBlockId :: !BlockId       -- noreference
  } deriving (Eq)

adaPotsDecoder :: D.Row AdaPots
adaPotsDecoder =
  AdaPots
    <$> idDecoder AdaPotsId -- adaPotsId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- adaPotsSlotNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- adaPotsEpochNo
    <*> dbLovelaceDecoder -- adaPotsTreasury
    <*> dbLovelaceDecoder -- adaPotsReserves
    <*> dbLovelaceDecoder -- adaPotsRewards
    <*> dbLovelaceDecoder -- adaPotsUtxo
    <*> dbLovelaceDecoder -- adaPotsDepositsStake
    <*> dbLovelaceDecoder -- adaPotsDepositsDrep
    <*> dbLovelaceDecoder -- adaPotsDepositsProposal
    <*> dbLovelaceDecoder -- adaPotsFees
    <*> idDecoder BlockId -- adaPotsBlockId

adaPotsEncoder :: E.Params AdaPots
adaPotsEncoder =
  mconcat
    [ adaPotsId >$< idEncoder getAdaPotsId
    , adaPotsSlotNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , adaPotsEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , adaPotsTreasury >$< dbLovelaceEncoder
    , adaPotsReserves >$< dbLovelaceEncoder
    , adaPotsRewards >$< dbLovelaceEncoder
    , adaPotsUtxo >$< dbLovelaceEncoder
    , adaPotsDepositsStake >$< dbLovelaceEncoder
    , adaPotsDepositsDrep >$< dbLovelaceEncoder
    , adaPotsDepositsProposal >$< dbLovelaceEncoder
    , adaPotsFees >$< dbLovelaceEncoder
    , adaPotsBlockId >$< idEncoder getBlockId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: pot_transfer
Description: Records transfers between different pots (e.g., from the rewards pot to the treasury pot).
-}
data PotTransfer = PotTransfer
  { potTransferId :: !PotTransferId
  , potTransferCertIndex :: !Word16
  , potTransferTreasury :: !DbInt65    -- sqltype=int65type
  , potTransferReserves :: !DbInt65    -- sqltype=int65type
  , potTransferTxId :: !TxId           -- noreference
  } deriving (Show, Eq, Generic)

potTransferDecoder :: D.Row PotTransfer
potTransferDecoder =
  PotTransfer
    <$> idDecoder PotTransferId -- potTransferId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- potTransferCertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- potTransferTreasury
    <*> D.column (D.nonNullable dbInt65Decoder) -- potTransferReserves
    <*> idDecoder TxId -- potTransferTxId

potTransferEncoder :: E.Params PotTransfer
potTransferEncoder =
  mconcat
    [ potTransferId >$< idEncoder getPotTransferId
    , potTransferCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , potTransferTreasury >$< E.param (E.nonNullable dbInt65Encoder)
    , potTransferReserves >$< E.param (E.nonNullable dbInt65Encoder)
    , potTransferTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: treasury
Description: Holds funds allocated to the treasury, which can be used for network upgrades or other community initiatives.
-}
data Treasury = Treasury
  { treasuryId :: !TreasuryId
  , treasuryAddrId :: !StakeAddressId -- noreference
  , treasuryCertIndex :: !Word16
  , treasuryAmount :: !DbInt65        -- sqltype=int65type
  , treasuryTxId :: !TxId             -- noreference
  } deriving (Show, Eq, Generic)

treasuryDecoder :: D.Row Treasury
treasuryDecoder =
  Treasury
    <$> idDecoder TreasuryId -- treasuryId
    <*> idDecoder StakeAddressId -- treasuryAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- treasuryCertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- treasuryAmount
    <*> idDecoder TxId -- treasuryTxId

treasuryEncoder :: E.Params Treasury
treasuryEncoder =
  mconcat
    [ treasuryId >$< idEncoder getTreasuryId
    , treasuryAddrId >$< idEncoder getStakeAddressId
    , treasuryCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , treasuryAmount >$< E.param (E.nonNullable dbInt65Encoder)
    , treasuryTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reserve
Description: Stores reserves set aside by the protocol to stabilize the cryptocurrency's value or fund future activities.
-}
data Reserve = Reserve
  { reserveId :: !ReserveId
  , reserveAddrId :: !StakeAddressId  -- noreference
  , reserveCertIndex :: !Word16
  , reserveAmount :: !DbInt65         -- sqltype=int65type
  , reserveTxId :: !TxId              -- noreference
  } deriving (Show, Eq, Generic)

reserveDecoder :: D.Row Reserve
reserveDecoder =
  Reserve
    <$> idDecoder ReserveId -- reserveId
    <*> idDecoder StakeAddressId -- reserveAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- reserveCertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- reserveAmount
    <*> idDecoder TxId -- reserveTxId

reserveEncoder :: E.Params Reserve
reserveEncoder =
  mconcat
    [ reserveId >$< idEncoder getReserveId
    , reserveAddrId >$< idEncoder getStakeAddressId
    , reserveCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , reserveAmount >$< E.param (E.nonNullable dbInt65Encoder)
    , reserveTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: cost_model
Description: Defines the cost model used for estimating transaction fees, ensuring efficient resource allocation on the network.
-}
data CostModel = CostModel
  { costModelId :: !CostModelId
  , costModelHash :: !ByteString    -- sqltype=hash32type
  , costModelCosts :: !Text         -- sqltype=jsonb
  } deriving (Eq, Show, Generic)
-- uniqueCostModel  hash

costModelDecoder :: D.Row CostModel
costModelDecoder =
  CostModel
    <$> idDecoder CostModelId -- costModelId
    <*> D.column (D.nonNullable D.bytea) -- costModelHash
    <*> D.column (D.nonNullable D.text) -- costModelCosts

costModelEncoder :: E.Params CostModel
costModelEncoder =
  mconcat
    [ costModelId >$< idEncoder getCostModelId
    , costModelHash >$< E.param (E.nonNullable E.bytea)
    , costModelCosts >$< E.param (E.nonNullable E.text)
    ]
