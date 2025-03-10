{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.EpochAndProtocol where

import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Types (
  DbInt65,
  DbLovelace (..),
  DbWord64,
  SyncState,
  dbInt65Decoder,
  dbInt65Encoder,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  maybeDbWord64Decoder,
  maybeDbWord64Encoder,
  syncStateDecoder,
  syncStateEncoder,
  word128Decoder,
  word128Encoder,
 )
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

import Cardano.Db.Statement.Function.Core (manyEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Contravariant.Extras (contrazip4)
import Hasql.Decoders as D
import Hasql.Encoders as E

-----------------------------------------------------------------------------------------------------------------------------------
-- EPOCH AND PROTOCOL PARAMETER
-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.
-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: epoch
-- Description: The Epoch table is an aggregation of data in the 'Block' table, but is kept in this form
--   because having it as a 'VIEW' is incredibly slow and inefficient.
--   The 'outsum' type in the PostgreSQL world is 'bigint >= 0' so it will error out if an
--   overflow (sum of tx outputs in an epoch) is detected. 'maxBound :: !Int` is big enough to
--   hold 204 times the total Lovelace distribution. The chance of that much being transacted
--   in a single epoch is relatively low.
data Epoch = Epoch
  { epochOutSum :: !Word128 -- sqltype=word128type
  , epochFees :: !DbLovelace -- sqltype=lovelace
  , epochTxCount :: !Word64 -- sqltype=word31type
  , epochBlkCount :: !Word64 -- sqltype=word31type
  , epochNo :: !Word64 -- sqltype=word31type
  , epochStartTime :: !UTCTime -- sqltype=timestamp
  , epochEndTime :: !UTCTime -- sqltype=timestamp
  }
  deriving (Eq, Show, Generic)

type instance Key Epoch = EpochId
instance DbInfo Epoch where
  uniqueFields _ = ["no"]

entityEpochDecoder :: D.Row (Entity Epoch)
entityEpochDecoder =
  Entity
    <$> idDecoder EpochId
    <*> epochDecoder

epochDecoder :: D.Row Epoch
epochDecoder =
  Epoch
    <$> D.column (D.nonNullable word128Decoder) -- epochOutSum
    <*> dbLovelaceDecoder -- epochFees
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochTxCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochBlkCount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochNo
    <*> D.column (D.nonNullable D.timestamptz) -- epochStartTime
    <*> D.column (D.nonNullable D.timestamptz) -- epochEndTime

entityEpochEncoder :: E.Params (Entity Epoch)
entityEpochEncoder =
  mconcat
    [ entityKey >$< idEncoder getEpochId
    , entityVal >$< epochEncoder
    ]

epochEncoder :: E.Params Epoch
epochEncoder =
  mconcat
    [ epochOutSum >$< E.param (E.nonNullable word128Encoder)
    , epochFees >$< dbLovelaceEncoder
    , epochTxCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochBlkCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochStartTime >$< E.param (E.nonNullable E.timestamptz)
    , epochEndTime >$< E.param (E.nonNullable E.timestamptz)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: epochparam
-- Description: Stores parameters relevant to each epoch, such as the number of slots per epoch or the block size limit.
data EpochParam = EpochParam
  { epochParamEpochNo :: !Word64 -- sqltype=word31type
  , epochParamMinFeeA :: !Word64 -- sqltype=word31type
  , epochParamMinFeeB :: !Word64 -- sqltype=word31type
  , epochParamMaxBlockSize :: !Word64 -- sqltype=word31type
  , epochParamMaxTxSize :: !Word64 -- sqltype=word31type
  , epochParamMaxBhSize :: !Word64 -- sqltype=word31type
  , epochParamKeyDeposit :: !DbLovelace -- sqltype=lovelace
  , epochParamPoolDeposit :: !DbLovelace -- sqltype=lovelace
  , epochParamMaxEpoch :: !Word64 -- sqltype=word31type
  , epochParamOptimalPoolCount :: !Word64 -- sqltype=word31type
  , epochParamInfluence :: !Double
  , epochParamMonetaryExpandRate :: !Double
  , epochParamTreasuryGrowthRate :: !Double
  , epochParamDecentralisation :: !Double
  , epochParamProtocolMajor :: !Word16 -- sqltype=word31type
  , epochParamProtocolMinor :: !Word16 -- sqltype=word31type
  , epochParamMinUtxoValue :: !DbLovelace -- sqltype=lovelace
  , epochParamMinPoolCost :: !DbLovelace -- sqltype=lovelace
  , epochParamNonce :: !(Maybe ByteString) -- sqltype=hash32type
  , epochParamCoinsPerUtxoSize :: !(Maybe DbLovelace) -- sqltype=lovelace
  , epochParamCostModelId :: !(Maybe CostModelId) -- noreference
  , epochParamPriceMem :: !(Maybe Double)
  , epochParamPriceStep :: !(Maybe Double)
  , epochParamMaxTxExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxTxExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxBlockExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxBlockExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMaxValSize :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamCollateralPercent :: !(Maybe Word16) -- sqltype=word31type
  , epochParamMaxCollateralInputs :: !(Maybe Word16) -- sqltype=word31type
  , epochParamBlockId :: !BlockId -- noreference -- The first block where these parameters are valid.
  , epochParamExtraEntropy :: !(Maybe ByteString) -- sqltype=hash32type
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
  , epochParamGovActionDeposit :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamDrepDeposit :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamDrepActivity :: !(Maybe DbWord64) -- sqltype=word64type
  , epochParamMinFeeRefScriptCostPerByte :: !(Maybe Double)
  }
  deriving (Eq, Show, Generic)

type instance Key EpochParam = EpochParamId
instance DbInfo EpochParam

entityEpochParamDecoder :: D.Row (Entity EpochParam)
entityEpochParamDecoder =
  Entity
    <$> idDecoder EpochParamId
    <*> epochParamDecoder

epochParamDecoder :: D.Row EpochParam
epochParamDecoder =
  EpochParam
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochParamEpochNo
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
    <*> idDecoder BlockId -- epochParamBlockId
    <*> D.column (D.nullable D.bytea) -- epochParamExtraEntropy
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

entityEpochParamEncoder :: E.Params (Entity EpochParam)
entityEpochParamEncoder =
  mconcat
    [ entityKey >$< idEncoder getEpochParamId
    , entityVal >$< epochParamEncoder
    ]

epochParamEncoder :: E.Params EpochParam
epochParamEncoder =
  mconcat
    [ epochParamEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
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
    , epochParamBlockId >$< idEncoder getBlockId
    , epochParamExtraEntropy >$< E.param (E.nullable E.bytea)
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
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: epochstate
-- Description: Contains the state of the blockchain at the end of each epoch, including the committee, constitution, and no-confidence votes.
data EpochState = EpochState
  { epochStateCommitteeId :: !(Maybe CommitteeId) -- noreference
  , epochStateNoConfidenceId :: !(Maybe GovActionProposalId) -- noreference
  , epochStateConstitutionId :: !(Maybe ConstitutionId) -- noreference
  , epochStateEpochNo :: !Word64 -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key EpochState = EpochStateId
instance DbInfo EpochState

entityEpochStateDecoder :: D.Row (Entity EpochState)
entityEpochStateDecoder =
  Entity
    <$> idDecoder EpochStateId
    <*> epochStateDecoder

epochStateDecoder :: D.Row EpochState
epochStateDecoder =
  EpochState
    <$> maybeIdDecoder CommitteeId -- epochStateCommitteeId
    <*> maybeIdDecoder GovActionProposalId -- epochStateNoConfidenceId
    <*> maybeIdDecoder ConstitutionId -- epochStateConstitutionId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStateEpochNo

entityEpochStateEncoder :: E.Params (Entity EpochState)
entityEpochStateEncoder =
  mconcat
    [ entityKey >$< idEncoder getEpochStateId
    , entityVal >$< epochStateEncoder
    ]

epochStateEncoder :: E.Params EpochState
epochStateEncoder =
  mconcat
    [ epochStateCommitteeId >$< maybeIdEncoder getCommitteeId
    , epochStateNoConfidenceId >$< maybeIdEncoder getGovActionProposalId
    , epochStateConstitutionId >$< maybeIdEncoder getConstitutionId
    , epochStateEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

epochStateBulkEncoder :: E.Params ([Maybe CommitteeId], [Maybe GovActionProposalId], [Maybe ConstitutionId], [Word64])
epochStateBulkEncoder =
  contrazip4
    (manyEncoder $ E.nullable $ getCommitteeId >$< E.int8)
    (manyEncoder $ E.nullable $ getGovActionProposalId >$< E.int8)
    (manyEncoder $ E.nullable $ getConstitutionId >$< E.int8)
    (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: epochsync_time
-- Description: Tracks synchronization times for epochs, ensuring nodes are in consensus regarding the current state.
data EpochSyncTime = EpochSyncTime
  { epochSyncTimeNo :: !Word64 -- sqltype=word31type
  , epochSyncTimeSeconds :: !Word64 -- sqltype=word63type
  , epochSyncTimeState :: !SyncState -- sqltype=syncstatetype
  }
  deriving (Show, Eq, Generic)

type instance Key EpochSyncTime = EpochSyncTimeId
instance DbInfo EpochSyncTime where
  uniqueFields _ = ["no"]

entityEpochSyncTimeDecoder :: D.Row (Entity EpochSyncTime)
entityEpochSyncTimeDecoder =
  Entity
    <$> idDecoder EpochSyncTimeId
    <*> epochSyncTimeDecoder

epochSyncTimeDecoder :: D.Row EpochSyncTime
epochSyncTimeDecoder =
  EpochSyncTime
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochSyncTimeNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochSyncTimeSeconds
    <*> D.column (D.nonNullable syncStateDecoder) -- epochSyncTimeState

entityEpochSyncTimeEncoder :: E.Params (Entity EpochSyncTime)
entityEpochSyncTimeEncoder =
  mconcat
    [ entityKey >$< idEncoder getEpochSyncTimeId
    , entityVal >$< epochSyncTimeEncoder
    ]

epochSyncTimeEncoder :: E.Params EpochSyncTime
epochSyncTimeEncoder =
  mconcat
    [ epochSyncTimeNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochSyncTimeSeconds >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochSyncTimeState >$< E.param (E.nonNullable syncStateEncoder)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: ada_pots
-- Description: A table with all the different types of total balances.
--   This is only populated for the Shelley and later eras, and only on epoch boundaries.
--   The treasury and rewards fields will be correct for the whole epoch, but all other
--   fields change block by block.
data AdaPots = AdaPots
  { adaPotsSlotNo :: !Word64 -- sqltype=word63type
  , adaPotsEpochNo :: !Word64 -- sqltype=word31type
  , adaPotsTreasury :: !DbLovelace -- sqltype=lovelace
  , adaPotsReserves :: !DbLovelace -- sqltype=lovelace
  , adaPotsRewards :: !DbLovelace -- sqltype=lovelace
  , adaPotsUtxo :: !DbLovelace -- sqltype=lovelace
  , adaPotsDepositsStake :: !DbLovelace -- sqltype=lovelace
  , adaPotsFees :: !DbLovelace -- sqltype=lovelace
  , adaPotsBlockId :: !BlockId -- noreference
  , adaPotsDepositsDrep :: !DbLovelace -- sqltype=lovelace
  , adaPotsDepositsProposal :: !DbLovelace -- sqltype=lovelace
  }
  deriving (Show, Eq, Generic)

type instance Key AdaPots = AdaPotsId
instance DbInfo AdaPots

entityAdaPotsDecoder :: D.Row (Entity AdaPots)
entityAdaPotsDecoder =
  Entity
    <$> idDecoder AdaPotsId
    <*> adaPotsDecoder

adaPotsDecoder :: D.Row AdaPots
adaPotsDecoder =
  AdaPots
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- adaPotsSlotNo
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- adaPotsEpochNo
    <*> dbLovelaceDecoder -- adaPotsTreasury
    <*> dbLovelaceDecoder -- adaPotsReserves
    <*> dbLovelaceDecoder -- adaPotsRewards
    <*> dbLovelaceDecoder -- adaPotsUtxo
    <*> dbLovelaceDecoder -- adaPotsDepositsStake
    <*> dbLovelaceDecoder -- adaPotsFees
    <*> idDecoder BlockId -- adaPotsBlockId
    <*> dbLovelaceDecoder -- adaPotsDepositsDrep
    <*> dbLovelaceDecoder -- adaPotsDepositsProposal

entityAdaPotsEncoder :: E.Params (Entity AdaPots)
entityAdaPotsEncoder =
  mconcat
    [ entityKey >$< idEncoder getAdaPotsId
    , entityVal >$< adaPotsEncoder
    ]

adaPotsEncoder :: E.Params AdaPots
adaPotsEncoder =
  mconcat
    [ adaPotsSlotNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , adaPotsEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , adaPotsTreasury >$< dbLovelaceEncoder
    , adaPotsReserves >$< dbLovelaceEncoder
    , adaPotsRewards >$< dbLovelaceEncoder
    , adaPotsUtxo >$< dbLovelaceEncoder
    , adaPotsDepositsStake >$< dbLovelaceEncoder
    , adaPotsFees >$< dbLovelaceEncoder
    , adaPotsBlockId >$< idEncoder getBlockId
    , adaPotsDepositsDrep >$< dbLovelaceEncoder
    , adaPotsDepositsProposal >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: pot_transfer
-- Description: Records transfers between different pots (e.g., from the rewards pot to the treasury pot).
data PotTransfer = PotTransfer
  { potTransferCertIndex :: !Word16
  , potTransferTreasury :: !DbInt65 -- sqltype=int65type
  , potTransferReserves :: !DbInt65 -- sqltype=int65type
  , potTransferTxId :: !TxId -- noreference
  }
  deriving (Show, Eq, Generic)

instance DbInfo PotTransfer
type instance Key PotTransfer = PotTransferId

entityPotTransferDecoder :: D.Row (Entity PotTransfer)
entityPotTransferDecoder =
  Entity
    <$> idDecoder PotTransferId
    <*> potTransferDecoder

potTransferDecoder :: D.Row PotTransfer
potTransferDecoder =
  PotTransfer
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- potTransferCertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- potTransferTreasury
    <*> D.column (D.nonNullable dbInt65Decoder) -- potTransferReserves
    <*> idDecoder TxId -- potTransferTxId

entityPotTransferEncoder :: E.Params (Entity PotTransfer)
entityPotTransferEncoder =
  mconcat
    [ entityKey >$< idEncoder getPotTransferId
    , entityVal >$< potTransferEncoder
    ]

potTransferEncoder :: E.Params PotTransfer
potTransferEncoder =
  mconcat
    [ potTransferCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , potTransferTreasury >$< E.param (E.nonNullable dbInt65Encoder)
    , potTransferReserves >$< E.param (E.nonNullable dbInt65Encoder)
    , potTransferTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: treasury
-- Description: Holds funds allocated to the treasury, which can be used for network upgrades or other community initiatives.
data Treasury = Treasury
  { treasuryAddrId :: !StakeAddressId -- noreference
  , treasuryCertIndex :: !Word16
  , treasuryAmount :: !DbInt65 -- sqltype=int65type
  , treasuryTxId :: !TxId -- noreference
  }
  deriving (Show, Eq, Generic)

instance DbInfo Treasury
type instance Key Treasury = TreasuryId

entityTreasuryDecoder :: D.Row (Entity Treasury)
entityTreasuryDecoder =
  Entity
    <$> idDecoder TreasuryId
    <*> treasuryDecoder

treasuryDecoder :: D.Row Treasury
treasuryDecoder =
  Treasury
    <$> idDecoder StakeAddressId -- treasuryAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- treasuryCertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- treasuryAmount
    <*> idDecoder TxId -- treasuryTxId

entityTreasuryEncoder :: E.Params (Entity Treasury)
entityTreasuryEncoder =
  mconcat
    [ entityKey >$< idEncoder getTreasuryId
    , entityVal >$< treasuryEncoder
    ]

treasuryEncoder :: E.Params Treasury
treasuryEncoder =
  mconcat
    [ treasuryAddrId >$< idEncoder getStakeAddressId
    , treasuryCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , treasuryAmount >$< E.param (E.nonNullable dbInt65Encoder)
    , treasuryTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: reserve
-- Description: Stores reserves set aside by the protocol to stabilize the cryptocurrency's value or fund future activities.
data Reserve = Reserve
  { reserveAddrId :: !StakeAddressId -- noreference
  , reserveCertIndex :: !Word16
  , reserveAmount :: !DbInt65 -- sqltype=int65type
  , reserveTxId :: !TxId -- noreference
  }
  deriving (Show, Eq, Generic)

type instance Key Reserve = ReserveId
instance DbInfo Reserve

entityReserveDecoder :: D.Row (Entity Reserve)
entityReserveDecoder =
  Entity
    <$> idDecoder ReserveId
    <*> reserveDecoder

reserveDecoder :: D.Row Reserve
reserveDecoder =
  Reserve
    <$> idDecoder StakeAddressId -- reserveAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- reserveCertIndex
    <*> D.column (D.nonNullable dbInt65Decoder) -- reserveAmount
    <*> idDecoder TxId -- reserveTxId

entityReserveEncoder :: E.Params (Entity Reserve)
entityReserveEncoder =
  mconcat
    [ entityKey >$< idEncoder getReserveId
    , entityVal >$< reserveEncoder
    ]

reserveEncoder :: E.Params Reserve
reserveEncoder =
  mconcat
    [ reserveAddrId >$< idEncoder getStakeAddressId
    , reserveCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , reserveAmount >$< E.param (E.nonNullable dbInt65Encoder)
    , reserveTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: cost_model
-- Description: Defines the cost model used for estimating transaction fees, ensuring efficient resource allocation on the network.
data CostModel = CostModel
  { costModelCosts :: !Text -- sqltype=jsonb
  , costModelHash :: !ByteString -- sqltype=hash32type
  }
  deriving (Eq, Show, Generic)

type instance Key CostModel = CostModelId
instance DbInfo CostModel where
  uniqueFields _ = ["hash"]

entityCostModelDecoder :: D.Row (Entity CostModel)
entityCostModelDecoder =
  Entity
    <$> idDecoder CostModelId
    <*> costModelDecoder

costModelDecoder :: D.Row CostModel
costModelDecoder =
  CostModel
    <$> D.column (D.nonNullable D.text) -- costModelCosts
    <*> D.column (D.nonNullable D.bytea) -- costModelHash

entityCostModelEncoder :: E.Params (Entity CostModel)
entityCostModelEncoder =
  mconcat
    [ entityKey >$< idEncoder getCostModelId
    , entityVal >$< costModelEncoder
    ]

costModelEncoder :: E.Params CostModel
costModelEncoder =
  mconcat
    [ costModelCosts >$< E.param (E.nonNullable E.text)
    , costModelHash >$< E.param (E.nonNullable E.bytea)
    ]
