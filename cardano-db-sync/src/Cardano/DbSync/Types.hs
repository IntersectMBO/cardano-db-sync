{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Types
  ( BlockDetails (..)
  , CardanoBlock
  , CardanoProtocol
  , EpochSlot (..)
  , ShelleyAddress
  , ShelleyBlock
  , ShelleyDCert
  , ShelleyDelegCert
  , ShelleyHash
  , ShelleyMIRCert
  , ShelleyPoolCert
  , ShelleyPoolParams
  , ShelleyRewardAccount
  , ShelleyStakeCreds
  , ShelleyStakePoolKeyHash
  , ShelleyStakingCred
  , ShelleyStakingKeyHash
  , ShelleyTx
  , ShelleyTxBody
  , ShelleyTxId
  , ShelleyTxIn
  , ShelleyTxOut
  , ShelleyTxSeq
  , SlotDetails (..)
  , SyncState (..)
  ) where

import           Cardano.DbSync.Config.Types (CardanoBlock, CardanoProtocol)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley


data BlockDetails = BlockDetails
  { bdBlock :: !CardanoBlock
  , bdSlot :: !SlotDetails
  }

newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Show)

type ShelleyAddress = Shelley.Addr StandardShelley
type ShelleyBlock = Shelley.ShelleyBlock StandardShelley
type ShelleyDCert = Shelley.DCert StandardShelley
type ShelleyDelegCert = Shelley.DelegCert StandardShelley
type ShelleyHash = Shelley.ShelleyHash StandardShelley
type ShelleyMIRCert = Shelley.MIRCert StandardShelley
type ShelleyPoolCert = Shelley.PoolCert StandardShelley
type ShelleyPoolParams = Shelley.PoolParams StandardShelley
type ShelleyRewardAccount = Shelley.RewardAcnt StandardShelley
type ShelleyStakeCreds = Shelley.StakeCreds StandardShelley
type ShelleyStakingCred = Shelley.StakeCredential StandardShelley
type ShelleyStakingKeyHash = Shelley.KeyHash 'Shelley.Staking StandardShelley
type ShelleyStakePoolKeyHash = Shelley.KeyHash 'Shelley.StakePool StandardShelley
type ShelleyTx = Shelley.Tx StandardShelley
type ShelleyTxBody = Shelley.TxBody StandardShelley
type ShelleyTxId = Shelley.TxId StandardShelley
type ShelleyTxIn = Shelley.TxIn StandardShelley
type ShelleyTxOut = Shelley.TxOut StandardShelley
type ShelleyTxSeq = Shelley.TxSeq StandardShelley

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

data SyncState
  = SyncLagging         -- Local tip is lagging the global chain tip.
  | SyncFollowing       -- Local tip is following global chain tip.
  deriving (Eq, Show)
