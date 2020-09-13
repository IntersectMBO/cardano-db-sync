{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Types
  ( BlockDetails (..)
  , CardanoBlock
  , ConfigFile (..)
  , DbSyncEnv (..)
  , DbSyncNodeParams (..)
  , ShelleyAddress
  , ShelleyBlock
  , ShelleyCredentialStaking
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
  , EpochSlot (..)
  , SocketPath (..)
  ) where

import           Cardano.Db (MigrationDir (..))
import           Cardano.DbSync.Config

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto, StandardShelley)

import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley

-- No longer contains a Tip value because the Tip value was useless.
data BlockDetails
  = ByronBlockDetails !ByronBlock !SlotDetails
  | ShelleyBlockDetails !(Shelley.ShelleyBlock StandardShelley) !SlotDetails

type CardanoBlock = Consensus.CardanoBlock StandardCrypto

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

-- | The product type of all command line arguments
data DbSyncNodeParams = DbSyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpSocketPath :: !SocketPath
  , enpMigrationDir :: !MigrationDir
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

data DbSyncEnv = DbSyncEnv
  { envProtocol :: !DbSyncProtocol
  , envNetwork :: !Shelley.Network
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  }

type ShelleyAddress = Shelley.Addr StandardShelley
type ShelleyBlock = Shelley.ShelleyBlock StandardShelley
type ShelleyCredentialStaking = Shelley.Credential 'Shelley.Staking StandardShelley
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

type ShelleyDCert = Shelley.DCert StandardShelley
type ShelleyTx = Shelley.Tx StandardShelley
type ShelleyTxBody = Shelley.TxBody StandardShelley
type ShelleyTxId = Shelley.TxId StandardShelley
type ShelleyTxIn = Shelley.TxIn StandardShelley
type ShelleyTxOut = Shelley.TxOut StandardShelley
type ShelleyTxSeq = Shelley.TxSeq StandardShelley

data SlotDetails = SlotDetails
  { sdTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }

