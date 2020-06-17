{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Types
  ( CardanoBlockTip (..)
  , CardanoPoint (..)
  , ConfigFile (..)
  , DbSyncNodeParams (..)
  , GenesisFile (..)
  , ShelleyAddress
  , ShelleyBlock
  , ShelleyDCert
  , ShelleyHash
  , ShelleyPoolCert
  , ShelleyPoolParams
  , ShelleyRewardAccount
  , ShelleyStakePoolKeyHash
  , ShelleyStakingKeyHash
  , ShelleyTx
  , ShelleyTxBody
  , ShelleyTxId
  , ShelleyTxIn
  , ShelleyTxOut
  , ShelleyTxSeq
  , SocketPath (..)
  ) where

import           Cardano.Db (MigrationDir (..))

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley
import           Ouroboros.Network.Block (Point (..), Tip)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BlockChain as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley


data CardanoBlockTip
  = ByronBlockTip !ByronBlock !(Tip ByronBlock)
  | ShelleyBlockTip !ShelleyBlock !(Tip ShelleyBlock)

data CardanoPoint
  = ByronPoint !(Point ByronBlock)
  | ShelleyPoint !(Point ShelleyBlock)

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

-- | The product type of all command line arguments
data DbSyncNodeParams = DbSyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpGenesisFile :: !GenesisFile
  , enpSocketPath :: !SocketPath
  , enpMigrationDir :: !MigrationDir
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

type ShelleyAddress = Shelley.Addr Shelley.TPraosStandardCrypto
type ShelleyBlock = Shelley.ShelleyBlock Shelley.TPraosStandardCrypto
type ShelleyDCert = Shelley.DCert Shelley.TPraosStandardCrypto
type ShelleyHash = Shelley.ShelleyHash Shelley.TPraosStandardCrypto
type ShelleyPoolCert = Shelley.PoolCert Shelley.TPraosStandardCrypto
type ShelleyPoolParams = Shelley.PoolParams Shelley.TPraosStandardCrypto
type ShelleyRewardAccount = Shelley.RewardAcnt Shelley.TPraosStandardCrypto
type ShelleyStakingKeyHash = Shelley.KeyHash 'Shelley.Staking Shelley.TPraosStandardCrypto
type ShelleyStakePoolKeyHash = Shelley.KeyHash 'Shelley.StakePool Shelley.TPraosStandardCrypto
type ShelleyTx = Shelley.Tx Shelley.TPraosStandardCrypto
type ShelleyTxBody = Shelley.TxBody Shelley.TPraosStandardCrypto
type ShelleyTxId = Shelley.TxId Shelley.TPraosStandardCrypto
type ShelleyTxIn = Shelley.TxIn Shelley.TPraosStandardCrypto
type ShelleyTxOut = Shelley.TxOut Shelley.TPraosStandardCrypto
type ShelleyTxSeq = Shelley.TxSeq Shelley.TPraosStandardCrypto

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }
