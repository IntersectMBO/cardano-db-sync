{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Mock.Forging.Types
  ( CardanoBlock
  , TPraosStandard
  , PraosStandard
  , ForgingError (..)
  , MockBlock (..)
  , NodeId (..)
  , PoolIndex (..)
  , StakeIndex (..)
  , TxEra (..)
  , UTxOIndex (..)
  ) where

import           Control.Exception

import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo, StandardBabbage,
                   StandardShelley, StandardCrypto)

import           Cardano.Ledger.Address
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Keys
import           Cardano.Ledger.TxIn (TxIn (..))

import           Cardano.Slotting.Slot (SlotNo (..))

type TPraosStandard = TPraos StandardCrypto
type PraosStandard = Praos StandardCrypto

type CardanoBlock = Consensus.CardanoBlock StandardCrypto

data MockBlock = MockBlock
  { txs :: ![TxEra]
  , node :: !NodeId
  }

data TxEra
  = TxAlonzo !(Core.Tx StandardAlonzo)
  | TxBabbage !(Core.Tx StandardBabbage)
  | TxShelley !(Core.Tx StandardShelley)

newtype NodeId = NodeId { unNodeId :: Int }
  deriving Show

data ForgingError
  = WentTooFar !SlotNo
  | ForecastError !SlotNo !OutsideForecastRange
  | NonExistantNode !NodeId
  | CantFindUTxO
  | CantFindStake
  | ExpectedBabbageState
  | ExpectedAlonzoState
  | ExpectedShelleyState
  | UnexpectedEra
  | EmptyFingerprint !SlotNo !FilePath
  | FailedToValidateSlot !SlotNo !Int !FilePath
  | NotExpectedSlotNo !SlotNo !SlotNo !Int
  | FingerprintDecodeError !String
  deriving (Show, Exception)

data UTxOIndex era
  = UTxOIndex Int
  | UTxOAddress !(Addr StandardCrypto)
  | UTxOInput !(TxIn StandardCrypto)
  | UTxOPair !(TxIn StandardCrypto, Core.TxOut era)
  | UTxOAddressNew !Int
  | UTxOAddressNewWithStake !Int !StakeIndex
  | UTxOAddressNewWithPtr !Int !Ptr

data StakeIndex
  = StakeIndex !Int
  | StakeAddress !(StakeCredential StandardCrypto)
  | StakeIndexNew !Int
  | StakeIndexScript !Bool
  | StakeIndexPoolLeader !PoolIndex
  | StakeIndexPoolMember !Int !PoolIndex

data PoolIndex
  = PoolIndex !Int
  | PoolIndexId !(KeyHash 'StakePool StandardCrypto)
  | PoolIndexNew !Int
