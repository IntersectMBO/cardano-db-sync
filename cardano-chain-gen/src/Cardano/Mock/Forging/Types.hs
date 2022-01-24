{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Mock.Forging.Types where

import           Control.Exception

import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock)
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, ShelleyEra, StandardCrypto)

import           Cardano.Ledger.Address
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Keys
import           Cardano.Ledger.TxIn (TxIn (..))

import           Cardano.Slotting.Slot (SlotNo (..))

type CardanoBlock = HardForkBlock (CardanoEras StandardCrypto)

data MockBlock = MockBlock
  { txs :: [TxEra]
  , node :: NodeId
  }

data TxEra = TxAlonzo (Core.Tx (AlonzoEra StandardCrypto))
           | TxShelley (Core.Tx (ShelleyEra StandardCrypto))

newtype NodeId = NodeId { unNodeId :: Int }
  deriving Show

data ForgingError =
    WentTooFar
  | ForecastError SlotNo OutsideForecastRange
  | NonExistantNode NodeId
  | CantFindUTxO
  | CantFindStake
  | ExpectedAlonzoState
  | ExpectedShelleyState
  | UnexpectedEra
  | EmptyFingerprint SlotNo FilePath
  | FailedToValidateSlot SlotNo Int FilePath
  | NotExpectedSlotNo SlotNo SlotNo Int
  | FingerprintDecodeError String
  deriving (Show, Exception)

data UTxOIndex era = UTxOIndex Int | UTxOAddress (Addr StandardCrypto) | UTxOInput (TxIn StandardCrypto)
                   | UTxOPair (TxIn StandardCrypto, Core.TxOut era)
                   | UTxOAddressNew Int | UTxOAddressNewWithStake Int StakeIndex
                   | UTxOAddressNewWithPtr Int Ptr

data StakeIndex = StakeIndex Int | StakeAddress (StakeCredential StandardCrypto)
                | StakeIndexNew Int | StakeIndexScript Bool
                | StakeIndexPoolLeader PoolIndex | StakeIndexPoolMember Int PoolIndex

data PoolIndex = PoolIndex Int | PoolIndexId (KeyHash 'StakePool StandardCrypto)
