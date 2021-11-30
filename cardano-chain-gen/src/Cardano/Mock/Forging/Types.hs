{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Mock.Forging.Types where

import           Control.Exception

import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock)
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, ShelleyEra, StandardCrypto)

import           Cardano.Ledger.Address
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Credential

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
  | CantFindUTxO UTxOIndex
  | ExpectedAlonzoState
  | ExpectedShelleyState
  | UnexpectedEra
  deriving (Show, Exception)

data UTxOIndex = UTxOIndex Int | UTxOAddress (Addr StandardCrypto)
    deriving (Show, Eq)

data StakeIndex = StakeIndex Int | StakeAddress (StakeCredential StandardCrypto)
