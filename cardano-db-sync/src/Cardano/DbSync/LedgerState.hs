{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.LedgerState
  ( initialLedgerState
  ) where

import qualified Cardano.Chain.Genesis as Byron

import           Cardano.DbSync.Types

import           Cardano.Prelude

import           Ouroboros.Consensus.Byron.Ledger (initByronLedgerState)
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.State.Infra (initHardForkState)


initialLedgerState :: Byron.Config -> LedgerState CardanoBlock
initialLedgerState genesisConfig =
  HardForkLedgerState $ initHardForkState (initByronLedgerState genesisConfig Nothing)
