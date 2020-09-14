{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.LedgerState
  ( CardanoLedgerState (..)
  , LedgerStateVar (..)
  , applyBlockChecked
  , initLedgerStateVar
  , saveLedgerState
  ) where

import           Cardano.DbSync.Config
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, putTMVar, takeTMVar)

import           Ouroboros.Consensus.Block (blockNo, blockPrevHash)
import           Ouroboros.Consensus.Byron.Ledger (initByronLedgerState)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, ledgerTipHash, tickThenReapply)
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.State.Infra (initHardForkState)


data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(LedgerState CardanoBlock)
  , clsConfig :: !(LedgerConfig CardanoBlock)
  }

newtype LedgerStateVar = LedgerStateVar
  { unLedgerStateVar :: TMVar CardanoLedgerState
  }

initLedgerStateVar :: GenesisConfig -> IO LedgerStateVar
initLedgerStateVar genesisConfig = do
  LedgerStateVar <$>
    case genesisConfig of
      GenesisCardano byronConfig _ ->
        newTMVarIO $
          CardanoLedgerState
            { clsState = HardForkLedgerState $ initHardForkState (initByronLedgerState byronConfig Nothing)
            , clsConfig = cardanoLedgerConfig genesisConfig
            }

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlockChecked :: LedgerStateVar -> CardanoBlock -> IO CardanoLedgerState
applyBlockChecked (LedgerStateVar stateVar) blk =
  -- 'LedgerStateVar' is just being used as a mutable variable. There should not ever
  -- be any contention on this variable, so putting everything inside 'atomically'
  -- is fine.
  atomically $ do
    oldState <- takeTMVar stateVar
    if ledgerTipHash (clsState oldState) == blockPrevHash blk
      then do
        let !newState = oldState { clsState = tickThenReapply (clsConfig oldState) blk (clsState oldState) }
        putTMVar stateVar newState
        pure newState
      else panic $ "applyBlockChecked: Hash mismatch for block no " <> textShow (blockNo blk)


saveLedgerState :: SlotNo -> CardanoLedgerState -> IO ()
saveLedgerState = undefined
