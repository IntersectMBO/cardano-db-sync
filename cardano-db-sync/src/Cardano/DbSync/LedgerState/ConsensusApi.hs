{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.LedgerState.ConsensusApi where

import Cardano.DbSync.Types (CardanoBlock) -- Just the normal block
import Data.Map.Diff.Strict (applyDiff)
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)
import Ouroboros.Consensus.Cardano.CanHardFork (CardanoHardForkConstraints)
import Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import Ouroboros.Consensus.Ledger.Abstract (LedgerResult, tickThenReapplyLedgerResult)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg, ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))

type ExtLedger = ExtLedgerState CardanoBlock EmptyMK
type AppResult = LedgerResult (ExtLedgerState CardanoBlock) ExtLedger

type LedgerB blk = LedgerState blk EmptyMK
type Ledger = LedgerState CardanoBlock EmptyMK

initLedger ::
  CardanoHardForkConstraints StandardCrypto =>
  ProtocolInfo IO CardanoBlock ->
  ExtLedger
initLedger = stowLedgerTables . pInfoInitLedger

reapplyBlock ::
  CardanoHardForkConstraints StandardCrypto =>
  ExtLedgerCfg CardanoBlock ->
  CardanoBlock ->
  ExtLedger ->
  AppResult
reapplyBlock cfg block lsb =
  fmap (stowLedgerTables . applyDiffToTables) res
  where
    unstowedLedger = unstowLedgerTables lsb
    tables = projectLedgerTables unstowedLedger
    res = tickThenReapplyLedgerResult cfg block unstowedLedger
    applyDiffToTables st = zipOverLedgerTables f st tables

    f :: Ord k => DiffMK k v -> ValuesMK k v -> ValuesMK k v
    f (ApplyDiffMK d) (ApplyValuesMK v) = ApplyValuesMK (applyDiff v d)
