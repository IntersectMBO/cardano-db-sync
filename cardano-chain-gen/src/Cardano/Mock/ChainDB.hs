{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Mock.ChainDB (
  ChainDB (..),
  currentState,
  initChainDB,
  headTip,
  replaceGenesisDB,
  extendChainDB,
  findFirstPoint,
  rollbackChainDB,
  findPointByBlockNo,
  currentBlockNo,
) where

import Cardano.Mock.Chain
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Cardano.Ledger ()
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Ledger.Tables as Consensus
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffsMK, forgetLedgerTables, restrictValuesMK)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Tip (..))

-- | Thin layer around 'Chain' that knows how to apply blocks and maintain
-- new and old states. The state here, which is the 'Chain', is not a MVar,
-- because we want to reuse the Api in different places, like in  the state
-- of Chainsync server, in property tests where we want pure code, in
-- Forging etc.
data ChainDB block = ChainDB
  { chainConfig :: TopLevelConfig block
  , cchain :: Chain block
  }

instance Eq (Chain block) => Eq (ChainDB block) where
  a == b = cchain a == cchain b

instance Show (Chain block) => Show (ChainDB block) where
  show = show . cchain

initChainDB ::
  TopLevelConfig block ->
  State block ->
  ChainDB block
initChainDB config st = ChainDB config (Genesis st)

headTip :: HasHeader block => ChainDB block -> Tip block
headTip chainDB =
  case cchain chainDB of
    Genesis _ -> TipGenesis
    (_ :> (b, _)) -> Tip (blockSlot b) (blockHash b) (blockNo b)

currentState :: ChainDB block -> State block
currentState chainDB =
  case cchain chainDB of
    Genesis st -> st
    _ :> (_, st) -> st

replaceGenesisDB ::
  ChainDB block ->
  State block ->
  ChainDB block
replaceGenesisDB chainDB st = chainDB {cchain = Genesis st}

extendChainDB ::
  forall block.
  LedgerSupportsProtocol block =>
  ChainDB block ->
  block ->
  ChainDB block
extendChainDB chainDB blk = do
  let !chain = cchain chainDB
      -- Get the current ledger state
      (tipState, tables) = getTipState chain
      -- Apply the block and compute the diffs
      keys :: LedgerTables (Consensus.ExtLedgerState block) KeysMK
      keys = getBlockKeySets blk
      ledgerTables = Consensus.getLedgerTables tables
      restrictedTables = restrictValuesMK ledgerTables (Consensus.getLedgerTables keys)
      ledgerState = Consensus.withLedgerTables tipState (Consensus.LedgerTables restrictedTables)
      !diffState =
        tickThenReapply
          ComputeLedgerEvents
          (Consensus.ExtLedgerCfg $ chainConfig chainDB)
          blk
          ledgerState
      !ledgerTables' =
        Consensus.LedgerTables
          . applyDiffsMK ledgerTables
          . Consensus.getLedgerTables
          . Consensus.projectLedgerTables
          $ diffState
      !ledgerState' = forgetLedgerTables diffState
   in chainDB {cchain = chain :> (blk, (ledgerState', ledgerTables'))}

findFirstPoint :: HasHeader block => [Point block] -> ChainDB block -> Maybe (Point block)
findFirstPoint points chainDB = findFirstPointChain points (cchain chainDB)

rollbackChainDB :: HasHeader block => ChainDB block -> Point block -> Maybe (ChainDB block)
rollbackChainDB chainDB p = do
  chain <- rollback (cchain chainDB) p
  Just $ chainDB {cchain = chain}

findPointByBlockNo :: HasHeader block => ChainDB block -> BlockNo -> Maybe (Point block)
findPointByBlockNo chainDB =
  findFirstPointByBlockNo (cchain chainDB)

currentBlockNo :: HasHeader block => ChainDB block -> Maybe BlockNo
currentBlockNo chainDB =
  currentTipBlockNo (cchain chainDB)
