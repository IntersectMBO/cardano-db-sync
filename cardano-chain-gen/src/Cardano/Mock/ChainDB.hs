{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Mock.ChainDB
  ( ChainDB (..)
  , initChainDB
  , headTip
  , currentState
  , replaceGenesisDB
  , extendChainDB
  , findFirstPoint
  , rollbackChainDB
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import           Ouroboros.Consensus.Ledger.SupportsProtocol

import           Ouroboros.Network.Block (Tip (..))

import           Cardano.Mock.Chain

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

initChainDB :: TopLevelConfig block -> State block -> ChainDB block
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

replaceGenesisDB :: ChainDB block -> State block -> ChainDB block
replaceGenesisDB chainDB st = chainDB {cchain = Genesis st}

extendChainDB :: LedgerSupportsProtocol block => ChainDB block -> block -> ChainDB block
extendChainDB chainDB blk = do
  let !chain = cchain chainDB
      !st = tickThenReapply (Consensus.ExtLedgerCfg $ chainConfig chainDB) blk (getTipState chain)
   in chainDB {cchain = chain :> (blk, st)}

findFirstPoint :: HasHeader block => [Point block] -> ChainDB block -> Maybe (Point block)
findFirstPoint points chainDB = findFirstPointChain points (cchain chainDB)

rollbackChainDB :: HasHeader block => ChainDB block -> Point block -> Maybe (ChainDB block)
rollbackChainDB chainDB p = do
  chain <- rollback (cchain chainDB) p
  Just $ chainDB { cchain = chain}
