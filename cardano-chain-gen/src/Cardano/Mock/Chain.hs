{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Mock.Chain (
  Chain' (..),
  State,
  Chain,
  getTipState,
  successorBlock,
  pointOnChain,
  rollback,
  findFirstPointChain,
  pointIsAfter,
  findFirstPointByBlockNo,
  currentTipBlockNo,
) where

import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Basics (EmptyMK, ValuesMK)
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block

-- | This looks a lot like the 'Chain' defined in Ouroboros.Network.MockChain.Chain
-- but this version includes also the ledger states.
data Chain' block st
  = Genesis st
  | Chain' block st :> (block, st)
  deriving (Eq, Ord, Show, Functor)

type State block = (Consensus.ExtLedgerState block EmptyMK, Consensus.LedgerTables (Consensus.ExtLedgerState block) ValuesMK)

type Chain block = Chain' block (State block)

infixl 5 :>

getTipState :: Chain' blk st -> st
getTipState (Genesis st) = st
getTipState (_ :> (_, st)) = st

successorBlock :: forall block. HasHeader block => Point block -> Chain block -> Maybe block
successorBlock p c0 | headPoint c0 == p = Nothing
successorBlock p c0 =
  go c0
  where
    go :: Chain block -> Maybe block
    go (c :> (b', st') :> (b, _))
      | blockPoint b' == p = Just b
      | otherwise = go (c :> (b', st'))
    go (Genesis _ :> (b, _)) | p == genesisPoint = Just b
    go _ = Nothing

pointOnChain :: HasHeader block => Point block -> Chain block -> Bool
pointOnChain GenesisPoint _ = True
pointOnChain (BlockPoint _ _) (Genesis _) = False
pointOnChain p@(BlockPoint pslot phash) (c :> (b, _))
  | pslot > blockSlot b = False
  | phash == blockHash b = True
  | otherwise = pointOnChain p c

headPoint :: HasHeader block => Chain block -> Point block
headPoint (Genesis _) = genesisPoint
headPoint (_ :> (b, _)) = blockPoint b

findFirstPointChain ::
  HasHeader block =>
  [Point block] ->
  Chain block ->
  Maybe (Point block)
findFirstPointChain [] _ = Nothing
findFirstPointChain (p : ps) c
  | pointOnChain p c = Just p
  | otherwise = findFirstPointChain ps c

rollback :: HasHeader block => Chain block -> Point block -> Maybe (Chain block)
rollback (c :> (b, st)) p
  | blockPoint b == p = Just (c :> (b, st))
  | otherwise = rollback c p
rollback (Genesis st) p
  | p == genesisPoint = Just (Genesis st)
  | otherwise = Nothing

-- | Check whether the first point is after the second point on the chain.
-- Usually, this can simply be checked using the 'SlotNo's, but some blocks
-- may have the same 'SlotNo'.
--
-- When the first point equals the second point, the answer will be 'False'.
--
-- PRECONDITION: both points are on the chain.
pointIsAfter ::
  HasHeader block =>
  Point block ->
  Point block ->
  Chain block ->
  Bool
pointIsAfter pt1 pt2 c =
  case pointSlot pt1 `compare` pointSlot pt2 of
    LT -> False
    GT -> True
    EQ
      | Just (_, afterPt2) <- AF.splitAfterPoint (toAnchoredFragment c) pt2 ->
          AF.pointOnFragment pt1 afterPt2
      | otherwise ->
          False

-- * Conversions to/from 'AnchoredFragment'

-- | Convert a 'Chain' to an 'AnchoredFragment'.
--
-- The anchor of the fragment will be 'Chain.genesisPoint'.
toAnchoredFragment :: HasHeader block => Chain block -> AF.AnchoredFragment block
toAnchoredFragment = AF.fromOldestFirst AF.AnchorGenesis . toOldestFirst

-- | Produce the list of blocks, from genesis to the most recent
toOldestFirst :: Chain block -> [block]
toOldestFirst = reverse . toNewestFirst

-- | Produce the list of blocks, from most recent back to genesis
toNewestFirst :: Chain block -> [block]
toNewestFirst = foldChain (flip (:)) []

foldChain :: (a -> b -> a) -> a -> Chain b -> a
foldChain _blk gen (Genesis _st) = gen
foldChain blk gen (c :> (b, _)) = blk (foldChain blk gen c) b

findFirstPointByBlockNo ::
  HasHeader block =>
  Chain block ->
  BlockNo ->
  Maybe (Point block)
findFirstPointByBlockNo c blkNo = case c of
  Genesis _ -> Nothing
  (_ :> (b, _)) | blockNo b == blkNo -> Just $ blockPoint b
  (c' :> _) -> findFirstPointByBlockNo c' blkNo

currentTipBlockNo :: HasHeader block => Chain block -> Maybe BlockNo
currentTipBlockNo c = case c of
  Genesis _ -> Nothing
  (_ :> (b, _)) -> Just $ blockNo b
