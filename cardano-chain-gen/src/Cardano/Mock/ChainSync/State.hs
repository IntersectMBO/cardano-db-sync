{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Mock.ChainSync.State (
  ChainProducerState (..),
  FollowerStates,
  FollowerId,
  FollowerState (..),
  initChainProducerState,
  addBlockState,
  rollbackState,
  updateFollower,
  initFollower,
  followerInstruction,
) where

import qualified Cardano.Mock.Chain as Chain
import Cardano.Mock.ChainDB
import Control.Exception (assert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block (HasHeader, HeaderHash, Point, blockPoint, castPoint)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Network.Block (ChainUpdate (..))

data ChainProducerState block = ChainProducerState
  { chainDB :: ChainDB block
  , chainFollowers :: FollowerStates block
  , nextFollowerId :: FollowerId
  }

type FollowerStates block = Map FollowerId (FollowerState block)

type FollowerId = Int

data FollowerState block = FollowerState
  { followerPoint :: Point block
  -- ^ Where the chain of the consumer and producer intersect. If the
  -- consumer is on the chain then this is the consumer's chain head,
  -- but if the consumer's chain is off the producer's chain then this is
  -- the point the consumer will need to rollback to.
  , followerNext :: FollowerNext
  -- ^ Where the will go next, roll back to the follower point, or roll
  -- forward from the follower point.
  }
  deriving (Eq, Show)

data FollowerNext
  = FollowerBackTo
  | FollowerForwardFrom
  deriving (Eq, Show)

initChainProducerState :: TopLevelConfig block -> Chain.State block -> ChainProducerState block
initChainProducerState config st = ChainProducerState (initChainDB config st) Map.empty 0

-- | Add a block to the chain. It does not require any follower's state changes.
addBlockState ::
  LedgerSupportsProtocol block =>
  block ->
  ChainProducerState block ->
  ChainProducerState block
addBlockState b (ChainProducerState c cflrst cfid) =
  ChainProducerState (extendChainDB c b) cflrst cfid

-- | Rollback producer chain. It requires to update follower states, since some
-- @'followerPoint'@s may not be on the new chain; in this case find intersection
-- of the two chains and set @'followerNext'@ to @'FollowerBackTo'@.
rollbackState ::
  (HasHeader block, HeaderHash block ~ HeaderHash block') =>
  Point block' ->
  ChainProducerState block ->
  Maybe (ChainProducerState block)
rollbackState p (ChainProducerState c cflrst cfid) = do
  c' <- rollbackChainDB c (castPoint p)
  pure (ChainProducerState c' (rollbackFollower <$> cflrst) cfid)
  where
    rollbackFollower flrst@FollowerState {followerPoint = p'}
      | Chain.pointIsAfter p' (castPoint p) (cchain c) =
          flrst {followerPoint = castPoint p, followerNext = FollowerBackTo}
      | otherwise =
          flrst

-- | Get the recorded state of a chain consumer. The 'FollowerId' is assumed to
-- exist.
lookupFollower :: ChainProducerState block -> FollowerId -> FollowerState block
lookupFollower (ChainProducerState _ cflrst _) fid = cflrst Map.! fid

-- | Change the intersection point of a follower. This also puts it into
-- the 'FollowerBackTo' state.
updateFollower ::
  FollowerId ->
  -- | new follower intersection point
  Point block ->
  ChainProducerState block ->
  ChainProducerState block
updateFollower fid point (ChainProducerState c cflrst cnfid) =
  ChainProducerState c (Map.adjust update fid cflrst) cnfid
  where
    update flrst = flrst {followerPoint = point, followerNext = FollowerBackTo}

-- | Add a new follower with the given intersection point and return the new
-- 'FollowerId'.
initFollower ::
  HasHeader block =>
  Point block ->
  ChainProducerState block ->
  (ChainProducerState block, FollowerId)
initFollower point (ChainProducerState c cflrst cfid) =
  assert
    (Chain.pointOnChain point $ cchain c)
    (ChainProducerState c (Map.insert cfid flrst cflrst) (succ cfid), cfid)
  where
    flrst =
      FollowerState
        { followerPoint = point
        , followerNext = FollowerBackTo
        }

-- | What a follower needs to do next. Should they move on to the next block or
-- do they need to roll back to a previous point on their chain. It also updates
-- the producer's state assuming that the follower follows its instruction.
followerInstruction ::
  HasHeader block =>
  FollowerId ->
  ChainProducerState block ->
  Maybe (ChainUpdate block block, ChainProducerState block)
followerInstruction fid cps@(ChainProducerState c cflrst cfid) =
  let FollowerState {followerPoint, followerNext} = lookupFollower cps fid
   in case followerNext of
        FollowerForwardFrom ->
          assert (Chain.pointOnChain followerPoint $ cchain c) $
            case Chain.successorBlock followerPoint $ cchain c of
              -- There is no successor block because the follower is at the head
              Nothing -> Nothing
              Just b -> Just (AddBlock b, cps')
                where
                  cps' = ChainProducerState c (Map.adjust setPoint fid cflrst) cfid
                  setPoint flrst = flrst {followerPoint = blockPoint b}
        FollowerBackTo -> Just (RollBack followerPoint, cps')
          where
            cps' = ChainProducerState c (Map.adjust setForwardFrom fid cflrst) cfid
            setForwardFrom flrst = flrst {followerNext = FollowerForwardFrom}
