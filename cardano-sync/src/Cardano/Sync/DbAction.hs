{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Sync.DbAction
  ( DbAction (..)
  , DbPoint (..)
  , DbActionQueue (..)
  , blockingFlushDbActionQueue
  , lengthDbActionQueue
  , mkDbApply
  , mkDbRollback
  , newDbActionQueue
  , writeDbActionQueue
  ) where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import           Cardano.Sync.Types

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ

import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))

import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Point (Block (..), blockPointHash)

data DbAction
  = DbApplyBlock !BlockDetails
  | DbRollBackToPoint !DbPoint
  | DbFinish

-- Define a db-sync specific Point type because the one in ouroborous-network
-- is parameterised over era which makes it a huge pain in the neck to use.
data DbPoint = DbPoint
  { dbpSlot :: !SlotNo
  , dbpHash :: !ByteString
  }

newtype DbActionQueue = DbActionQueue
  { dbActQueue :: TBQueue DbAction
  }

mkDbApply :: CardanoBlock -> SlotDetails -> DbAction
mkDbApply cblk details =
  DbApplyBlock (BlockDetails cblk details)

mkDbRollback :: Point CardanoBlock -> DbAction
mkDbRollback pt =
  case getPoint pt of
    Origin -> DbRollBackToPoint $ DbPoint (SlotNo 0) "genesis"
    At blk -> DbRollBackToPoint $
                DbPoint
                  (blockPointSlot blk)
                  (toRawHash (Proxy @CardanoBlock) $ blockPointHash blk)

lengthDbActionQueue :: DbActionQueue -> STM Natural
lengthDbActionQueue (DbActionQueue q) = STM.lengthTBQueue q

newDbActionQueue :: IO DbActionQueue
newDbActionQueue =
    -- Use an odd number here so that the db_tip_height metric increments by this odd number
    -- when syncing, instead of incrementing by say 2000.
    DbActionQueue <$> TBQ.newTBQueueIO 2117

writeDbActionQueue :: DbActionQueue -> DbAction -> STM ()
writeDbActionQueue (DbActionQueue q) = TBQ.writeTBQueue q

-- | Block if the queue is empty and if its not read/flush everything.
-- Need this because `flushTBQueue` never blocks and we want to block until
-- there is one item or more.
-- Use this instead of STM.check to make sure it blocks if the queue is empty.
blockingFlushDbActionQueue :: DbActionQueue -> IO [DbAction]
blockingFlushDbActionQueue (DbActionQueue queue) = do
  STM.atomically $ do
    x <- TBQ.readTBQueue queue
    xs <- TBQ.flushTBQueue queue
    pure $ x : xs
