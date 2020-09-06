{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.DbSync.DbAction
  ( DbAction (..)
  , DbActionQueue (..)
  , MkDbAction (..)
  , blockingFlushDbActionQueue
  , lengthDbActionQueue
  , newDbActionQueue
  , writeDbActionQueue
  ) where

import           Cardano.Prelude

import           Cardano.DbSync.Types
import           Cardano.Slotting.Slot (SlotNo (..), withOrigin)

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Network.Block (Point (..), pointSlot)


data DbAction
  = DbApplyBlock !BlockDetails
  | DbRollBackToPoint !SlotNo
  | DbFinish


newtype DbActionQueue = DbActionQueue
  { dbActQueue :: TBQueue DbAction
  }

class MkDbAction blk where
  mkDbApply :: blk -> SlotDetails -> DbAction
  mkDbRollback :: Point blk -> DbAction


instance MkDbAction ByronBlock where
  mkDbApply blk details = DbApplyBlock (ByronBlockDetails blk details)
  mkDbRollback point = DbRollBackToPoint (toRollbackSlot point)

instance MkDbAction (ShelleyBlock TPraosStandardCrypto) where
  mkDbApply blk details = DbApplyBlock (ShelleyBlockDetails blk details)
  mkDbRollback point = DbRollBackToPoint (toRollbackSlot point)

instance MkDbAction CardanoBlock where
  mkDbApply cblk details = do
    case cblk of
      BlockByron blk -> DbApplyBlock (ByronBlockDetails blk details)
      BlockShelley blk -> DbApplyBlock (ShelleyBlockDetails blk details)

  mkDbRollback point =
      DbRollBackToPoint (toRollbackSlot point)


-- The Point data type is probably really convenient in the libraries where it is defined
-- and used but is a huge pain in the neck here in db-sync.
-- A Point contains a SlotNo and a hash, but the hashes on Byron and Shelley are different
-- and its an incredible pain in the neck extracting the hash in a way that works for
-- Byron, Shelley and Cardano/HFC. Since the hash was only used for the log message anyway
-- its easier to just drop it.
toRollbackSlot :: Point blk -> SlotNo
toRollbackSlot = withOrigin (SlotNo 0) identity . pointSlot

lengthDbActionQueue :: DbActionQueue -> STM Natural
lengthDbActionQueue (DbActionQueue q) = STM.lengthTBQueue q

newDbActionQueue :: IO DbActionQueue
newDbActionQueue = DbActionQueue <$> TBQ.newTBQueueIO 2000

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
