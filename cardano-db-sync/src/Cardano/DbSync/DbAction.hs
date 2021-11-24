{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.DbAction
  ( DbAction (..)
  , DbActionQueue (..)
  , blockingFlushDbActionQueue
  , lengthDbActionQueue
  , mkDbApply
  , newDbActionQueue
  , writeDbActionQueue
  , waitRollback
  ) where

import           Cardano.Prelude

import           Cardano.DbSync.Types

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ

import           Control.Monad.Class.MonadSTM.Strict (StrictTMVar, newEmptyTMVarIO, takeTMVar)

data DbAction
  = DbApplyBlock !BlockDetails
  | DbRollBackToPoint !CardanoPoint (StrictTMVar IO (Maybe [CardanoPoint]))
  | DbFinish

newtype DbActionQueue = DbActionQueue
  { dbActQueue :: TBQueue DbAction
  }

mkDbApply :: CardanoBlock -> SlotDetails -> DbAction
mkDbApply cblk details =
  DbApplyBlock (BlockDetails cblk details)

-- | This simulates a synhronous operations, since the thread waits for the db
-- worker thread to finish the rollback.
waitRollback :: DbActionQueue -> CardanoPoint -> IO (Maybe [CardanoPoint])
waitRollback queue point = do
    resultVar <- newEmptyTMVarIO
    atomically $ writeDbActionQueue queue $ DbRollBackToPoint point resultVar
    atomically $ takeTMVar resultVar

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
