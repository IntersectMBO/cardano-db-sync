{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.DbAction (
  DbAction (..),
  ThreadChannels (..),
  blockingFlushDbActionQueue,
  lengthDbActionQueue,
  mkDbApply,
  newThreadChannels,
  writeDbActionQueue,
  waitRollback,
  waitRestartState,
  waitDoneInit,
  runAndSetDone,
) where

import Cardano.DbSync.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar, StrictTVar, newEmptyTMVarIO, newTVarIO, readTVar, readTVarIO, takeTMVar, writeTVar)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Ouroboros.Network.Block (BlockNo, Tip (..))
import qualified Ouroboros.Network.Point as Point

data DbAction
  = DbApplyBlock !CardanoBlock
  | DbRollBackToPoint !CardanoPoint !(Tip CardanoBlock) (StrictTMVar IO (Maybe [CardanoPoint], Point.WithOrigin BlockNo))
  | DbRestartState (StrictTMVar IO ([(CardanoPoint, Bool)], Point.WithOrigin BlockNo))
  | DbFinish

data ThreadChannels = ThreadChannels
  { tcQueue :: TBQueue DbAction
  , tcDoneInit :: !(StrictTVar IO Bool)
  }

mkDbApply :: CardanoBlock -> DbAction
mkDbApply = DbApplyBlock

-- | This simulates a synhronous operations, since the thread waits for the db
-- worker thread to finish the rollback.
waitRollback :: ThreadChannels -> CardanoPoint -> Tip CardanoBlock -> IO (Maybe [CardanoPoint], Point.WithOrigin BlockNo)
waitRollback tc point serverTip = do
  resultVar <- newEmptyTMVarIO
  atomically $ writeDbActionQueue tc $ DbRollBackToPoint point serverTip resultVar
  atomically $ takeTMVar resultVar

waitRestartState :: ThreadChannels -> IO ([(CardanoPoint, Bool)], Point.WithOrigin BlockNo)
waitRestartState tc = do
  resultVar <- newEmptyTMVarIO
  atomically $ do
    _ <- TBQ.flushTBQueue (tcQueue tc)
    writeDbActionQueue tc $ DbRestartState resultVar
  atomically $ takeTMVar resultVar

waitDoneInit :: ThreadChannels -> IO ()
waitDoneInit tc = atomically $ do
  isDone <- readTVar (tcDoneInit tc)
  unless isDone retry

runAndSetDone :: ThreadChannels -> IO Bool -> IO Bool
runAndSetDone tc action = do
  isDone <- readTVarIO (tcDoneInit tc)
  if isDone
    then pure True
    else do
      fl <- action
      atomically $ writeTVar (tcDoneInit tc) fl
      pure fl

lengthDbActionQueue :: ThreadChannels -> STM Natural
lengthDbActionQueue = STM.lengthTBQueue . tcQueue

newThreadChannels :: IO ThreadChannels
newThreadChannels =
  -- Use an odd number here so that the db_tip_height metric increments by this odd number
  -- when syncing, instead of incrementing by say 100.
  -- The pipeline queue in the LocalChainSync machinery is 50 elements long
  -- so we should not exceed that.
  ThreadChannels
    <$> TBQ.newTBQueueIO 47
    <*> newTVarIO False

writeDbActionQueue :: ThreadChannels -> DbAction -> STM ()
writeDbActionQueue = TBQ.writeTBQueue . tcQueue

-- | Block if the queue is empty and if its not read/flush everything.
-- Need this because `flushTBQueue` never blocks and we want to block until
-- there is one item or more.
-- Use this instead of STM.check to make sure it blocks if the queue is empty.
blockingFlushDbActionQueue :: ThreadChannels -> IO [DbAction]
blockingFlushDbActionQueue tc = do
  STM.atomically $ do
    x <- TBQ.readTBQueue $ tcQueue tc
    xs <- TBQ.flushTBQueue $ tcQueue tc
    pure $ x : xs
