{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Ledger.Async where

import Cardano.Ledger.BaseTypes (EpochNo)
import Control.Concurrent.Class.MonadSTM.Strict -- (StrictTMVar, newEmptyTMVarIO, atomically, takeTMVar, putTMVar)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Cardano.Ledger.EpochBoundary as Ledger
import Cardano.DbSync.Ledger.Types
import Cardano.Ledger.Crypto (StandardCrypto)

newStakeChannels :: IO StakeChannels
newStakeChannels =
  -- This may never be more than 1. But let's keep it a queue for extensibility shake.
  -- This may allow us to parallelize the events workload even further
  StakeChannels
    <$> TBQ.newTBQueueIO 1
    <*> newTVarIO Nothing

-- To be used by the main thread
ensureEpochDone :: StakeChannels -> EpochNo -> Ledger.SnapShot StandardCrypto -> IO ()
ensureEpochDone sQueue epoch snapshot = atomically $ do
  mLastEpochDone <- waitFinished sQueue
  case mLastEpochDone of
    Just lastEpochDone | lastEpochDone == epoch -> pure ()
    _ -> do
      -- If last is not already there, put it to list and wait again
      writeStakeAction sQueue epoch snapshot True
      retry

-- To be used by the main thread
waitFinished :: StakeChannels -> STM IO (Maybe EpochNo)
waitFinished sQueue = do
  stakeThreadState <- readTVar (epochResult sQueue)
  case stakeThreadState of
    Just (lastEpoch, Done) -> pure $ Just lastEpoch -- Normal case
    Just (_, Running) -> retry -- Wait to finish current work.
    Nothing -> pure Nothing -- This will happen after a restart

-- To be used by the main thread
writeStakeAction :: StakeChannels -> EpochNo -> Ledger.SnapShot StandardCrypto -> Bool -> STM IO ()
writeStakeAction sQueue epoch snapShot checkFirst = do
  TBQ.writeTBQueue (stakeQueue sQueue) $ StakeDBAction epoch snapShot checkFirst
  writeTVar (epochResult sQueue) $ Just (epoch, Running)
