{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Ledger.Async where

import Cardano.DbSync.Ledger.Types
import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.EpochBoundary as Ledger
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ

newEpochStakeChannels :: IO EpochStakeChannels
newEpochStakeChannels =
  -- This may never be more than 1. But let's keep it a queue for extensibility shake.
  -- This may allow us to parallelize the events workload even further
  EpochStakeChannels
    <$> TBQ.newTBQueueIO 1
    <*> newTVarIO Nothing

-- To be used by the main thread
ensureEpochDone :: EpochStakeChannels -> EpochNo -> Ledger.SnapShot StandardCrypto -> IO ()
ensureEpochDone sQueue epoch snapshot = atomically $ do
  mLastEpochDone <- waitFinished sQueue
  case mLastEpochDone of
    Just lastEpochDone | lastEpochDone == epoch -> pure ()
    _ -> do
      -- If last is not already there, put it to list and wait again
      writeEpochStakeAction sQueue epoch snapshot True
      retry

-- To be used by the main thread
waitFinished :: EpochStakeChannels -> STM IO (Maybe EpochNo)
waitFinished sQueue = do
  stakeThreadState <- readTVar (epochResult sQueue)
  case stakeThreadState of
    Just (lastEpoch, Done) -> pure $ Just lastEpoch -- Normal case
    Just (_, Running) -> retry -- Wait to finish current work.
    Nothing -> pure Nothing -- This will happen after a restart

-- To be used by the main thread
writeEpochStakeAction :: EpochStakeChannels -> EpochNo -> Ledger.SnapShot StandardCrypto -> Bool -> STM IO ()
writeEpochStakeAction sQueue epoch snapShot checkFirst = do
  TBQ.writeTBQueue (estakeQueue sQueue) $ EpochStakeDBAction epoch snapShot checkFirst
  writeTVar (epochResult sQueue) $ Just (epoch, Running)
