{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Ledger.Async where

import Cardano.DbSync.Era.Shelley.Generic.Rewards as Generic
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.Types
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.EpochBoundary as Ledger
import qualified Cardano.Ledger.Rewards as Ledger
import Cardano.Ledger.Shelley.RewardUpdate as Ledger
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad.Extra (whenJust)
import Data.Map (Map)
import Data.Set (Set)

--------------------------------------------------------------------------------
-- EpochStake
--------------------------------------------------------------------------------

newEpochStakeChannels :: IO EpochStakeChannels
newEpochStakeChannels =
  -- This may never be more than 1. But let's keep it a queue for extensibility shake.
  -- This may allow us to parallelize the events workload even further
  EpochStakeChannels
    <$> TBQ.newTBQueueIO 1
    <*> newTVarIO Nothing

-- To be used by the main thread
ensureStakeDone :: EpochStakeChannels -> EpochNo -> Ledger.SnapShot StandardCrypto -> IO ()
ensureStakeDone esc epoch snapshot = do
  mLastEpochDone <- atomically $ waitStakeFinished esc
  case mLastEpochDone of
    Just lastEpochDone | lastEpochDone >= epoch -> pure ()
    _ -> do
      -- If last is not already there, put it to list and wait again
      atomically $ writeEpochStakeAction esc epoch snapshot True -- TODO: do outside STM
      _ <- atomically $ waitStakeFinished esc
      pure ()

-- To be used by the main thread. Only blocks if it's 'Running', until it finishes.
waitStakeFinished :: EpochStakeChannels -> STM IO (Maybe EpochNo)
waitStakeFinished esc = do
  stakeThreadState <- readTVar (epochResult esc)
  case stakeThreadState of
    Just (lastEpoch, Done) -> pure $ Just lastEpoch -- Normal case
    Just (_, Running) -> retry -- Wait to finish current work.
    Nothing -> pure Nothing -- This will happen after a restart

-- To be used by the main thread
writeEpochStakeAction :: EpochStakeChannels -> EpochNo -> Ledger.SnapShot StandardCrypto -> Bool -> STM IO ()
writeEpochStakeAction esc epoch snapShot checkFirst = do
  TBQ.writeTBQueue (estakeQueue esc) $ EpochStakeDBAction epoch snapShot checkFirst
  writeTVar (epochResult esc) $ Just (epoch, Running)

--------------------------------------------------------------------------------
-- Rewards
--------------------------------------------------------------------------------

newRewardsChannels :: IO RewardsChannels
newRewardsChannels =
  RewardsChannels
    <$> TBQ.newTBQueueIO 5
    <*> newTVarIO Nothing

asyncWriteRewards :: HasLedgerEnv -> CardanoLedgerState -> EpochNo -> Bool -> [LedgerEvent] -> IO ()
asyncWriteRewards env newState currentEpochNo isNewEpoch rewardEventsEB = do
  rewState <- atomically $ readTVar $ rewardsResult rc
  if isNewEpoch
    then do
      case rewState of
        Just (e', RewRunning) | e' == currentEpochNo -> do
          waitRewardUntil rc (e', RewDone)
        _ -> do
          ensureRewardsDone rc currentEpochNo (findTotal rewardEventsEB)
      waitEBRewardsAction rc currentEpochNo rewardEventsEB
    else do
      case rewState of
        Just (e', _) | e' >= currentEpochNo -> pure ()
        _ ->
          whenJust (Generic.getRewardsUpdate (getTopLevelconfigHasLedger env) (clsState newState)) $ \ru -> do
            atomically $ writeRewardsAction rc currentEpochNo False (Ledger.rs ru) -- (e-1) (e+1)
  where
    rc = leRewardsChans env

    findTotal :: [LedgerEvent] -> Maybe (Map StakeCred (Set (Ledger.Reward StandardCrypto)))
    findTotal [] = Nothing
    findTotal (LedgerTotalRewards _ mp : _) = Just mp
    findTotal (_ : rest) = findTotal rest

-- To be used by the main thread
ensureRewardsDone :: RewardsChannels -> EpochNo -> Maybe (Map StakeCred (Set (Ledger.Reward StandardCrypto))) -> IO ()
ensureRewardsDone rc epoch mmp = do
  whenJust mmp $ \mp -> do
    atomically $ writeRewardsAction rc epoch True mp
    waitRewardUntil rc (epoch, RewDone)

waitEBRewardsAction :: RewardsChannels -> EpochNo -> [LedgerEvent] -> IO ()
waitEBRewardsAction rc epoch les = do
  atomically $ do
    TBQ.writeTBQueue (rQueue rc) $ RewardsEpochBoundary epoch les
    writeTVar (rewardsResult rc) $ Just (epoch, RewEBRunning)
  waitRewardUntil rc (epoch, RewEBDone)

-- To be used by the main thread
writeRewardsAction :: RewardsChannels -> EpochNo -> Bool -> Map StakeCred (Set (Ledger.Reward StandardCrypto)) -> STM IO ()
writeRewardsAction rc epoch checkFirst mp = do
  TBQ.writeTBQueue (rQueue rc) $ RewardsDBAction epoch mp checkFirst
  writeTVar (rewardsResult rc) $ Just (epoch, RewRunning)

waitRewardUntil :: RewardsChannels -> (EpochNo, EpochRewardState) -> IO ()
waitRewardUntil rc st = waitRewardUntilPred rc (== st)

-- blocks until the reward result satisfies a specific predicate.
waitRewardUntilPred :: RewardsChannels -> ((EpochNo, EpochRewardState) -> Bool) -> IO ()
waitRewardUntilPred rc prd = atomically $ do
  rewardsThreadState <- readTVar (rewardsResult rc)
  case rewardsThreadState of
    Just st | prd st -> pure ()
    _ -> retry
