{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Threads.Rewards where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Era.Shelley.Generic.Rewards
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Era.Universal.Insert.LedgerEvent
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.Util
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Data.Map as Map
import Database.Persist.Postgresql (IsolationLevel (..), runSqlConnWithIsolation, withPostgresqlConn)

runRewardsThread ::
  SyncEnv ->
  IO ()
runRewardsThread syncEnv =
  case envLedgerEnv syncEnv of
    NoLedger _ -> pure ()
    HasLedger le -> do
      logInfo trce "Running Rewards thread"
      logException trce "runRewardsThread: " (runRewLoop syncEnv le)
      logInfo trce "Shutting Rewards thread"
  where
    trce = getTrace syncEnv

runRewLoop :: SyncEnv -> HasLedgerEnv -> IO ()
runRewLoop syncEnv lenv =
  DB.runIohkLogging trce $
    withPostgresqlConn (envConnectionString syncEnv) loop
  where
    loop backend = do
      runOrThrowIO $ runSqlConnWithIsolation (runExceptT loopAction) backend Serializable
      loop backend

    loopAction = do
      rAction <- liftIO $ atomically $ TBQ.readTBQueue (rQueue rc)
      case rAction of
        RewardsDBAction epoch mp _shouldCheck -> do
          insertRewards syncEnv epoch (Map.toList $ unRewards $ convertPoolRewards mp)
          liftIO $ atomically $ writeTVar (rewardsResult rc) $ Just (epoch, RewDone)
        RewardsEpochBoundary epoch events -> do
          insertNewEpochLedgerEvents syncEnv epoch events
          liftIO $ atomically $ writeTVar (rewardsResult rc) $ Just (epoch, RewEBDone)

    rc = leRewardsChans lenv
    trce = getTrace syncEnv
