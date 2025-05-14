{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Threads.Stake where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Stake
import Cardano.DbSync.Cache.Types
import Cardano.DbSync.Util
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (IsolationLevel (..), runSqlConnWithIsolation, withPostgresqlConn)

runStakeThread :: SyncEnv -> IO ()
runStakeThread syncEnv = do
  logInfo trce "Running Event thread"
  logException trce "runStakeThread: " (runStakeLoop syncEnv)
  logInfo trce "Shutting Event thread"
  where
    trce = getTrace syncEnv

runStakeLoop :: SyncEnv -> IO ()
runStakeLoop syncEnv =
  DB.runIohkLogging trce $
    withPostgresqlConn (envConnectionString syncEnv) actionDB
  where
    actionDB backend = runSqlConnWithIsolation (forever loopAction) backend Serializable

    loopAction = do
      action <- liftIO $ atomically $ TBQ.readTBQueue (scPriorityQueue stakeChan)
      case action of
        QueryInsertStake rewardAcc ca resVar -> do
          stakeId <- resolveInsertRewardAccount syncEnv ca rewardAcc
          liftIO $ atomically $ writeTMVar resVar stakeId
        CacheStake {} -> pure ()
        BulkPrefetchStake _ -> pure ()
        CommitStake retVar -> do
          DB.transactionCommit
          liftIO $ atomically $ writeTMVar retVar ()

    stakeChan = envStakeChans syncEnv
    trce = getTrace syncEnv
