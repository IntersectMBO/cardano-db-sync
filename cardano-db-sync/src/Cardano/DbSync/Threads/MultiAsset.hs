{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Threads.MultiAsset where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Types
import Cardano.DbSync.Era.Universal.Insert.Other
import Cardano.DbSync.Util
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (IsolationLevel (..), runSqlConnWithIsolation, withPostgresqlConn)

runMAThread :: SyncEnv -> IO ()
runMAThread syncEnv = do
  logInfo trce "Running Event thread"
  logException trce "runMAThread: " (runMALoop syncEnv)
  logInfo trce "Shutting Event thread"
  where
    trce = getTrace syncEnv

runMALoop :: SyncEnv -> IO ()
runMALoop syncEnv =
  DB.runIohkLogging trce $
    withPostgresqlConn (envConnectionString syncEnv) actionDB
  where
    actionDB backend = runSqlConnWithIsolation (forever loopAction) backend Serializable

    loopAction = do
      action <- liftIO $ atomically $ TBQ.readTBQueue (macPriorityQueue maChan)
      case action of
        QueryInsertMA policy name resVar -> do
          stakeId <- insertMultiAsset (envCache syncEnv) policy name
          liftIO $ atomically $ writeTMVar resVar stakeId
        CacheMA {} -> pure ()
        BulkPrefetchMA _ -> pure ()
        CommitMA -> DB.transactionCommit

    maChan = envMAChans syncEnv
    trce = getTrace syncEnv
