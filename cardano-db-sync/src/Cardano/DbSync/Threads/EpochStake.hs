{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Threads.EpochStake where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Era.Shelley.Generic.StakeDist
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.Util
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except.Extra (runExceptT)
import Database.Persist.Postgresql (IsolationLevel (..), runSqlConnWithIsolation, withPostgresqlConn)

runEpochStakeThread ::
  SyncEnv ->
  IO ()
runEpochStakeThread syncEnv =
  case envLedgerEnv syncEnv of
    NoLedger _ -> pure ()
    HasLedger le -> do
      logInfo trce "Running Event thread"
      logException trce "runEpochStakeThread: " (runESLoop syncEnv le)
      logInfo trce "Shutting Event thread"
  where
    trce = getTrace syncEnv

runESLoop :: SyncEnv -> HasLedgerEnv -> IO ()
runESLoop syncEnv lenv =
  DB.runIohkLogging trce $
    withPostgresqlConn (envConnectionString syncEnv) loop
  where
    loop backend = do
      runOrThrowIO $ runSqlConnWithIsolation (runExceptT loopAction) backend Serializable
      loop backend

    loopAction = do
      EpochStakeDBAction epoch snapShot shouldCheck <- liftIO $ atomically $ TBQ.readTBQueue (estakeQueue estakeChan)
      if shouldCheck
        then do
          stakeExists <- lift $ DB.queryEpochStakeExists (unEpochNo epoch)
          unless stakeExists $ insertEpochStake syncEnv epoch (snapShotToList snapShot)
        else insertEpochStake syncEnv epoch (snapShotToList snapShot)
      liftIO $ atomically $ writeTVar (epochResult estakeChan) $ Just (epoch, Done)

    estakeChan = leEpochStakeChans lenv
    trce = getTrace syncEnv
