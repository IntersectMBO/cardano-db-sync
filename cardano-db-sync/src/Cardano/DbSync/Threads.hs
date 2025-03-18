{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.DbSync.Threads where

import Cardano.BM.Trace (logInfo)
import Cardano.DbSync.Util
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Api
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.StakeDist
import Cardano.DbSync.Era.Universal.Epoch
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Cardano.DbSync.Ledger.Types
import Control.Monad
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Trans.Class
import Database.Persist.Postgresql (IsolationLevel (..), runSqlConnWithIsolation, withPostgresqlConn)
import Cardano.DbSync.Error
import Control.Monad.Trans.Except.Extra (runExceptT)

runStakeThread ::
  SyncEnv ->
  IO ()
runStakeThread syncEnv =
  case envLedgerEnv syncEnv of
    NoLedger _ -> pure ()
    HasLedger le -> do
      logInfo trce "Running Event thread"
      logException trce "runStakeThread: " (runStakeLoop syncEnv le)
      logInfo trce "Shutting Event thread"
  where
    trce = getTrace syncEnv

runStakeLoop :: SyncEnv -> HasLedgerEnv -> IO ()
runStakeLoop syncEnv lenv =
  DB.runIohkLogging trce $
    withPostgresqlConn (envConnectionString syncEnv) loop
  where
    loop backend = do
      runOrThrowIO $ runSqlConnWithIsolation (runExceptT loopAction) backend Serializable
      loop backend

    loopAction = do
      StakeDBAction epoch snapShot shouldCheck <- liftIO $ atomically $ TBQ.readTBQueue (stakeQueue stakeChan)
      if shouldCheck
        then do
          stakeExists <- lift $ DB.queryEpochStakeExists (unEpochNo epoch)
          unless stakeExists $ insertEpochStake syncEnv epoch (snapShotToList snapShot)
        else
          insertEpochStake syncEnv epoch (snapShotToList snapShot)
      liftIO $ atomically $ writeTVar (epochResult stakeChan) $ Just (epoch, Done)

    stakeChan = leStakeChans lenv
    trce = getTrace syncEnv
