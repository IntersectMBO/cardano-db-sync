{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Cardano.DbSync.Threads.Ledger where

import Cardano.BM.Trace (logInfo)
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Control.Concurrent.Class.MonadSTM.Strict (
  atomically,
  newEmptyTMVarIO,
  newTMVarIO,
  writeTMVar,
 )
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

runLedgerThread ::
  SyncEnv ->
  IO ()
runLedgerThread syncEnv =
  case envLedgerEnv syncEnv of
    NoLedger _ -> pure ()
    HasLedger le -> do
      logInfo trce "Running Event thread"
      logException trce "runEpochStakeThread: " (runLedgerLoop syncEnv le)
      logInfo trce "Shutting Event thread"
  where
    trce = getTrace syncEnv

runLedgerLoop :: SyncEnv -> HasLedgerEnv -> IO ()
runLedgerLoop syncEnv lenv = forever $ do
  LedgerAction cblk resVar <- liftIO $ atomically $ TBQ.readTBQueue (leApplyQueue lenv)
  res <- applyBlockAction syncEnv lenv cblk True
  atomically $ writeTMVar resVar res

-- May be used by 2 different thread. Not at the same time.
applyBlockAction :: SyncEnv -> HasLedgerEnv -> CardanoBlock -> Bool -> IO (ApplyResult, Bool)
applyBlockAction syncEnv lenv cblk isCons = do
  (applyRes, tookSnapshot) <- applyBlockAndSnapshot lenv cblk isCons
  applyRes' <- addNewEventsAndSort syncEnv applyRes
  pure (applyRes', tookSnapshot)

-- Not used by the Ledger thread
noLedgerAction :: SyncEnv -> NoLedgerEnv -> CardanoBlock -> IO ApplyResult
noLedgerAction syncEnv nle cblk = do
  slotDetails <- getSlotDetailsNode nle (cardanoBlockSlotNo cblk)
  addNewEventsAndSort syncEnv $ defaultApplyResult slotDetails

-- Not used by the Ledger thread
writeLedgerAction :: HasLedgerEnv -> CardanoBlock -> IO LedgerResultResTMVar
writeLedgerAction lenv cblock = do
  resVar <- newEmptyTMVarIO
  atomically $ TBQ.writeTBQueue (leApplyQueue lenv) $ LedgerAction cblock resVar
  pure resVar

-- Not used by the Ledger thread
asyncApplyResult :: SyncEnv -> CardanoBlock -> IO LedgerResultResTMVar
asyncApplyResult syncEnv cblk =
  case envLedgerEnv syncEnv of
    HasLedger hle -> writeLedgerAction hle cblk
    NoLedger nle -> do
      applyRes <- noLedgerAction syncEnv nle cblk
      newTMVarIO (applyRes, False)

-- Not used by the Ledger thread. This doesn't even send something to the thread.
mkApplyResult :: SyncEnv -> CardanoBlock -> IO (ApplyResult, Bool)
mkApplyResult syncEnv cblk = do
  case envLedgerEnv syncEnv of
    HasLedger hle -> applyBlockAction syncEnv hle cblk False
    NoLedger nle -> (,False) <$> noLedgerAction syncEnv nle cblk
