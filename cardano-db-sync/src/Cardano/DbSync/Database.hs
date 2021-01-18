{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Database
  ( DbAction (..)
  , DbActionQueue (..)
  , lengthDbActionQueue
  , mkDbApply
  , mkDbRollback
  , newDbActionQueue
  , runDbStartup
  , runDbThread
  , writeDbActionQueue
  ) where

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo)
import           Cardano.Prelude
import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Config
import           Cardano.DbSync.DbAction
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Database.Persist.Sql (SqlBackend)

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge


data NextState
  = Continue
  | Done
  deriving Eq


runDbStartup :: Trace IO Text -> DbSyncNodePlugin -> IO ()
runDbStartup trce plugin =
  DB.runDbAction (Just trce) $
    mapM_ (\action -> action trce) $ plugOnStartup plugin

runDbThread
    :: Trace IO Text -> DbSyncEnv -> DbSyncNodePlugin -> Metrics
    -> DbActionQueue -> LedgerStateVar
    -> IO ()
runDbThread trce env plugin metrics queue ledgerStateVar = do
    logInfo trce "Running DB thread"
    logException trce "runDBThread: " loop
    logInfo trce "Shutting down DB thread"
  where
    loop = do
      xs <- blockingFlushDbActionQueue queue
      when (length xs > 1) $ do
        logDebug trce $ "runDbThread: " <> textShow (length xs) <> " blocks"
      eNextState <- runExceptT $ runActions trce env plugin ledgerStateVar xs
      mBlkNo <-  DB.runDbAction (Just trce) DB.queryLatestBlockNo
      case mBlkNo of
        Nothing -> pure ()
        Just blkNo -> Gauge.set (fromIntegral blkNo) $ mDbHeight metrics
      case eNextState of
        Left err -> logError trce $ renderDbSyncNodeError err
        Right Continue -> loop
        Right Done -> pure ()

-- | Run the list of 'DbAction's. Block are applied in a single set (as a transaction)
-- and other operations are applied one-by-one.
runActions
    :: Trace IO Text -> DbSyncEnv -> DbSyncNodePlugin -> LedgerStateVar -> [DbAction]
    -> ExceptT DbSyncNodeError IO NextState
runActions trce env plugin ledgerState actions = do
    dbAction Continue actions
  where
    dbAction :: NextState -> [DbAction] -> ExceptT DbSyncNodeError IO NextState
    dbAction next [] = pure next
    dbAction Done _ = pure Done
    dbAction Continue xs =
      case spanDbApply xs of
        ([], DbFinish:_) -> do
            pure Done
        ([], DbRollBackToPoint pt:ys) -> do
            runRollbacks trce plugin (dbpSlot pt)
            liftIO $ loadLedgerStateAtPoint (envLedgerStateDir env) ledgerState pt
            dbAction Continue ys
        (ys, zs) -> do
          insertBlockList trce env ledgerState plugin ys
          if null zs
            then pure Continue
            else dbAction Continue zs

runRollbacks
    :: Trace IO Text -> DbSyncNodePlugin -> SlotNo
    -> ExceptT DbSyncNodeError IO ()
runRollbacks trce plugin point =
  newExceptT
    . traverseMEither (\ f -> f trce point)
    $ plugRollbackBlock plugin

insertBlockList
    :: Trace IO Text -> DbSyncEnv -> LedgerStateVar -> DbSyncNodePlugin -> [BlockDetails]
    -> ExceptT DbSyncNodeError IO ()
insertBlockList trce env ledgerState plugin blks =
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise is *way* too chatty.
  newExceptT
    . DB.runDbAction (Just trce)
    $ traverseMEither insertBlock blks
  where
    insertBlock
        :: BlockDetails
        -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
    insertBlock blkTip =
      traverseMEither (\ f -> f trce env ledgerState blkTip) $ plugInsertBlock plugin

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([BlockDetails], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt:xs) -> let (ys, zs) = spanDbApply xs in (bt:ys, zs)
    xs -> ([], xs)
