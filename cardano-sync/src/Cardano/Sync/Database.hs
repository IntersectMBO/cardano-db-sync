{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Sync.Database
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

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo)

import           Control.Monad.Extra (whenJust)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Cardano.Sync.Api
import           Cardano.Sync.DbAction
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Metrics
import           Cardano.Sync.Plugin
import           Cardano.Sync.Types
import           Cardano.Sync.Util

data NextState
  = Continue
  | Done
  deriving Eq

runDbStartup :: Trace IO Text -> SyncNodePlugin -> IO ()
runDbStartup trce plugin =
    mapM_ (\action -> action trce) $ plugOnStartup plugin

runDbThread
    :: Trace IO Text -> SyncEnv -> MetricSetters -> SyncNodePlugin -> DbActionQueue
    -> IO ()
runDbThread trce env metricsSetters plugin queue = do
    logInfo trce "Running DB thread"
    logException trce "runDBThread: " loop
    logInfo trce "Shutting down DB thread"
  where
    loop = do
      xs <- blockingFlushDbActionQueue queue

      when (length xs > 1) $ do
        logDebug trce $ "runDbThread: " <> textShow (length xs) <> " blocks"

      eNextState <- runExceptT $ runActions trce env plugin xs

      mBlock <- sdlGetLatestBlock (envDataLayer env)
      whenJust mBlock $ \ block -> do
        setDbBlockHeight metricsSetters $ bBlockNo block
        setDbSlotHeight metricsSetters $ bSlotNo block

      case eNextState of
        Left err -> logError trce $ renderSyncNodeError err
        Right Continue -> loop
        Right Done -> pure ()

-- | Run the list of 'DbAction's. Block are applied in a single set (as a transaction)
-- and other operations are applied one-by-one.
runActions
    :: Trace IO Text -> SyncEnv -> SyncNodePlugin -> [DbAction]
    -> ExceptT SyncNodeError IO NextState
runActions trce env plugin actions = do
    dbAction Continue actions
  where
    dbAction :: NextState -> [DbAction] -> ExceptT SyncNodeError IO NextState
    dbAction next [] = pure next
    dbAction Done _ = pure Done
    dbAction Continue xs =
      case spanDbApply xs of
        ([], DbFinish:_) -> do
            pure Done
        ([], DbRollBackToPoint pt:ys) -> do
            runRollbacks trce plugin pt
            liftIO $ loadLedgerStateAtPoint (envLedger env) pt
            dbAction Continue ys
        (ys, zs) -> do
          insertBlockList trce env plugin ys
          if null zs
            then pure Continue
            else dbAction Continue zs

runRollbacks
    :: Trace IO Text -> SyncNodePlugin -> CardanoPoint
    -> ExceptT SyncNodeError IO ()
runRollbacks trce plugin point =
  newExceptT
    . traverseMEither (\ f -> f trce point)
    $ plugRollbackBlock plugin

insertBlockList
    :: Trace IO Text -> SyncEnv -> SyncNodePlugin -> [CardanoBlock]
    -> ExceptT SyncNodeError IO ()
insertBlockList trce env plugin blks = do
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise is *way* too chatty.
  --newExceptT $ traverseMEither insertBlock blks
  blockDetails <- plugInsertBlock plugin trce env blks
  forM_ (plugInsertBlockDetails plugin) $ \f -> f trce env blockDetails

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([CardanoBlock], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt:xs) -> let (ys, zs) = spanDbApply xs in (bt:ys, zs)
    xs -> ([], xs)
