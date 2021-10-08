{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Database
  ( DbAction (..)
  , DbActionQueue (..)
  , lengthDbActionQueue
  , mkDbApply
  , newDbActionQueue
  , runDbStartup
  , runDbThread
  , writeDbActionQueue
  ) where

import           Cardano.Prelude hiding (atomically)

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo)

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Extra (whenJust)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Cardano.DbSync.Api
import           Cardano.DbSync.DbAction
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util hiding (whenJust)

import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended

import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Point (blockPointHash, blockPointSlot)

import           System.IO.Error

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
        ([], DbRollBackToPoint pt resultVar : ys) -> do
            runRollbacksDB trce plugin pt
            res <- lift $ rollbackLedger trce plugin env pt
            lift $ atomically $ putTMVar resultVar res
            dbAction Continue ys
        (ys, zs) -> do
          insertBlockList trce env plugin ys
          if null zs
            then pure Continue
            else dbAction Continue zs

rollbackLedger :: Trace IO Text -> SyncNodePlugin -> SyncEnv -> CardanoPoint -> IO (Maybe [CardanoPoint])
rollbackLedger _trce _plugin env point = do
    mst <- loadLedgerAtPoint (envLedger env) point
    dbBlock <- sdlGetLatestBlock (envDataLayer env)
    case mst of
      Right st -> do
        checkDBWithState point dbBlock
        let statePoint = headerStatePoint $ headerState $ clsState st
        -- This check should always succeed, since 'loadLedgerAtPoint' returns succesfully.
        -- we just leave it here for extra validation.
        checkDBWithState statePoint dbBlock
        pure Nothing
      Left lsfs -> do
        points <- verifyFilePoints env lsfs
        pure $ Just points
  where

    checkDBWithState :: CardanoPoint -> Maybe Block -> IO ()
    checkDBWithState pnt blk =
      if compareTips pnt blk
        then pure ()
        else throwIO . userError $
                mconcat
                    [ "Ledger state point ", show pnt, " and db tip "
                    , show blk, " don't match"
                    ]

compareTips :: CardanoPoint -> Maybe Block -> Bool
compareTips = go
  where
    go (Point Origin) Nothing = True
    go (Point (At blk)) (Just tip) =
         getHeaderHash (blockPointHash blk) == bHash tip
      && blockPointSlot blk == bSlotNo tip
    go  _ _ = False

runRollbacksDB
    :: Trace IO Text -> SyncNodePlugin -> CardanoPoint
    -> ExceptT SyncNodeError IO ()
runRollbacksDB trce plugin point =
  newExceptT
    . traverseMEither (\ f -> f trce point)
    $ plugRollbackBlock plugin

insertBlockList
    :: Trace IO Text -> SyncEnv -> SyncNodePlugin -> [BlockDetails]
    -> ExceptT SyncNodeError IO ()
insertBlockList trce env plugin blks =
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise is *way* too chatty.
  --newExceptT $ traverseMEither insertBlock blks
  newExceptT
    . traverseMEither (\ f -> f trce env blks)
    $ plugInsertBlock plugin

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([BlockDetails], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt:xs) -> let (ys, zs) = spanDbApply xs in (bt:ys, zs)
    xs -> ([], xs)
