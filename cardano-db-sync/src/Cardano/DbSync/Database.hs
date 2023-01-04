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
  , runDbThread
  , writeDbActionQueue
  ) where

import           Cardano.Prelude hiding (atomically)

import           Cardano.BM.Trace (logDebug, logError, logInfo)

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Extra (whenJust)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Cardano.DbSync.Api
import           Cardano.DbSync.DbAction
import           Cardano.DbSync.Default
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Rollback
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Consensus.HeaderValidation hiding (TipInfo)
import           Ouroboros.Consensus.Ledger.Extended

import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Point (blockPointHash, blockPointSlot)

data NextState
  = Continue
  | Done
  deriving Eq

runDbThread
    :: SyncEnv -> MetricSetters -> DbActionQueue
    -> IO ()
runDbThread env metricsSetters queue = do
    logInfo trce "Running DB thread"
    logException trce "runDBThread: " loop
    logInfo trce "Shutting down DB thread"
  where
    trce = getTrace env
    loop = do
      xs <- blockingFlushDbActionQueue queue

      when (length xs > 1) $ do
        logDebug trce $ "runDbThread: " <> textShow (length xs) <> " blocks"

      eNextState <- runExceptT $ runActions env xs

      backend <- getBackend env
      mBlock <- getDbLatestBlockInfo backend
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
    :: SyncEnv -> [DbAction]
    -> ExceptT SyncNodeError IO NextState
runActions env actions = do
    dbAction Continue actions
  where
    dbAction :: NextState -> [DbAction] -> ExceptT SyncNodeError IO NextState
    dbAction next [] = pure next
    dbAction Done _ = pure Done
    dbAction Continue xs =
      case spanDbApply xs of
        ([], DbFinish:_) -> do
            pure Done
        ([], DbRollBackToPoint chainSyncPoint serverTip resultVar : ys) -> do
            deletedAllBlocks <- newExceptT $ prepareRollback env chainSyncPoint serverTip
            points <- if hasLedgerState env
              then lift $ rollbackLedger env chainSyncPoint
              else pure Nothing
            -- Ledger state always rollbacks at least back to the 'point' given by the Node.
            -- It needs to rollback even further, if 'points' is not 'Nothing'.
            -- The db may not rollback to the Node point.
            case (deletedAllBlocks, points) of
              (True, Nothing) -> do
                liftIO $ setConsistentLevel env Consistent
                liftIO $ validateConsistentLevel env chainSyncPoint
              (False, Nothing) -> do
                liftIO $ setConsistentLevel env DBAheadOfLedger
                liftIO $ validateConsistentLevel env chainSyncPoint
              _ ->
                -- No need to validate here
                liftIO $ setConsistentLevel env DBAheadOfLedger
            blockNo <- lift $ getDbTipBlockNo env
            lift $ atomically $ putTMVar resultVar (points, blockNo)
            dbAction Continue ys
        (ys, zs) -> do
          newExceptT $ insertListBlocks env ys
          if null zs
            then pure Continue
            else dbAction Continue zs

rollbackLedger :: SyncEnv -> CardanoPoint -> IO (Maybe [CardanoPoint])
rollbackLedger env point = do
    mst <- loadLedgerAtPoint (envLedger env) point
    case mst of
      Right st -> do
        let statePoint = headerStatePoint $ headerState $ clsState st
        -- This is an extra validation that should always succeed.
        unless (point == statePoint) $
          logAndPanic (getTrace env) $ mconcat
            [ "Ledger ", textShow statePoint
            , " and ChainSync ", textShow point , " don't match."
            ]
        pure Nothing
      Left lsfs ->
        Just . fmap fst <$> verifySnapshotPoint env (OnDisk <$> lsfs)

-- | This not only checks that the ledger and ChainSync points are equal, but also that the
-- 'Consistent' Level is correct based on the db tip.
validateConsistentLevel :: SyncEnv -> CardanoPoint -> IO ()
validateConsistentLevel env stPoint = do
    backend <- getBackend env
    dbTipInfo <- getDbLatestBlockInfo backend
    cLevel <- getConsistentLevel env
    compareTips stPoint dbTipInfo cLevel

    where
      compareTips _ dbTip Unchecked =
        logAndPanic tracer $
          "Found Unchecked Consistent Level. " <> showContext dbTip Unchecked
      compareTips (Point Origin) Nothing Consistent = pure ()
      compareTips (Point Origin) _ DBAheadOfLedger = pure ()
      compareTips (Point (At blk)) (Just tip) Consistent
          | getHeaderHash (blockPointHash blk) == bHash tip
            && blockPointSlot blk == bSlotNo tip = pure ()
      compareTips (Point (At blk)) (Just tip) DBAheadOfLedger
          | blockPointSlot blk <= bSlotNo tip = pure ()
      compareTips  _ dbTip cLevel =
          logAndPanic tracer $
            "Unexpected Consistent Level. " <> showContext dbTip cLevel

      tracer = getTrace env
      showContext dbTip cLevel = mconcat
        [ "Ledger state point is ", textShow stPoint
        , ". DB Tip is ", textShow dbTip
        , ". Consistent Level is ", textShow cLevel
        ]

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([CardanoBlock], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt:xs) -> let (ys, zs) = spanDbApply xs in (bt:ys, zs)
    xs -> ([], xs)
