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
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util hiding (whenJust)

import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Consensus.HeaderValidation hiding (TipInfo)
import           Ouroboros.Consensus.Ledger.Extended

import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Point (blockPointHash, blockPointSlot)

import           System.IO.Error

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

      mBlock <- getDbLatestBlockInfo (envBackend env)
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
        ([], DbRollBackToPoint pt resultVar : ys) -> do
            runRollbacksDB env pt
            points <- lift $ rollbackLedger env pt
            blockNo <- lift $ getDbTipBlockNo env
            lift $ atomically $ putTMVar resultVar (points, blockNo)
            dbAction Continue ys
        (ys, zs) -> do
          insertBlockList env ys
          if null zs
            then pure Continue
            else dbAction Continue zs

rollbackLedger :: SyncEnv -> CardanoPoint -> IO (Maybe [CardanoPoint])
rollbackLedger env point = do
    mst <- loadLedgerAtPoint (envLedger env) point
    dbTipInfo <- getDbLatestBlockInfo (envBackend env)
    case mst of
      Right st -> do
        checkDBWithState point dbTipInfo
        let statePoint = headerStatePoint $ headerState $ clsState st
        -- This check should always succeed, since 'loadLedgerAtPoint' returns succesfully.
        -- we just leave it here for extra validation.
        checkDBWithState statePoint dbTipInfo
        pure Nothing
      Left lsfs ->
        Just <$> verifyFilePoints env lsfs
  where

    checkDBWithState :: CardanoPoint -> Maybe TipInfo -> IO ()
    checkDBWithState pnt dbTipInfo =
      if compareTips pnt dbTipInfo
        then pure ()
        else throwIO . userError $
                mconcat
                    [ "Ledger state point ", show pnt, " and db tip "
                    , show dbTipInfo, " don't match"
                    ]

compareTips :: CardanoPoint -> Maybe TipInfo -> Bool
compareTips = go
  where
    go (Point Origin) Nothing = True
    go (Point (At blk)) (Just tip) =
         getHeaderHash (blockPointHash blk) == bHash tip
      && blockPointSlot blk == bSlotNo tip
    go  _ _ = False

runRollbacksDB
    :: SyncEnv -> CardanoPoint
    -> ExceptT SyncNodeError IO ()
runRollbacksDB env point =
  newExceptT $ rollbackToPoint (envBackend env) (getTrace env) point

insertBlockList
    :: SyncEnv -> [CardanoBlock]
    -> ExceptT SyncNodeError IO ()
insertBlockList env blks =
  newExceptT $ insertDefaultBlock env blks

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([CardanoBlock], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt:xs) -> let (ys, zs) = spanDbApply xs in (bt:ys, zs)
    xs -> ([], xs)
