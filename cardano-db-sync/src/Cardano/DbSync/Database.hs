{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Database (
  DbAction (..),
  ThreadChannels,
  lengthDbActionQueue,
  mkDbApply,
  runDbThread,
) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logDebug, logError, logInfo)
import Cardano.DbSync.Api
import Cardano.DbSync.AppT (App, ConsistentLevel (..), LedgerEnv (..), SyncEnv (..), askTrace, runApp)
import Cardano.DbSync.DbAction
import Cardano.DbSync.Default
import Cardano.DbSync.Error (runOrThrowApp, throwAppError)
import Cardano.DbSync.Error.Types (SyncNodeError (..))
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types (CardanoLedgerState (..), SnapshotPoint (..))
import Cardano.DbSync.Metrics
import Cardano.DbSync.Rollback
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Extra (whenJust)
import qualified Data.Text as Text
import Ouroboros.Consensus.HeaderValidation hiding (TipInfo)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Network.Block (BlockNo, Point (..))
import Ouroboros.Network.Point (blockPointHash, blockPointSlot)

data NextState
  = Continue
  | Done
  deriving (Eq)

runDbThread ::
  MetricSetters ->
  ThreadChannels ->
  App ()
runDbThread metricsSetters queue = do
  trce <- askTrace
  liftIO $ logInfo trce "Running DB thread"
  logExceptions "runDBThread: " loop
  liftIO $ logInfo trce "Shutting down DB thread"
  where
    loop :: App ()
    loop = do
      syncEnv <- ask
      trce <- askTrace

      xs <- blockingFlushDbActionQueue queue

      when (length xs > 1) $ do
        liftIO $ logDebug trce $ "runDbThread: " <> textShow (length xs) <> " blocks"

      case hasRestart xs of
        Nothing -> do
          eNextState <- runActions syncEnv xs

          mBlock <- getDbLatestBlockInfo (envBackend syncEnv)
          whenJust mBlock $ \block -> do
            liftIO $ setDbBlockHeight metricsSetters $ bBlockNo block
            liftIO $ setDbSlotHeight metricsSetters $ bSlotNo block

          case eNextState of
            Continue -> loop
            Done -> pure ()
        Just resultVar -> do
          -- In this case the syncing thread has restarted, so ignore all blocks that are not
          -- inserted yet.
          liftIO $ logInfo trce "Chain Sync client thread has restarted"
          latestPoints <- getLatestPoints
          currentTip <- getCurrentTipBlockNo syncEnv
          logDbState
          liftIO $ atomically $ putTMVar resultVar (latestPoints, currentTip)
          loop

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logExceptions :: Text -> App () -> App ()
logExceptions context action = do
  syncEnv <- ask
  trce <- askTrace
  result <- liftIO $ try $ runApp syncEnv action
  case result of
    Left ex -> liftIO $ logError trce (context <> Text.pack (show (ex :: SomeException)))
    Right _ -> pure ()

-- | Run the list of 'DbAction's. Block are applied in a single set (as a transaction)
-- and other operations are applied one-by-one.
runActions ::
  SyncEnv ->
  [DbAction] ->
  App NextState
runActions syncEnv actions = do
  dbAction Continue actions
  where
    dbAction :: NextState -> [DbAction] -> App NextState
    dbAction next [] = pure next
    dbAction Done _ = pure Done
    dbAction Continue xs = do
      tracer <- askTrace
      case spanDbApply xs of
        ([], DbFinish : _) -> do
          pure Done
        ([], DbRollBackToPoint chainSyncPoint serverTip resultVar : ys) -> do
          deletedAllBlocks <- runOrThrowApp tracer $ prepareRollback chainSyncPoint serverTip
          points <- rollbackLedger syncEnv chainSyncPoint

          -- Ledger state always rollbacks at least back to the 'point' given by the Node.
          -- It needs to rollback even further, if 'points' is not 'Nothing'.
          -- The db may not rollback to the Node point.
          case (deletedAllBlocks, points) of
            (True, Nothing) -> do
              setConsistentLevel Consistent
              validateConsistentLevel tracer chainSyncPoint
            (False, Nothing) -> do
              setConsistentLevel DBAheadOfLedger
              validateConsistentLevel tracer chainSyncPoint
            _anyOtherOption ->
              -- No need to validate here
              setConsistentLevel DBAheadOfLedger
          blockNo <- getDbTipBlockNo syncEnv
          lift $ atomically $ putTMVar resultVar (points, blockNo)
          dbAction Continue ys
        (ys, zs) -> do
          insertListBlocks ys
          if null zs
            then pure Continue
            else dbAction Continue zs

rollbackLedger :: SyncEnv -> CardanoPoint -> App (Maybe [CardanoPoint])
rollbackLedger syncEnv point = do
  tracer <- askTrace
  case envLedgerEnv syncEnv of
    HasLedger hle -> do
      mst <- loadLedgerAtPoint hle point
      case mst of
        Right st -> do
          let statePoint = headerStatePoint $ headerState $ clsState st
          -- This is an extra validation that should always succeed.
          unless (point == statePoint) $
            throwAppError tracer $
              SNErrDatabaseRollBackLedger $
                mconcat
                  [ "Ledger "
                  , show statePoint
                  , " and ChainSync "
                  , show point
                  , " don't match."
                  ]
          pure Nothing
        Left lsfs ->
          Just . fmap fst <$> verifySnapshotPoint (OnDisk <$> lsfs)
    NoLedger _ -> pure Nothing

-- | This not only checks that the ledger and ChainSync points are equal, but also that the
-- 'Consistent' Level is correct based on the db tip.
validateConsistentLevel :: Trace IO Text -> CardanoPoint -> App ()
validateConsistentLevel tracer stPoint = do
  backend <- asks envBackend
  dbTipInfo <- getDbLatestBlockInfo backend
  cLevel <- getConsistentLevel
  compareTips stPoint dbTipInfo cLevel
  where
    compareTips :: CardanoPoint -> Maybe TipInfo -> ConsistentLevel -> App ()
    compareTips _ dbTip Unchecked = do
      throwAppError tracer $
        SNErrDatabaseValConstLevel $
          "Found Unchecked Consistent Level. " <> showContext dbTip Unchecked
    compareTips (Point Origin) Nothing Consistent = pure ()
    compareTips (Point Origin) _ DBAheadOfLedger = pure ()
    compareTips (Point (At blk)) (Just tip) Consistent
      | getHeaderHash (blockPointHash blk) == bHash tip
          && blockPointSlot blk == bSlotNo tip =
          pure ()
    compareTips (Point (At blk)) (Just tip) DBAheadOfLedger
      | blockPointSlot blk <= bSlotNo tip = pure ()
    compareTips _ dbTip cLevel =
      throwAppError tracer $
        SNErrDatabaseValConstLevel $
          "Unexpected Consistent Level. " <> showContext dbTip cLevel

    showContext dbTip cLevel =
      mconcat
        [ "Ledger state point is "
        , show stPoint
        , ". DB Tip is "
        , show dbTip
        , ". Consistent Level is "
        , show cLevel
        ]

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([CardanoBlock], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt : xs) -> let (ys, zs) = spanDbApply xs in (bt : ys, zs)
    xs -> ([], xs)

hasRestart :: [DbAction] -> Maybe (StrictTMVar IO ([(CardanoPoint, Bool)], WithOrigin BlockNo))
hasRestart = go
  where
    go [] = Nothing
    go (DbRestartState mvar : _) = Just mvar
    go (_ : rest) = go rest
