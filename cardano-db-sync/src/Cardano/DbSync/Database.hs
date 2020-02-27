{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Database
  ( DbAction (..)
  , DbActionQueue (..)
  , lengthDbActionQueue
  , newDbActionQueue
  , runDbStartup
  , runDbThread
  , writeDbActionQueue
  ) where

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo)
import qualified Cardano.Chain.Block as Ledger
import           Cardano.Prelude

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (left, newExceptT, runExceptT)

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Metrics
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Network.Block (Point (..), Tip)

import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge


data NextState
  = Continue
  | Done
  deriving Eq

data DbAction
  = DbApplyBlock !ByronBlock !(Tip ByronBlock)
  | DbRollBackToPoint !(Point ByronBlock)
  | DbFinish

newtype DbActionQueue = DbActionQueue
  { dbActQueue :: TBQueue DbAction
  }

lengthDbActionQueue :: DbActionQueue -> STM Natural
lengthDbActionQueue (DbActionQueue q) = STM.lengthTBQueue q

newDbActionQueue :: IO DbActionQueue
newDbActionQueue = DbActionQueue <$> TBQ.newTBQueueIO 2000

writeDbActionQueue :: DbActionQueue -> DbAction -> STM ()
writeDbActionQueue (DbActionQueue q) = TBQ.writeTBQueue q


runDbStartup :: Trace IO Text -> DbSyncNodePlugin -> IO ()
runDbStartup trce plugin =
  DB.runDbAction (Just trce) $
    mapM_ (\action -> action trce) $ plugOnStartup plugin

runDbThread :: Trace IO Text -> DbSyncNodePlugin -> Metrics -> DbActionQueue -> IO ()
runDbThread trce plugin metrics queue = do
    logInfo trce "Running DB thread"
    logException trce "runDBThread: " loop
    logInfo trce "Shutting down DB thread"
  where
    loop = do
      xs <- blockingFlushDbActionQueue queue
      when (length xs > 1) $ do
        logDebug trce $ "runDbThread: " <> textShow (length xs) <> " blocks"
      eNextState <- runExceptT $ runActions trce plugin xs
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
runActions :: Trace IO Text -> DbSyncNodePlugin -> [DbAction] -> ExceptT DbSyncNodeError IO NextState
runActions trce plugin actions = do
    nextState <- checkDbState trce actions
    if nextState /= Done
      then dbAction Continue actions
      else pure Continue
  where
    dbAction :: NextState -> [DbAction] -> ExceptT DbSyncNodeError IO NextState
    dbAction next [] = pure next
    dbAction Done _ = pure Done
    dbAction Continue xs =
      case spanDbApply xs of
        ([], DbFinish:_) -> do
            pure Done
        ([], DbRollBackToPoint pt:ys) -> do
            runRollbacks trce plugin pt
            dbAction Continue ys
        (ys, zs) -> do
          insertBlockList trce plugin ys
          if null zs
            then pure Continue
            else dbAction Continue zs

checkDbState :: Trace IO Text -> [DbAction] -> ExceptT DbSyncNodeError IO NextState
checkDbState trce xs =
    case filter isMainBlockApply (reverse xs) of
      [] -> pure Continue
      (DbApplyBlock blk _tip : _) -> validateBlock blk
      _ -> pure Continue
  where
    validateBlock :: ByronBlock -> ExceptT DbSyncNodeError IO NextState
    validateBlock bblk = do
      case byronBlockRaw bblk of
        Ledger.ABOBBoundary _ -> left $ ENEError "checkDbState got a boundary block"
        Ledger.ABOBBlock chBlk -> do
          mDbBlk <- liftIO $ DB.runDbAction (Just trce) $ DB.queryBlockNo (blockNumber chBlk)
          case mDbBlk of
            Nothing -> pure Continue
            Just dbBlk -> do
              when (DB.blockHash dbBlk /= blockHash chBlk) $ do
                liftIO $ logInfo trce (textShow chBlk)
                -- liftIO $ logInfo trce (textShow dbBlk)
                left $ ENEBlockMismatch (blockNumber chBlk) (DB.blockHash dbBlk) (blockHash chBlk)

              liftIO . logInfo trce $
                mconcat [ "checkDbState: Block no ", textShow (blockNumber chBlk), " present" ]
              pure Done -- Block already exists, so we are done.

    isMainBlockApply :: DbAction -> Bool
    isMainBlockApply dba =
      case dba of
        DbApplyBlock blk _tip ->
          case byronBlockRaw blk of
            Ledger.ABOBBlock _ -> True
            Ledger.ABOBBoundary _ -> False
        DbRollBackToPoint _ -> False
        DbFinish -> False

runRollbacks
    :: Trace IO Text
    -> DbSyncNodePlugin
    -> Point ByronBlock
    -> ExceptT DbSyncNodeError IO ()
runRollbacks trce plugin point =
    newExceptT $ foldM rollback (Right ()) $ plugRollbackBlock plugin
  where
    rollback prevResult action =
      case prevResult of
        Left e -> pure $ Left e
        Right () -> action trce point

insertBlockList
    :: Trace IO Text
    -> DbSyncNodePlugin
    -> [(ByronBlock, Tip ByronBlock)]
    -> ExceptT DbSyncNodeError IO ()
insertBlockList trce plugin blks =
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise is *way* too chatty.
  newExceptT
    . DB.runDbAction (Just trce)
    $ foldM insertBlock (Right ()) blks
  where
    insertBlock
        :: Either DbSyncNodeError ()
        -> (ByronBlock, Tip ByronBlock)
        -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
    insertBlock prevResult (blk, blkNo) =
      case prevResult of
        Left e -> pure $ Left e
        Right () -> foldM (insertAction (blk, blkNo)) (Right ()) $ plugInsertBlock plugin

    insertAction
        :: (ByronBlock, Tip ByronBlock)
        -> Either DbSyncNodeError ()
        -> (Trace IO Text -> ByronBlock -> Tip ByronBlock -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ()))
        -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
    insertAction (blk, blkNo) prevResult action =
      case prevResult of
        Left e -> pure $ Left e
        Right () -> action trce blk blkNo

-- | Block if the queue is empty and if its not read/flush everything.
-- Need this because `flushTBQueue` never blocks and we want to block until
-- there is one item or more.
-- Use this instead of STM.check to make sure it blocks if the queue is empty.
blockingFlushDbActionQueue :: DbActionQueue -> IO [DbAction]
blockingFlushDbActionQueue (DbActionQueue queue) = do
  STM.atomically $ do
    x <- TBQ.readTBQueue queue
    xs <- TBQ.flushTBQueue queue
    pure $ x : xs

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([(ByronBlock, Tip ByronBlock)], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock b t:xs) -> let (ys, zs) = spanDbApply xs in ((b, t):ys, zs)
    xs -> ([], xs)
