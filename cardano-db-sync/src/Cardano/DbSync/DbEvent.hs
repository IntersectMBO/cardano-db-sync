{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.DbEvent (
  DbEvent (..),
  ThreadChannels (..),
  liftFail,
  liftFailEither,
  liftDbError,
  acquireDbConnection,
  blockingFlushDbEventQueue,
  lengthDbEventQueue,
  mkDbApply,
  newThreadChannels,
  writeDbEventQueue,
  waitRollback,
  waitRestartState,

  -- * Transaction and error handling utilities
  lift,
  runDbSyncTransaction,
  runDbSyncTransactionNoLogging,
  runDbSyncNoTransaction,
  runDbSyncNoTransactionNoLogging,
  runDbSyncTransactionPool,
) where

import Cardano.BM.Trace (Trace)
import qualified Cardano.Db as DB
import Cardano.DbSync.Error (SyncNodeCallStack, SyncNodeError (..), mkSyncNodeCallStack)
import Cardano.DbSync.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar, StrictTVar, newEmptyTMVarIO, newTVarIO, takeTMVar)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Connection.Setting as HsqlSet
import Ouroboros.Network.Block (BlockNo, Tip (..))
import qualified Ouroboros.Network.Point as Point

data DbEvent
  = DbApplyBlock !CardanoBlock
  | DbRollBackToPoint !CardanoPoint !(Tip CardanoBlock) !(StrictTMVar IO (Maybe [CardanoPoint], Point.WithOrigin BlockNo))
  | DbRestartState !(StrictTMVar IO ([(CardanoPoint, Bool)], Point.WithOrigin BlockNo))
  | DbFinish

data ThreadChannels = ThreadChannels
  { tcQueue :: !(TBQueue DbEvent)
  , tcDoneInit :: !(StrictTVar IO Bool)
  }

--------------------------------------------------------------------------------
-- Transaction and error handling utilities
--------------------------------------------------------------------------------

-- | Execute database operations in a single transaction using the main connection
--
-- This is the primary transaction runner for sequential database operations in db-sync.
-- All operations within the ExceptT stack are executed atomically in one database transaction.
--
-- == Transaction Behavior:
-- * Uses the main database connection from DbEnv for sequential operations
-- * All DbM operations are combined into a single Hasql session
-- * Entire transaction commits on success or rolls back on any failure
-- * Provides atomic all-or-nothing semantics for blockchain data consistency
--
-- == Error Handling:
-- * Captures full call stack with HasCallStack for precise error location
-- * Converts low-level Hasql SessionErrors to high-level SyncNodeErrors
-- * Returns Either for explicit error handling rather than throwing exceptions
-- * Database errors include 8-frame call chain showing exact failure path
--
-- == Usage:
-- * Primary use: insertListBlocks and other critical sync operations
-- * Sequential operations that must maintain strict consistency
-- * Operations where blocking the main connection is acceptable
--
-- == Example:
-- @
-- insertBlockWithValidation :: BlockData -> ExceptT SyncNodeError DB.DbM BlockId
-- insertBlockWithValidation blockData = do
--   liftIO $ logInfo tracer "Starting block insertion"
--   blockId <- lift $ insertBlock blockData  -- lift DbM to ExceptT
--   liftIO $ logDebug tracer $ "Inserted block with ID: " <> show blockId
--   pure blockId
--
-- result <- runDbSyncTransaction tracer dbEnv $ do
--   blockId <- insertBlockWithValidation blockData
--   lift $ updateSyncProgress blockId
--   pure blockId
-- -- All operations succeed together or all fail together
-- @
-- runDbSyncTransaction ::
--   forall m a.
--   (MonadUnliftIO m, HasCallStack) =>
--   Trace IO Text ->
--   DB.DbEnv ->
--   ExceptT SyncNodeError DB.DbM a ->
--   m (Either SyncNodeError a)
-- runDbSyncTransaction tracer dbEnv exceptTAction = do
--   let dbAction = runExceptT exceptTAction
--   eResult <- liftIO $ try $ DB.runDbDirectLogged tracer dbEnv dbAction
--   case eResult of
--     Left (dbErr :: DB.DbError) -> do
--       let cs = mkSyncNodeCallStack "runDbSyncTransaction"
--       pure $ Left $ SNErrDatabase cs dbErr
--     Right appResult -> pure appResult
runDbSyncTransaction ::
  forall m a.
  (MonadUnliftIO m, HasCallStack) =>
  Trace IO Text ->
  DB.DbEnv ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncTransaction tracer dbEnv exceptTAction = do
  -- OUTER TRY: Catch any exceptions from the entire database operation
  -- This includes connection errors, DB.DbError exceptions thrown from runDbTransLogged,
  -- or any other unexpected exceptions during database access
  eResult <- liftIO $ try $ DB.runDbTransLogged tracer dbEnv (runExceptT exceptTAction)
  case eResult of
    Left (dbErr :: DB.DbError) -> do
      let cs = mkSyncNodeCallStack "runDbSyncTransaction"
      pure $ Left $ SNErrDatabase cs dbErr
    Right appResult -> pure appResult

runDbSyncTransactionNoLogging ::
  forall m a.
  (MonadUnliftIO m, HasCallStack) =>
  DB.DbEnv ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncTransactionNoLogging dbEnv exceptTAction = do
  let dbAction = runExceptT exceptTAction
  eResult <- liftIO $ try $ DB.runDbTransSilent dbEnv dbAction
  case eResult of
    Left (dbErr :: DB.DbError) -> do
      let cs = mkSyncNodeCallStack "runDbSyncTransactionNoLogging"
      pure $ Left $ SNErrDatabase cs dbErr
    Right appResult -> pure appResult

runDbSyncNoTransaction ::
  forall m a.
  (MonadUnliftIO m, HasCallStack) =>
  Trace IO Text ->
  DB.DbEnv ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncNoTransaction tracer dbEnv exceptTAction = do
  eResult <- liftIO $ try $ DB.runDbDirectLogged tracer dbEnv (runExceptT exceptTAction)
  case eResult of
    Left (dbErr :: DB.DbError) -> do
      let cs = mkSyncNodeCallStack "runDbSyncNoTransaction"
      pure $ Left $ SNErrDatabase cs dbErr
    Right appResult -> pure appResult

runDbSyncNoTransactionNoLogging ::
  forall m a.
  (MonadUnliftIO m, HasCallStack) =>
  DB.DbEnv ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncNoTransactionNoLogging dbEnv exceptTAction = do
  let dbAction = runExceptT exceptTAction
  eResult <- liftIO $ try $ DB.runDbDirectSilent dbEnv dbAction
  case eResult of
    Left (dbErr :: DB.DbError) -> do
      let cs = mkSyncNodeCallStack "runDbSyncNoTransactionNoLogging"
      pure $ Left $ SNErrDatabase cs dbErr
    Right appResult -> pure appResult

-- | Execute database operations in a single transaction using the connection pool
runDbSyncTransactionPool ::
  (MonadUnliftIO m, HasCallStack) =>
  Trace IO Text ->
  DB.DbEnv ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncTransactionPool tracer dbEnv exceptTAction = do
  let dbAction = runExceptT exceptTAction
  eResult <- liftIO $ try $ DB.runDbPoolTransLogged tracer dbEnv dbAction -- Use pool
  case eResult of
    Left (dbErr :: DB.DbError) -> do
      let cs = mkSyncNodeCallStack "runDbSyncTransactionPool"
      pure $ Left $ SNErrDatabase cs dbErr
    Right appResult -> pure appResult

liftFail :: SyncNodeCallStack -> DB.DbM (Either DB.DbError a) -> ExceptT SyncNodeError DB.DbM a
liftFail cs dbAction = do
  result <- lift dbAction
  case result of
    Left dbErr -> throwError $ SNErrDatabase cs dbErr
    Right val -> pure val

liftFailEither :: SyncNodeCallStack -> ExceptT SyncNodeError DB.DbM (Either DB.DbError a) -> ExceptT SyncNodeError DB.DbM a
liftFailEither cs mResult = do
  resultE <- lift $ runExceptT mResult
  case resultE of
    Left err -> throwError $ SNErrDefault cs (show err)
    Right result -> case result of
      Left dbErr -> throwError $ SNErrDatabase cs dbErr
      Right val -> pure val

liftDbError :: ExceptT DB.DbError IO a -> ExceptT SyncNodeError IO a
liftDbError dbAction = do
  result <- liftIO $ runExceptT dbAction
  case result of
    Left dbErr -> throwError $ SNErrDatabase (mkSyncNodeCallStack "liftDbError") dbErr
    Right val -> pure val

acquireDbConnection :: [HsqlSet.Setting] -> IO HsqlC.Connection
acquireDbConnection settings = do
  result <- HsqlC.acquire settings
  case result of
    Left connErr -> throwIO $ SNErrDatabase (mkSyncNodeCallStack "acquireDbConnection") $ DB.DbError (show connErr)
    Right conn -> pure conn

mkDbApply :: CardanoBlock -> DbEvent
mkDbApply = DbApplyBlock

-- | This simulates a synhronous operations, since the thread waits for the db
-- worker thread to finish the rollback.
waitRollback :: ThreadChannels -> CardanoPoint -> Tip CardanoBlock -> IO (Maybe [CardanoPoint], Point.WithOrigin BlockNo)
waitRollback tc point serverTip = do
  resultVar <- newEmptyTMVarIO
  atomically $ writeDbEventQueue tc $ DbRollBackToPoint point serverTip resultVar
  atomically $ takeTMVar resultVar

waitRestartState :: ThreadChannels -> IO ([(CardanoPoint, Bool)], Point.WithOrigin BlockNo)
waitRestartState tc = do
  resultVar <- newEmptyTMVarIO
  atomically $ do
    _ <- TBQ.flushTBQueue (tcQueue tc)
    writeDbEventQueue tc $ DbRestartState resultVar
  atomically $ takeTMVar resultVar

lengthDbEventQueue :: ThreadChannels -> STM Natural
lengthDbEventQueue = STM.lengthTBQueue . tcQueue

newThreadChannels :: IO ThreadChannels
newThreadChannels =
  -- Use an odd number here so that the db_tip_height metric increments by this odd number
  -- when syncing, instead of incrementing by say 100.
  -- The pipeline queue in the LocalChainSync machinery is 50 elements long
  -- so we should not exceed that.
  ThreadChannels
    <$> TBQ.newTBQueueIO 47
    <*> newTVarIO False

writeDbEventQueue :: ThreadChannels -> DbEvent -> STM ()
writeDbEventQueue = TBQ.writeTBQueue . tcQueue

-- | Block if the queue is empty and if its not read/flush everything.
-- Need this because `flushTBQueue` never blocks and we want to block until
-- there is one item or more.
-- Use this instead of STM.check to make sure it blocks if the queue is empty.
blockingFlushDbEventQueue :: ThreadChannels -> IO [DbEvent]
blockingFlushDbEventQueue tc = do
  STM.atomically $ do
    x <- TBQ.readTBQueue $ tcQueue tc
    xs <- TBQ.flushTBQueue $ tcQueue tc
    pure $ x : xs
