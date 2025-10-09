{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.DbEvent (
  DbEvent (..),
  ThreadChannels (..),
  liftDbSession,
  liftDbLookup,
  liftDbLookupMaybe,
  liftDbSessionEither,
  liftDbLookupEither,
  liftSessionIO,
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
-- Accepts an optional isolation level (Nothing uses RepeatableRead default).
runDbSyncTransaction ::
  forall m a.
  (MonadUnliftIO m, HasCallStack) =>
  Trace IO Text ->
  DB.DbEnv ->
  Maybe DB.IsolationLevel ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncTransaction tracer dbEnv mIsolationLevel exceptTAction = do
  -- Catch database exceptions and convert to Either
  eResult <- liftIO $ try $ DB.runDbTransLogged tracer dbEnv mIsolationLevel (runExceptT exceptTAction)
  case eResult of
    Left (dbErr :: DB.DbSessionError) -> do
      pure $ Left $ SNErrDbSessionErr mkSyncNodeCallStack dbErr
    Right appResult -> pure appResult

-- | Execute database operations in a single transaction without logging.
-- Same as runDbSyncTransaction but uses silent database runner.
runDbSyncTransactionNoLogging ::
  forall m a.
  (MonadUnliftIO m, HasCallStack) =>
  DB.DbEnv ->
  ExceptT SyncNodeError DB.DbM a ->
  m (Either SyncNodeError a)
runDbSyncTransactionNoLogging dbEnv exceptTAction = do
  let dbAction = runExceptT exceptTAction
  eResult <- liftIO $ try $ DB.runDbTransSilent dbEnv Nothing dbAction
  case eResult of
    Left (dbErr :: DB.DbSessionError) -> do
      pure $ Left $ SNErrDbSessionErr mkSyncNodeCallStack dbErr
    Right appResult -> pure appResult

-- | Execute database operations without transaction wrapper.
-- Operations run directly against the database without atomicity guarantees.
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
    Left (dbErr :: DB.DbSessionError) -> do
      pure $ Left $ SNErrDbSessionErr mkSyncNodeCallStack dbErr
    Right appResult -> pure appResult

-- | Execute database operations without transaction wrapper and without logging.
-- Direct database access with no atomicity guarantees or logging output.
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
    Left (dbErr :: DB.DbSessionError) -> do
      pure $ Left $ SNErrDbSessionErr mkSyncNodeCallStack dbErr
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
  eResult <- liftIO $ try $ DB.runDbPoolTransLogged tracer dbEnv Nothing dbAction -- Use pool
  case eResult of
    Left (dbErr :: DB.DbSessionError) -> do
      pure $ Left $ SNErrDbSessionErr mkSyncNodeCallStack dbErr
    Right appResult -> pure appResult

-- | Lift a database operation that returns Either DbSessionError to ExceptT SyncNodeError.
-- Converts database session errors to sync node errors with call stack context.
liftDbSession :: SyncNodeCallStack -> DB.DbM (Either DB.DbSessionError a) -> ExceptT SyncNodeError DB.DbM a
liftDbSession cs dbAction = do
  result <- lift dbAction
  case result of
    Left dbErr -> throwError $ SNErrDbSessionErr cs dbErr
    Right val -> pure val

-- | Helper function to lift DbLookupError to SyncNodeError (similar to liftDbSession)
liftDbLookup :: SyncNodeCallStack -> DB.DbM (Either DB.DbLookupError a) -> ExceptT SyncNodeError DB.DbM a
liftDbLookup cs dbAction = do
  result <- lift dbAction
  case result of
    Left dbErr -> throwError $ SNErrDbLookupError cs dbErr
    Right val -> pure val

-- | Lift a nested ExceptT operation that returns Either DbSessionError.
-- Handles both SyncNodeError and DbSessionError, converting the latter to SyncNodeError.
liftDbSessionEither :: SyncNodeCallStack -> ExceptT SyncNodeError DB.DbM (Either DB.DbSessionError a) -> ExceptT SyncNodeError DB.DbM a
liftDbSessionEither cs mResult = do
  resultE <- lift $ runExceptT mResult
  case resultE of
    Left err -> throwError $ SNErrDefault cs (show err)
    Right result -> case result of
      Left dbErr -> throwError $ SNErrDbSessionErr cs dbErr
      Right val -> pure val

-- | Lift a nested ExceptT operation that returns Either DbLookupError.
-- Handles both SyncNodeError and DbLookupError, converting the latter to SyncNodeError.
liftDbLookupEither :: SyncNodeCallStack -> ExceptT SyncNodeError DB.DbM (Either DB.DbLookupError a) -> ExceptT SyncNodeError DB.DbM a
liftDbLookupEither cs mResult = do
  resultE <- lift $ runExceptT mResult
  case resultE of
    Left err -> throwError $ SNErrDefault cs (show err)
    Right result -> case result of
      Left dbErr -> throwError $ SNErrDbLookupError cs dbErr
      Right val -> pure val

-- | Lift a Maybe-returning database operation to Either DbLookupError
--
-- Converts DbM (Maybe a) to ExceptT SyncNodeError DB.DbM (Either DB.DbLookupError a).
-- Common pattern for database lookups that may not find results.
liftDbLookupMaybe :: DB.DbCallStack -> Text -> DB.DbM (Maybe a) -> ExceptT SyncNodeError DB.DbM (Either DB.DbLookupError a)
liftDbLookupMaybe cs errMsg dbAction = do
  result <- lift dbAction
  pure $ case result of
    Nothing -> Left $ DB.DbLookupError cs errMsg
    Just value -> Right value

liftSessionIO :: SyncNodeCallStack -> ExceptT DB.DbSessionError IO a -> ExceptT SyncNodeError IO a
liftSessionIO cs dbAction = do
  result <- liftIO $ runExceptT dbAction
  case result of
    Left dbErr -> throwError $ SNErrDbSessionErr cs dbErr
    Right val -> pure val

acquireDbConnection :: [HsqlSet.Setting] -> IO HsqlC.Connection
acquireDbConnection settings = do
  result <- HsqlC.acquire settings
  case result of
    Left connErr -> throwIO $ SNErrDbSessionErr mkSyncNodeCallStack $ DB.mkDbSessionError (show connErr)
    Right conn -> pure conn

mkDbApply :: CardanoBlock -> DbEvent
mkDbApply = DbApplyBlock

-- | This simulates a synchronous operations, since the thread waits for the db
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

-- | Block if the queue is empty and if it's not read/flush everything.
-- Need this because `flushTBQueue` never blocks and we want to block until
-- there is one item or more.
-- Use this instead of STM.check to make sure it blocks if the queue is empty.
blockingFlushDbEventQueue :: ThreadChannels -> IO [DbEvent]
blockingFlushDbEventQueue tc = do
  STM.atomically $ do
    x <- TBQ.readTBQueue $ tcQueue tc
    xs <- TBQ.flushTBQueue $ tcQueue tc
    pure $ x : xs
