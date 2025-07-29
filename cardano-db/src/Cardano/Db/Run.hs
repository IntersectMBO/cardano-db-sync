{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Run where

import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Trace (Trace)
import Cardano.Prelude
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger (
  LogLevel (..),
  LogSource,
  LoggingT,
  NoLoggingT,
  runLoggingT,
  runNoLoggingT,
 )
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Control.Tracer (traceWith)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as HsqlCon
import qualified Hasql.Connection.Setting as HsqlConS
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlStmt
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, fromLogStr)
import Prelude (error, userError)

import Cardano.Db.Error (DbCallStack (..), DbError (..), runOrThrowIO)
import Cardano.Db.PGConfig
import Cardano.Db.Statement.Function.Core (mkDbCallStack, runDbSessionMain)
import Cardano.Db.Types (DbAction (..), DbEnv (..))
import qualified Hasql.Session as HsqlSess

-----------------------------------------------------------------------------------------
-- Types and Constants
-----------------------------------------------------------------------------------------

-- | Database transaction isolation levels supported by PostgreSQL
data IsolationLevel
  = ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Show, Eq)

-----------------------------------------------------------------------------------------
-- Low-Level Transaction Management
-----------------------------------------------------------------------------------------

-- | Convert isolation level to SQL string representation
isolationLevelToSql :: IsolationLevel -> Text
isolationLevelToSql ReadUncommitted = "READ UNCOMMITTED"
isolationLevelToSql ReadCommitted = "READ COMMITTED"
isolationLevelToSql RepeatableRead = "REPEATABLE READ"
isolationLevelToSql Serializable = "SERIALIZABLE"

-- | Create a BEGIN statement with specified isolation level
beginTransactionStmt :: IsolationLevel -> HsqlStmt.Statement () ()
beginTransactionStmt isolationLevel =
  HsqlStmt.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    sql = "BEGIN ISOLATION LEVEL " <> encodeUtf8 (isolationLevelToSql isolationLevel)

-- | Create a COMMIT statement
commitTransactionStmt :: HsqlStmt.Statement () ()
commitTransactionStmt =
  HsqlStmt.Statement "COMMIT" HsqlE.noParams HsqlD.noResult True

-- | Create a ROLLBACK statement
rollbackTransactionStmt :: HsqlStmt.Statement () ()
rollbackTransactionStmt =
  HsqlStmt.Statement "ROLLBACK" HsqlE.noParams HsqlD.noResult True

-- | Commit the current transaction within a DbAction context
commitCurrentTransaction :: MonadIO m => DbAction m ()
commitCurrentTransaction = do
  runDbSessionMain (mkDbCallStack "commitCurrentTransaction") $
    HsqlSess.statement () commitTransactionStmt

-- | Convert Hasql SessionError to DbError for consistent error handling
sessionErrorToDbError :: DbCallStack -> HsqlS.SessionError -> DbError
sessionErrorToDbError cs sessionErr =
  DbError cs ("Transaction error: " <> Text.pack (show sessionErr)) (Just sessionErr)

-----------------------------------------------------------------------------------------
-- Connection Management
-----------------------------------------------------------------------------------------

-- | Acquire a single database connection with error handling
acquireConnection :: [HsqlConS.Setting] -> IO HsqlCon.Connection
acquireConnection settings = do
  result <- HsqlCon.acquire settings
  case result of
    Left err -> throwIO $ userError $ "Connection error: " <> show err
    Right conn -> pure conn

-- | Create a connection pool with specified settings and size
--
-- The pool uses a 30-second TTL and automatic connection cleanup.
-- Connections are acquired lazily and released automatically.
createHasqlConnectionPool :: [HsqlConS.Setting] -> Int -> IO (Pool HsqlCon.Connection)
createHasqlConnectionPool settings numConnections = do
  newPool poolConfig
  where
    poolConfig =
      defaultPoolConfig
        acquireConn
        releaseConn
        30.0 -- cacheTTL (seconds) - connections are kept alive for 30s when idle
        numConnections -- maxResources - maximum number of connections in the pool
    acquireConn = do
      result <- HsqlCon.acquire settings
      case result of
        Left err -> throwIO $ userError $ "Connection error: " <> show err
        Right conn -> pure conn
    releaseConn = HsqlCon.release

-- | Create a DbEnv containing both a primary connection and connection pool
--
-- The primary connection is used for sequential/transactional operations,
-- while the pool is used for parallel/async operations.
createDbEnv :: HsqlCon.Connection -> Pool HsqlCon.Connection -> Maybe (Trace IO Text) -> DbEnv
createDbEnv conn pool mTracer =
  DbEnv
    { dbConnection = conn -- Primary connection for main thread operations
    , dbPoolConnection = pool -- Pool for parallel/async operations
    , dbTracer = mTracer -- Optional tracer for logging
    }

-- | Run an action with a managed connection pool that will be properly cleaned up
--
-- This function ensures that the connection pool is destroyed when the action
-- completes, preventing resource leaks. Uses 'finally' to guarantee cleanup
-- even if the action throws an exception.
withManagedPool :: [HsqlConS.Setting] -> Int -> (Pool HsqlCon.Connection -> IO a) -> IO a
withManagedPool settings numConns action = do
  pool <- createHasqlConnectionPool settings numConns
  action pool `finally` destroyAllResources pool

-----------------------------------------------------------------------------------------
-- Core Database Execution with Transaction Control
-----------------------------------------------------------------------------------------

-- | Run a DbAction with explicit transaction control and isolation level
--
-- This is the foundational function for all database operations with full control
-- over transaction behavior and error handling.
--
-- == Transaction Behavior:
-- * Begins transaction with specified isolation level
-- * Runs the action within the transaction
-- * Commits if action succeeds, rollback only on commit failure or async exceptions
-- * Returns Either for explicit error handling instead of throwing exceptions
--
-- == Exception Safety:
-- * Uses 'mask' to prevent async exceptions during transaction lifecycle
-- * Uses 'onException' to ensure rollback on interrupts (Ctrl+C, SIGTERM, etc.)
-- * Does NOT rollback on action errors - lets them commit (matches Persistent semantics)
--
-- == Note:
-- This follows Persistent's philosophy where successful function calls commit
-- their transactions regardless of the return value. Only async exceptions and
-- commit failures trigger rollbacks.
runDbActionWithIsolation ::
  MonadUnliftIO m =>
  DbEnv ->
  IsolationLevel ->
  DbAction m a ->
  m (Either DbError a)
runDbActionWithIsolation dbEnv isolationLevel action  = do
  withRunInIO $ \runInIO -> do
    -- Use masking to prevent async exceptions during transaction management
    mask $ \restore -> do
      -- Begin transaction with specified isolation level
      beginResult <- beginTransaction dbEnv isolationLevel
      case beginResult of
        Left err -> pure (Left err)
        Right _ -> do
          -- Run action with async exception protection via onException
          -- If interrupted (Ctrl+C), the onException handler will rollback
          actionResult <-
            try $
              onException
                (restore (runInIO $ runReaderT (runDbAction action) dbEnv))
                (restore $ rollbackTransaction dbEnv)
          case actionResult of
            -- Action threw exception - return the DbError
            Left (err :: DbError) -> pure (Left err)
            Right val -> do
              -- Attempt to commit the transaction
              commitResult <- commitTransaction dbEnv
              case commitResult of
                Left commitErr -> do
                  -- Commit failed - rollback and return the commit error
                  rollbackTransaction dbEnv
                  pure (Left commitErr)
                Right _ -> pure (Right val)
  where
    beginTransaction :: DbEnv -> IsolationLevel -> IO (Either DbError ())
    beginTransaction env level = do
      let cs = mkDbCallStack "beginTransaction"
      result <- HsqlS.run (HsqlS.statement () (beginTransactionStmt level)) (dbConnection env)
      pure $ first (sessionErrorToDbError cs) result

    commitTransaction :: DbEnv -> IO (Either DbError ())
    commitTransaction env = do
      let cs = mkDbCallStack "commitTransaction"
      result <- HsqlS.run (HsqlS.statement () commitTransactionStmt) (dbConnection env)
      pure $ first (sessionErrorToDbError cs) result

    rollbackTransaction :: DbEnv -> IO ()
    rollbackTransaction env = do
      void $ HsqlS.run (HsqlS.statement () rollbackTransactionStmt) (dbConnection env)

-- | Run a DbAction with transaction control, throwing exceptions on error
--
-- This is a convenience wrapper around 'runDbActionWithIsolation' that
-- throws exceptions instead of returning Either values.
runDbConnWithIsolation ::
  MonadUnliftIO m =>
  DbAction m a ->
  DbEnv ->
  IsolationLevel ->
  m a
runDbConnWithIsolation action dbEnv isolationLevel = do
  result <- runDbActionWithIsolation dbEnv isolationLevel action
  case result of
    Left err -> liftIO $ throwIO err
    Right val -> pure val

-- | Simple DbAction runner for testing and simple operations
--
-- Runs the action in IO context with basic error propagation.
-- Does not provide transaction control - use runDbActionWithIsolation for that.
runDbActionIO :: DbEnv -> DbAction IO a -> IO a
runDbActionIO dbEnv action = do
  result <- try $ runReaderT (runDbAction action) dbEnv
  case result of
    Left (err :: DbError) -> throwIO err
    Right val -> pure val

-----------------------------------------------------------------------------------------
-- High-Level Database Runners with Specific Patterns
-----------------------------------------------------------------------------------------

-- | Run DbAction with IOHK-style logging and RepeatableRead isolation
--
-- This is the standard runner for most database operations in the sync system.
-- Uses RepeatableRead isolation level to match historical behavior.
runDbIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbAction (LoggingT m) a -> m a
runDbIohkLogging tracer dbEnv action =
  runIohkLogging tracer $
    runDbConnWithIsolation action dbEnv RepeatableRead

-- | Like runDbIohkLogging but returns Either instead of throwing exceptions
--
-- Useful when you need to handle database errors explicitly rather than
-- letting them propagate as exceptions.
runDbIohkLoggingEither :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbAction (LoggingT m) a -> m (Either DbError a)
runDbIohkLoggingEither tracer dbEnv action = do
  runIohkLogging tracer $
    runDbActionWithIsolation dbEnv RepeatableRead action

-- | Run DbAction without logging but with RepeatableRead isolation
--
-- Useful for operations where logging overhead is not desired.
runDbIohkNoLogging :: MonadUnliftIO m => DbEnv -> DbAction (NoLoggingT m) a -> m a
runDbIohkNoLogging dbEnv action =
  runNoLoggingT $
    runDbConnWithIsolation action dbEnv RepeatableRead

-- | Standalone database runner that creates its own connection from PGPass
--
-- This function handles the complete lifecycle: reads configuration,
-- creates connections and pools, runs the action, and cleans up.
-- Suitable for standalone operations or testing.
runDbNoLogging :: MonadUnliftIO m => PGPassSource -> DbAction m a -> m a
runDbNoLogging source action = do
  pgconfig <- liftIO $ runOrThrowIO (readPGPass source)
  connSetting <- liftIO $ case toConnectionSetting pgconfig of
    Left err -> error err
    Right setting -> pure setting
  withRunInIO $ \runInIO ->
    withManagedPool [connSetting] 4 $ \pool ->
      bracket
        (acquireConnection [connSetting])
        HsqlCon.release
        ( \connection -> runInIO $ do
            let dbEnv = createDbEnv connection pool Nothing
            runDbConnWithIsolation action dbEnv RepeatableRead
        )

-- | Convenience wrapper for runDbNoLogging using default environment PGPass
runDbNoLoggingEnv :: MonadUnliftIO m => DbAction m a -> m a
runDbNoLoggingEnv = runDbNoLogging PGPassDefaultEnv

-- | Standalone runner with NoLoggingT monad for pure IO operations
--
-- Similar to runDbNoLogging but specifically for NoLoggingT IO actions.
runWithConnectionNoLogging :: PGPassSource -> DbAction (NoLoggingT IO) a -> IO a
runWithConnectionNoLogging source action = do
  pgConfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgConfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  withManagedPool [connSetting] 4 $ \pool ->
    bracket
      (acquireConnection [connSetting])
      HsqlCon.release
      ( \connection -> do
          let dbEnv = createDbEnv connection pool Nothing
          runNoLoggingT $ runDbConnWithIsolation action dbEnv RepeatableRead
      )

-----------------------------------------------------------------------------------------
-- Pool-Based Operations for Parallel/Async Work
-----------------------------------------------------------------------------------------

-- | Run DbAction using a connection from an existing pool with logging
--
-- This function takes a connection from the provided pool and runs the action
-- with full logging support. The connection is kept locked for the entire
-- duration of the action to prevent race conditions and resource leaks.
runPoolDbIohkLogging ::
  MonadUnliftIO m =>
  Pool HsqlCon.Connection ->
  Trace IO Text ->
  DbAction (LoggingT m) a ->
  m (Either DbError a)
runPoolDbIohkLogging connPool tracer action = do
  withRunInIO $ \runInIO ->
    withResource connPool $ \conn -> do
      let dbEnv = createDbEnv conn connPool (Just tracer)
      runInIO $
        runIohkLogging tracer $
          runDbActionWithIsolation dbEnv RepeatableRead action

-- | Run DbAction using a connection from the DbEnv's pool
--
-- This function extracts a connection from the DbEnv's connection pool
-- and runs the action with it. The connection is kept locked for the entire
-- duration of the action to prevent race conditions and resource leaks.
--
-- == Use Cases:
-- * Parallel database operations alongside the main thread
-- * Async database work that shouldn't block the main connection
-- * Bulk operations that can benefit from connection pooling
--
-- == Important Notes:
-- * The action runs in the same DbEnv context but with a pool connection
-- * Logging is preserved from the original DbEnv
-- * Connection is automatically managed by the pool and kept locked during execution
runPoolDbAction :: forall a m. MonadUnliftIO m => DbEnv -> DbAction m a -> m a
runPoolDbAction dbEnv action = do
  withRunInIO $ \runInIO ->
    withResource (dbPoolConnection dbEnv) $ \conn -> do
      let poolDbEnv = dbEnv {dbConnection = conn}
      runInIO $ runReaderT (runDbAction action) poolDbEnv

-----------------------------------------------------------------------------------------
-- Logging Utilities
-----------------------------------------------------------------------------------------

-- | Convert monad-logger LoggingT to IOHK-style tracing
--
-- This function bridges the gap between monad-logger's LoggingT and
-- the IOHK tracing system used throughout the cardano ecosystem.
runIohkLogging :: Trace IO Text -> LoggingT m a -> m a
runIohkLogging tracer action =
  runLoggingT action toIohkLog
  where
    toIohkLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    toIohkLog _loc _src level msg = do
      meta <- mkLOMeta (toIohkSeverity level) Public
      traceWith
        tracer
        (name, LogObject name meta (LogMessage . Text.decodeLatin1 $ fromLogStr msg))

    name :: Text
    name = "db-sync"

    -- \| Convert monad-logger LogLevel to IOHK Severity
    toIohkSeverity :: LogLevel -> Severity
    toIohkSeverity =
      \case
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Error
