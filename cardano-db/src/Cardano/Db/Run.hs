{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Run where

import Cardano.Logging.Trace (traceWith)
import Cardano.Logging.Types (SeverityS (..), Trace)
import Cardano.Prelude
import Control.Monad.Logger (
  LogLevel (..),
  LogSource,
  LoggingT,
  runLoggingT,
  runNoLoggingT,
 )
import Control.Monad.Trans.Resource (MonadUnliftIO)
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
import Prelude (userError)

import Cardano.Db.Error (DbSessionError (..), formatSessionError, mkDbCallStack, runOrThrowIO)
import Cardano.Db.Log (LogMessage (..), logWarning)
import Cardano.Db.PGConfig (PGPassSource (..), readPGPass, toConnectionSetting)
import Cardano.Db.Statement.Function.Core (runSession)
import Cardano.Db.Types (DbEnv (..), DbM (..))

-----------------------------------------------------------------------------------------
-- Transaction Runners for DbM
-----------------------------------------------------------------------------------------

-- | Run a transaction-control statement, failing loudly if it does not take effect.
runTxControlStmt :: HsqlCon.Connection -> HsqlStmt.Statement () () -> IO ()
runTxControlStmt conn stmt = do
  result <- HsqlS.run (HsqlS.statement () stmt) conn
  case result of
    Left sessionErr ->
      throwIO $
        DbSessionError mkDbCallStack ("Transaction control failed: " <> formatSessionError sessionErr)
    Right () -> pure ()

-- | Run an action between BEGIN and COMMIT, rolling back if it throws or if
-- @commitResult@ rejects what it returned.
--
-- An action that reports failure by returning 'Left' has to be rolled back here,
-- because the error is a value rather than an exception and nothing else will do it.
-- Committing one leaves behind whatever the action wrote before it failed.
runInTransaction ::
  HsqlCon.Connection ->
  IsolationLevel ->
  (a -> Bool) ->
  IO a ->
  IO a
runInTransaction conn isolationLevel commitResult action = do
  runTxControlStmt conn (beginTransactionStmt isolationLevel)
  result <- try @SomeException action
  case result of
    Left err -> do
      -- Best effort: a failure to roll back must not replace the error on its way out.
      void $ try @SomeException $ runTxControlStmt conn rollbackTransactionStmt
      throwIO err
    Right value -> do
      runTxControlStmt conn $
        if commitResult value then commitTransactionStmt else rollbackTransactionStmt
      pure value

-- | Main database runner for blockchain synchronization operations
--
-- This is the primary runner used for cardano-db-sync block processing.
-- Wraps all operations in a single database transaction with full ACID guarantees.
-- Automatically handles BEGIN/COMMIT/ROLLBACK and provides comprehensive logging.
-- Accepts an optional isolation level (defaults to RepeatableRead).
runDbTransLogged ::
  MonadUnliftIO m =>
  Trace IO LogMessage ->
  DbEnv ->
  Maybe IsolationLevel -> -- Optional isolation level
  DbM a ->
  m a
runDbTransLogged = runDbTransLoggedWith (const True)

-- | 'runDbTransLogged' for an action that reports failure by returning 'Left', which
-- is rolled back rather than committed.
runDbTransLoggedEither ::
  MonadUnliftIO m =>
  Trace IO LogMessage ->
  DbEnv ->
  Maybe IsolationLevel ->
  DbM (Either e a) ->
  m (Either e a)
runDbTransLoggedEither = runDbTransLoggedWith isRight

runDbTransLoggedWith ::
  MonadUnliftIO m =>
  (a -> Bool) ->
  Trace IO LogMessage ->
  DbEnv ->
  Maybe IsolationLevel ->
  DbM a ->
  m a
runDbTransLoggedWith commitResult tracer dbEnv mIsolationLevel action = do
  eResult <-
    liftIO $
      try @SomeException $
        runInTransaction (dbConnection dbEnv) isolationLevel commitResult $
          runDbLogging tracer $
            liftIO $
              runReaderT (runDbM action) dbEnv
  case eResult of
    Left err -> do
      liftIO $ logWarning tracer $ "Database transaction error: " <> Text.pack (show err)
      throwIO err
    Right dbResult -> pure dbResult
  where
    isolationLevel = fromMaybe RepeatableRead mIsolationLevel

-- | Transaction runner without logging overhead
--
-- Same transaction guarantees as runDbTransLogged but without logging.
-- Useful for performance-critical operations or testing where log output isn't needed.
-- Accepts an optional isolation level (defaults to RepeatableRead).
runDbTransSilent ::
  MonadUnliftIO m =>
  DbEnv ->
  IsolationLevel ->
  DbM a ->
  m a
runDbTransSilent = runDbTransSilentWith (const True)

-- | 'runDbTransSilent' for an action that reports failure by returning 'Left', which
-- is rolled back rather than committed.
runDbTransSilentEither ::
  MonadUnliftIO m =>
  DbEnv ->
  IsolationLevel ->
  DbM (Either e a) ->
  m (Either e a)
runDbTransSilentEither = runDbTransSilentWith isRight

runDbTransSilentWith ::
  MonadUnliftIO m =>
  (a -> Bool) ->
  DbEnv ->
  IsolationLevel ->
  DbM a ->
  m a
runDbTransSilentWith commitResult dbEnv isolationLevel action =
  runNoLoggingT $
    liftIO $
      runInTransaction (dbConnection dbEnv) isolationLevel commitResult $
        runReaderT (runDbM action) dbEnv

-- | Database runner without transaction management
--
-- Executes DbM operations without wrapping them in BEGIN/COMMIT.
-- Uses auto-commit mode where each individual statement commits immediately.
-- Useful for operations that manage their own transactions or don't need ACID guarantees.
runDbDirectLogged ::
  MonadUnliftIO m =>
  Trace IO LogMessage ->
  DbEnv ->
  DbM a ->
  m a
runDbDirectLogged tracer dbEnv action = do
  result <- liftIO $ HsqlS.run simpleSession (dbConnection dbEnv)
  case result of
    Left sessionErr -> do
      liftIO $ logWarning tracer $ "Database session error: " <> Text.pack (show sessionErr)
      throwIO $ DbSessionError mkDbCallStack ("Database session error: " <> formatSessionError sessionErr)
    Right dbResult -> pure dbResult
  where
    simpleSession = do
      -- No transaction management - just run the action
      result <- liftIO $ try @SomeException $ runDbLogging tracer $ liftIO $ runReaderT (runDbM action) dbEnv
      case result of
        Left err -> liftIO $ throwIO err
        Right value -> pure value

-- | Database runner without transaction management or logging
runDbDirectSilent ::
  MonadUnliftIO m =>
  DbEnv ->
  DbM a ->
  m a
runDbDirectSilent dbEnv action = do
  runNoLoggingT $ do
    result <- liftIO $ HsqlS.run simpleSession (dbConnection dbEnv)
    case result of
      Left sessionErr ->
        throwIO $ DbSessionError mkDbCallStack ("Database session error: " <> formatSessionError sessionErr)
      Right dbResult -> pure dbResult
  where
    simpleSession = do
      result <- liftIO $ try @SomeException $ runReaderT (runDbM action) dbEnv
      case result of
        Left err -> throwIO err
        Right value -> pure value

-- | Connection pool-based transaction runner
--
-- Uses a connection from the pool rather than the main DbEnv connection.
-- Wraps operations in a transaction with logging. Designed for concurrent operations
-- where multiple threads need independent database connections.
-- Accepts an optional isolation level (defaults to RepeatableRead).
runDbPoolTransLogged ::
  MonadUnliftIO m =>
  Trace IO LogMessage ->
  DbEnv ->
  Maybe IsolationLevel -> -- Optional isolation level
  DbM a ->
  m a
runDbPoolTransLogged = runDbPoolTransLoggedWith (const True)

-- | 'runDbPoolTransLogged' for an action that reports failure by returning 'Left',
-- which is rolled back rather than committed.
runDbPoolTransLoggedEither ::
  MonadUnliftIO m =>
  Trace IO LogMessage ->
  DbEnv ->
  Maybe IsolationLevel ->
  DbM (Either e a) ->
  m (Either e a)
runDbPoolTransLoggedEither = runDbPoolTransLoggedWith isRight

runDbPoolTransLoggedWith ::
  MonadUnliftIO m =>
  (a -> Bool) ->
  Trace IO LogMessage ->
  DbEnv ->
  Maybe IsolationLevel ->
  DbM a ->
  m a
runDbPoolTransLoggedWith commitResult tracer dbEnv mIsolationLevel action =
  case dbPoolConnection dbEnv of
    Nothing -> throwIO $ DbSessionError mkDbCallStack "No connection pool available in DbEnv"
    Just pool ->
      runDbLogging tracer $
        liftIO $
          withResource pool $ \conn ->
            runInTransaction conn isolationLevel commitResult $ do
              let tempDbEnv = createDbEnv conn (dbPoolConnection dbEnv) (dbTracer dbEnv)
              runReaderT (runDbM action) tempDbEnv
  where
    isolationLevel = fromMaybe RepeatableRead mIsolationLevel

runDbPoolLogged ::
  MonadUnliftIO m =>
  Trace IO LogMessage ->
  DbEnv ->
  DbM a ->
  m a
runDbPoolLogged tracer dbEnv = runDbPoolTransLoggedWith (const True) tracer dbEnv (Just RepeatableRead)

-- | External service database runner with error handling
--
-- Designed for external services (like SMASH server) that manage their own connection pools.
-- Returns Either for explicit error handling rather than throwing exceptions.
-- Creates temporary DbEnv from the provided pool connection.
runDbWithPool ::
  MonadIO m =>
  Pool HsqlCon.Connection ->
  Trace IO LogMessage ->
  DbM a ->
  m (Either DbSessionError a)
runDbWithPool connPool tracer action = do
  liftIO $ try $ runDbLogging tracer $ do
    liftIO $ withResource connPool $ \conn -> do
      let tempDbEnv = createDbEnv conn (Just connPool) (Just tracer)
      runReaderT (runDbM action) tempDbEnv

-----------------------------------------------------------------------------------------
-- High-Level Database Runners with Specific Patterns
-----------------------------------------------------------------------------------------

-- | Simple standalone runner using default environment configuration
--
-- Self-contained runner that reads database configuration from environment variables.
-- Creates its own temporary connection and cleans up automatically.
-- Perfect for simple scripts and testing scenarios
runDbStandaloneSilent :: DbM a -> IO a
runDbStandaloneSilent = runDbStandaloneTransSilent PGPassDefaultEnv

-- | Standalone runner with connection pool support
--
-- Creates both a main connection and connection pool from the provided configuration.
-- Self-contained with full cleanup, suitable for applications needing both connection types
runDbStandaloneTransSilent :: PGPassSource -> DbM a -> IO a
runDbStandaloneTransSilent source action =
  withDbEnvStandalone source $ \dbEnv -> runDbTransSilent dbEnv RepeatableRead action

-- | Acquire a standalone connection from @source@ and hand its 'DbEnv' to the action,
-- releasing the connection afterwards. Lets a caller pick the transaction runner.
withDbEnvStandalone :: PGPassSource -> (DbEnv -> IO a) -> IO a
withDbEnvStandalone source action = do
  pgconfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    (\connection -> action (createDbEnv connection Nothing Nothing))

-- | Standalone runner without transaction management
--
-- Self-contained runner that creates its own connection but doesn't wrap operations
-- in transactions. Uses auto-commit mode. Perfect for simple operations that don't
-- need ACID guarantees or tools that manage their own transaction boundaries.
runDbStandaloneDirectSilent :: PGPassSource -> DbM a -> IO a
runDbStandaloneDirectSilent source action = do
  pgconfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    ( \connection -> do
        let dbEnv = createDbEnv connection Nothing Nothing
        runDbDirectSilent dbEnv action
    )

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
    sql = encodeUtf8 $ "BEGIN ISOLATION LEVEL " <> isolationLevelToSql isolationLevel

-- beginTransaction :: IsolationLevel -> DbM ()
-- beginTransaction isolationLevel = do
--   -- Begin new transaction with specified isolation level
--   runSession $ HsqlS.statement () (beginTransactionStmt isolationLevel)

-- | Create a COMMIT statement
commitTransactionStmt :: HsqlStmt.Statement () ()
commitTransactionStmt =
  HsqlStmt.Statement "COMMIT" HsqlE.noParams HsqlD.noResult True

commitTransaction :: HasCallStack => DbM ()
commitTransaction = do
  runSession mkDbCallStack $ HsqlS.statement () commitTransactionStmt

-- | Create a ROLLBACK statement
rollbackTransactionStmt :: HsqlStmt.Statement () ()
rollbackTransactionStmt =
  HsqlStmt.Statement "ROLLBACK" HsqlE.noParams HsqlD.noResult True

transactionSaveWithIsolation :: HasCallStack => IsolationLevel -> DbM ()
transactionSaveWithIsolation isolationLevel = do
  -- Commit current transaction
  runSession mkDbCallStack $ HsqlS.statement () commitTransactionStmt
  -- Begin new transaction with specified isolation level
  runSession mkDbCallStack $ HsqlS.statement () (beginTransactionStmt isolationLevel)

setDefaultIsolationLevel :: HsqlCon.Connection -> IO ()
setDefaultIsolationLevel conn = do
  result <- HsqlS.run (HsqlS.statement () setIsolationStmt) conn
  case result of
    Left err -> throwIO $ DbSessionError mkDbCallStack ("Failed to set isolation level: " <> formatSessionError err)
    Right _ -> pure ()
  where
    setIsolationStmt =
      HsqlStmt.Statement
        "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL REPEATABLE READ"
        HsqlE.noParams
        HsqlD.noResult
        True

checkTransactionStmt :: HsqlStmt.Statement () Bool
checkTransactionStmt =
  HsqlStmt.Statement
    "SELECT pg_current_xact_id_if_assigned() IS NOT NULL"
    HsqlE.noParams
    (HsqlD.singleRow (HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
    True

-----------------------------------------------------------------------------------------
-- Connection Management
-----------------------------------------------------------------------------------------

-- | Acquire a single database connection with error handling
acquireConnection :: [HsqlConS.Setting] -> IO HsqlCon.Connection
acquireConnection settings = do
  result <- HsqlCon.acquire settings
  case result of
    Left err -> throwIO $ userError $ "Connection error: " <> show err
    Right conn -> do
      -- Set default isolation level for the connection to Repeatable Read
      setDefaultIsolationLevel conn
      pure conn

-- | Acquire a database connection without transaction management
acquireDbConnectionNoTrans :: [HsqlConS.Setting] -> IO HsqlCon.Connection
acquireDbConnectionNoTrans settings = do
  result <- HsqlCon.acquire settings
  case result of
    Left connErr -> throwIO $ userError $ "acquireDbConnectionNoTrans: " <> show connErr
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
createDbEnv :: HsqlCon.Connection -> Maybe (Pool HsqlCon.Connection) -> Maybe (Trace IO LogMessage) -> DbEnv
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
-- Logging Utilities
-----------------------------------------------------------------------------------------

-- | Convert monad-logger LoggingT to trace-dispatcher tracing
--
-- This function bridges the gap between monad-logger's LoggingT and
-- the trace-dispatcher system used throughout the cardano ecosystem.
runDbLogging :: Trace IO LogMessage -> LoggingT m a -> m a
runDbLogging tracer action =
  runLoggingT action toDbLog
  where
    toDbLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    toDbLog _loc _src level msg =
      traceWith tracer $
        LogMessage (toSeverity level) (Text.decodeLatin1 $ fromLogStr msg)

    -- \| Convert monad-logger LogLevel to trace-dispatcher severity
    toSeverity :: LogLevel -> SeverityS
    toSeverity =
      \case
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Error
