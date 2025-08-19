{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Run where

import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Trace (Trace, logWarning)
import Cardano.Prelude
import Control.Monad.Logger (
  LogLevel (..),
  LogSource,
  LoggingT,
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
import Prelude (userError)

import Cardano.Db.Error (DbSessionError (..), formatSessionError, mkDbCallStack, runOrThrowIO)
import Cardano.Db.PGConfig (PGPassSource (..), readPGPass, toConnectionSetting)
import Cardano.Db.Statement.Function.Core (runSession)
import Cardano.Db.Types (DbEnv (..), DbM (..))

-----------------------------------------------------------------------------------------
-- Transaction Runners for DbM
-----------------------------------------------------------------------------------------

-- | Main database runner for blockchain synchronization operations
--
-- This is the primary runner used for cardano-db-sync block processing.
-- Wraps all operations in a single database transaction with full ACID guarantees.
-- Automatically handles BEGIN/COMMIT/ROLLBACK and provides comprehensive logging.
runDbTransLogged ::
  MonadUnliftIO m =>
  Trace IO Text ->
  DbEnv ->
  DbM a ->
  m a
runDbTransLogged tracer dbEnv action = do
  result <- liftIO $ HsqlS.run transactionSession (dbConnection dbEnv)
  case result of
    Left sessionErr -> do
      liftIO $ logWarning tracer $ "Database transaction error: " <> Text.pack (show sessionErr)
      throwIO $ DbSessionError mkDbCallStack ("Database transaction error: " <> formatSessionError sessionErr)
    Right dbResult -> pure dbResult
  where
    transactionSession = do
      HsqlS.statement () (beginTransactionStmt RepeatableRead)

      result <- liftIO $ try @SomeException $ runIohkLogging tracer $ liftIO $ runReaderT (runDbM action) dbEnv
      case result of
        Left err -> do
          HsqlS.statement () rollbackTransactionStmt
          liftIO $ throwIO err
        Right value -> do
          HsqlS.statement () commitTransactionStmt
          pure value

-- | Transaction runner without logging overhead
--
-- Same transaction guarantees as runDbTransLogged but without logging.
-- Useful for performance-critical operations or testing where log output isn't needed.
runDbTransSilent ::
  MonadUnliftIO m =>
  DbEnv ->
  DbM a ->
  m a
runDbTransSilent dbEnv action = do
  runNoLoggingT $ do
    result <- liftIO $ HsqlS.run transactionSession (dbConnection dbEnv)
    case result of
      Left sessionErr ->
        throwIO $ DbSessionError mkDbCallStack ("Database transaction error: " <> formatSessionError sessionErr)
      Right dbResult -> pure dbResult
  where
    transactionSession = do
      HsqlS.statement () (beginTransactionStmt RepeatableRead)

      result <- liftIO $ try @SomeException $ runReaderT (runDbM action) dbEnv
      case result of
        Left err -> do
          HsqlS.statement () rollbackTransactionStmt
          throwIO err
        Right value -> do
          HsqlS.statement () commitTransactionStmt
          pure value

-- | Database runner without transaction management
--
-- Executes DbM operations without wrapping them in BEGIN/COMMIT.
-- Uses auto-commit mode where each individual statement commits immediately.
-- Useful for operations that manage their own transactions or don't need ACID guarantees.
runDbDirectLogged ::
  MonadUnliftIO m =>
  Trace IO Text ->
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
      result <- liftIO $ try @SomeException $ runIohkLogging tracer $ liftIO $ runReaderT (runDbM action) dbEnv
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
runDbPoolTransLogged ::
  MonadUnliftIO m =>
  Trace IO Text ->
  DbEnv ->
  DbM a ->
  m a
runDbPoolTransLogged tracer dbEnv action = do
  case dbPoolConnection dbEnv of
    Nothing -> throwIO $ DbSessionError mkDbCallStack "No connection pool available in DbEnv"
    Just pool -> do
      runIohkLogging tracer $ do
        liftIO $ withResource pool $ \conn -> do
          result <- HsqlS.run (transactionSession conn) conn
          case result of
            Left sessionErr -> throwIO $ DbSessionError mkDbCallStack ("Pool transaction error: " <> formatSessionError sessionErr)
            Right dbResult -> pure dbResult
  where
    transactionSession conn = do
      HsqlS.statement () (beginTransactionStmt RepeatableRead)
      result <- liftIO $ try @SomeException $ do
        let tempDbEnv = createDbEnv conn (dbPoolConnection dbEnv) (dbTracer dbEnv)
        runReaderT (runDbM action) tempDbEnv
      case result of
        Left err -> do
          HsqlS.statement () rollbackTransactionStmt
          liftIO $ throwIO err
        Right value -> do
          HsqlS.statement () commitTransactionStmt
          pure value

-- | External service database runner with error handling
--
-- Designed for external services (like SMASH server) that manage their own connection pools.
-- Returns Either for explicit error handling rather than throwing exceptions.
-- Creates temporary DbEnv from the provided pool connection.
runDbWithPool ::
  MonadIO m =>
  Pool HsqlCon.Connection ->
  Trace IO Text ->
  DbM a ->
  m (Either DbSessionError a)
runDbWithPool connPool tracer action = do
  liftIO $ try $ runIohkLogging tracer $ do
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
runDbStandaloneTransSilent source action = do
  pgconfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    ( \connection -> do
        let dbEnv = createDbEnv connection Nothing Nothing
        runDbTransSilent dbEnv action
    )

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
createDbEnv :: HsqlCon.Connection -> Maybe (Pool HsqlCon.Connection) -> Maybe (Trace IO Text) -> DbEnv
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
