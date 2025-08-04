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
import Cardano.BM.Trace (Trace)
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
import qualified Hasql.Session as HsqlS
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, fromLogStr)
import Prelude (userError)

import Cardano.Db.Error (DbError (..), runOrThrowIO)
import Cardano.Db.PGConfig (PGPassSource (..), readPGPass, toConnectionSetting)
import Cardano.Db.Statement.Function.Core (runSession)
import Cardano.Db.Types (DbEnv (..), DbM (..))
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlStmt

-----------------------------------------------------------------------------------------
-- Transaction Runners for DbM
-----------------------------------------------------------------------------------------

-- | Main database runner for blockchain synchronization operations
--
-- This is the primary runner used for cardano-db-sync block processing.
-- Wraps all operations in a single database transaction with full ACID guarantees.
-- Automatically handles BEGIN/COMMIT/ROLLBACK and provides comprehensive logging.
runDbTransactionIohkLogging ::
  MonadUnliftIO m =>
  Trace IO Text ->
  DbEnv ->
  DbM a ->
  m a
runDbTransactionIohkLogging tracer dbEnv action = do
  runIohkLogging tracer $ do
    result <- liftIO $ HsqlS.run transactionSession (dbConnection dbEnv)
    case result of
      Left sessionErr ->
        throwIO $ DbError $ "Database transaction error: " <> Text.pack (show sessionErr)
      Right dbResult -> pure dbResult
  where
    transactionSession = do
      -- Begin the transaction
      HsqlS.statement () (beginTransactionStmt RepeatableRead)
      -- Run the action in IO, but it will use the same connection
      -- since runSession uses the connection from dbEnv
      result <- liftIO $ try @SomeException $ runReaderT (runDbM action) dbEnv
      case result of
        Left err -> do
          -- Something went wrong - rollback the transaction
          HsqlS.statement () rollbackTransactionStmt
          throwIO err
        Right value -> do
          -- Everything succeeded - commit the transaction
          HsqlS.statement () commitTransactionStmt
          pure value

-- | Transaction runner without logging overhead
--
-- Same transaction guarantees as runDbTransactionIohkLogging but without logging.
-- Useful for performance-critical operations or testing where log output isn't needed.
runDbTransactionIohkNoLogging ::
  MonadUnliftIO m =>
  DbEnv ->
  DbM a ->
  m a
runDbTransactionIohkNoLogging dbEnv action = do
  runNoLoggingT $ do
    result <- liftIO $ HsqlS.run transactionSession (dbConnection dbEnv)
    case result of
      Left sessionErr ->
        throwIO $ DbError $ "Database transaction error: " <> Text.pack (show sessionErr)
      Right dbResult -> pure dbResult
  where
    transactionSession = do
      -- Step 1: Begin the transaction
      HsqlS.statement () (beginTransactionStmt RepeatableRead)
      -- Step 2: Run the action in IO, but it will use the same connection
      -- since runSession uses the connection from dbEnv
      result <- liftIO $ try @SomeException $ runReaderT (runDbM action) dbEnv
      case result of
        Left err -> do
          -- Step 2a: Something went wrong - rollback the transaction
          HsqlS.statement () rollbackTransactionStmt
          throwIO err
        Right value -> do
          -- Step 2b: Everything succeeded - commit the transaction
          HsqlS.statement () commitTransactionStmt
          pure value

-- | Pool-based runner for concurrent database operations
--
-- Uses a connection from the DbEnv's connection pool instead of the main connection.
-- Each database operation auto-commits (no explicit transaction boundaries).
-- Ideal for operations that run parallel to the main synchronization thread.
runDbPoolIohkLogging ::
  MonadUnliftIO m =>
  Trace IO Text ->
  DbEnv ->
  DbM a ->
  m a
runDbPoolIohkLogging tracer dbEnv action = do
  case dbPoolConnection dbEnv of
    Nothing -> throwIO $ DbError "No connection pool available in DbEnv"
    Just pool -> do
      runIohkLogging tracer $ do
        liftIO $ withResource pool $ \conn -> do
          -- Create a temporary DbEnv using the pool connection
          let tempDbEnv = dbEnv {dbConnection = conn}
          -- Run the action with the temporary DbEnv
          runReaderT (runDbM action) tempDbEnv

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
  m (Either DbError a)
runDbWithPool connPool tracer action = do
  liftIO $ try $ runIohkLogging tracer $ do
    liftIO $ withResource connPool $ \conn -> do
      let tempDbEnv =
            DbEnv
              { dbConnection = conn
              , dbPoolConnection = Just connPool
              , dbTracer = Just tracer
              }
      runReaderT (runDbM action) tempDbEnv

-----------------------------------------------------------------------------------------
-- High-Level Database Runners with Specific Patterns
-----------------------------------------------------------------------------------------

-- | Simple standalone runner using default environment configuration
--
-- Self-contained runner that reads database configuration from environment variables.
-- Creates its own temporary connection and cleans up automatically.
-- Perfect for simple scripts and testing scenarios
runDbMNoLoggingDefaultEnv :: DbM a -> IO a
runDbMNoLoggingDefaultEnv = runDbMTransactionNoLogging PGPassDefaultEnv

-- | Standalone runner with connection pool support
--
-- Creates both a main connection and connection pool from the provided configuration.
-- Self-contained with full cleanup, suitable for applications needing both connection types
runDbMWithPoolNoLogging :: PGPassSource -> DbM a -> IO a
runDbMWithPoolNoLogging source action = do
  pgconfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  withManagedPool [connSetting] 4 $ \pool ->
    bracket
      (acquireConnection [connSetting])
      HsqlCon.release
      ( \connection -> do
          let dbEnv =
                DbEnv
                  { dbConnection = connection
                  , dbPoolConnection = Just pool -- Pool available for async operations
                  , dbTracer = Nothing
                  }
          runReaderT (runDbM action) dbEnv
      )

-- | Standalone runner with connection pool support
--
-- Creates both a main connection and connection pool from the provided configuration.
-- Self-contained with full cleanup, suitable for applications needing both connection types
runDbMTransactionNoLogging :: PGPassSource -> DbM a -> IO a
runDbMTransactionNoLogging source action = do
  pgconfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    ( \connection -> do
        let dbEnv =
              DbEnv
                { dbConnection = connection
                , dbPoolConnection = Nothing
                , dbTracer = Nothing
                }
        runDbTransactionIohkNoLogging dbEnv action
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
    sql = "BEGIN ISOLATION LEVEL " <> encodeUtf8 (isolationLevelToSql isolationLevel)

beginTransaction :: IsolationLevel -> DbM ()
beginTransaction isolationLevel = do
  -- Begin new transaction with specified isolation level
  runSession $ HsqlS.statement () (beginTransactionStmt isolationLevel)

-- | Create a COMMIT statement
commitTransactionStmt :: HsqlStmt.Statement () ()
commitTransactionStmt =
  HsqlStmt.Statement "COMMIT" HsqlE.noParams HsqlD.noResult True

-- | Create a ROLLBACK statement
rollbackTransactionStmt :: HsqlStmt.Statement () ()
rollbackTransactionStmt =
  HsqlStmt.Statement "ROLLBACK" HsqlE.noParams HsqlD.noResult True

transactionSaveWithIsolation :: IsolationLevel -> DbM ()
transactionSaveWithIsolation isolationLevel = do
  -- Commit current transaction
  runSession $ HsqlS.statement () commitTransactionStmt
  -- Begin new transaction with specified isolation level
  runSession $ HsqlS.statement () (beginTransactionStmt isolationLevel)

setDefaultIsolationLevel :: HsqlCon.Connection -> IO ()
setDefaultIsolationLevel conn = do
  result <- HsqlS.run (HsqlS.statement () setIsolationStmt) conn
  case result of
    Left err -> throwIO $ DbError $ "Failed to set isolation level: " <> Text.pack (show err)
    Right _ -> pure ()
  where
    setIsolationStmt =
      HsqlStmt.Statement
        "SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ"
        HsqlE.noParams
        HsqlD.noResult
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
