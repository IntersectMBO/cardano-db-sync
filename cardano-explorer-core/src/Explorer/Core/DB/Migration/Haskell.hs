{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Core.DB.Migration.Haskell
  ( runHaskellMigration
  ) where

import           Control.Exception (SomeException, handle)
import           Control.Monad.Logger (MonadLogger, LoggingT, runLoggingT, defaultLogStr)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)

import qualified Data.ByteString.Char8 as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql (SqlBackend, SqlPersistT)

import           Explorer.Core.DB.PGConfig
import           Explorer.Core.DB.Migration.Version

import           System.Exit (exitFailure)
import           System.IO (Handle, hClose, hFlush, hPutStrLn, stdout)
import           System.Log.FastLogger (fromLogStr)

-- | Run a migration written in Haskell (eg one that cannot easily be done in SQL).
-- The Haskell migration is paired with an SQL migration and uses the same MigrationVersion
-- numbering system. For example when 'migration-2-0008-20190731.sql' is applied this
-- function will be called and if a Haskell migration with that version number exists
-- in the 'migrationMap' it will be run.
--
-- An example of how this may be used is:
--   1. 'migration-2-0008-20190731.sql' adds a new NULL-able column.
--   2. Haskell migration 'MigrationVersion 2 8 20190731' populates new column from data already
--      in the database.
--   3. 'migration-2-0009-20190731.sql' makes the new column NOT NULL.

runHaskellMigration :: PGConfig -> Handle -> MigrationVersion -> IO ()
runHaskellMigration pgconf logHandle mversion =
    case Map.lookup mversion migrationMap of
      Nothing -> pure ()
      Just action -> do
        hPutStrLn logHandle $ "Running : migration-" ++ renderMigrationVersion mversion ++ ".hs"
        putStr $ "    migration-" ++ renderMigrationVersion mversion ++ ".hs  ... "
        hFlush stdout
        handle handler $ runDbAction logHandle pgconf action
        putStrLn "ok"
  where
    handler :: SomeException -> IO a
    handler e = do
      putStrLn $ "runHaskellMigration: " ++ show e
      hPutStrLn logHandle $ "runHaskellMigration: " ++ show e
      hClose logHandle
      exitFailure

--------------------------------------------------------------------------------

migrationMap :: MonadLogger m => Map MigrationVersion (SqlPersistT m ())
migrationMap =
  Map.fromList
    [ ( MigrationVersion 2 1 20190731, migration0001 )
    ]

runDbAction :: Handle -> PGConfig -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbAction logHandle pgconf dbAction =
  runHandlerLoggerT logHandle .
    withPostgresqlConn (toConnectionString pgconf) $ \backend ->
      runReaderT dbAction backend

runHandlerLoggerT :: Handle -> LoggingT m a -> m a
runHandlerLoggerT hdl action =
    runLoggingT action logOut
  where
    logOut loc src level msg =
      BS.hPutStrLn hdl . fromLogStr $ defaultLogStr loc src level msg

--------------------------------------------------------------------------------

migration0001 :: MonadLogger m => SqlPersistT m ()
migration0001 =
  -- Place holder.
  pure ()

--------------------------------------------------------------------------------

