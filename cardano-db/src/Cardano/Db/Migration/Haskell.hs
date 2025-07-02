{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Migration.Haskell (
  runHaskellMigration,
) where

import Cardano.Db.Migration.Version
import Cardano.Db.PGConfig
import qualified Cardano.Db.Types as DB
import Control.Monad.Logger (LoggingT)
import qualified Data.Map.Strict as Map
import System.IO (Handle, hPutStrLn)

-- Simplified version that just logs if executed
runHaskellMigration :: PGPassSource -> Handle -> MigrationVersion -> IO ()
runHaskellMigration _ logHandle mversion =
  hPutStrLn logHandle $ "No Haskell migration for version " ++ renderMigrationVersion mversion

-- Empty migration map
_migrationMap :: Map.Map MigrationVersion (DB.DbAction (LoggingT IO) ())
_migrationMap = Map.empty

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
-- runHaskellMigration :: PGPassSource -> Handle -> MigrationVersion -> IO ()
-- runHaskellMigration source logHandle mversion =
--   case Map.lookup mversion migrationMap of
--     Nothing -> pure ()
--     Just action -> do
--       hPutStrLn logHandle $ "Running : migration-" ++ renderMigrationVersion mversion ++ ".hs"
--       putStr $ "    migration-" ++ renderMigrationVersion mversion ++ ".hs  ... "
--       hFlush stdout
--       handle handler $ runDbHandleLogger logHandle source action
--       putStrLn "ok"
--   where
--     handler :: SomeException -> IO a
--     handler e = do
--       putStrLn $ "runHaskellMigration: " ++ show e
--       hPutStrLn logHandle $ "runHaskellMigration: " ++ show e
--       hClose logHandle
--       exitFailure

-- --------------------------------------------------------------------------------

-- migrationMap :: MonadLogger m => Map MigrationVersion (DB.DbAction m ())
-- migrationMap =
--   Map.fromList
--     [ (MigrationVersion 2 1 20190731, migration0001)
--     ]

-- --------------------------------------------------------------------------------

-- migration0001 :: MonadLogger m => DB.DbAction m ()
-- migration0001 =
--   -- Place holder.
--   pure ()

-- --------------------------------------------------------------------------------
