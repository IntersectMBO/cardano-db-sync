{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Cardano.Db.Migration
  ( tests
  ) where

import           Cardano.Db

import           Control.Monad (unless)

import qualified Data.List as List
import           Data.Maybe (fromMaybe)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)


tests :: TestTree
tests =
  testGroup "Migration"
    [ testCase "Migration is idempotent" migrationTest
    ]

-- Really just make sure that the migrations do actually run correctly.
-- If they fail the file path of the log file (in /tmp) will be printed.
migrationTest :: IO ()
migrationTest = do
  let schemaDir = MigrationDir "../schema"
  pgConfig <- readPGPassFileEnv Nothing
  runMigrations pgConfig True schemaDir (Just $ LogFileDir "/tmp")
  expected <- readSchemaVersion schemaDir
  actual <- getDbSchemaVersion
  unless (expected == actual) $
    error $ mconcat
            [ "Schema version mismatch. Expected "
            , showSchemaVersion expected
            , " but got "
            , showSchemaVersion actual
            , "."
            ]

getDbSchemaVersion :: IO SchemaVersion
getDbSchemaVersion =
  runDbNoLogging $
    fromMaybe (error "getDbSchemaVersion: Nothing") <$> querySchemaVersion

readSchemaVersion :: MigrationDir -> IO SchemaVersion
readSchemaVersion migrationDir = do
    xs <- filter ignoreVersion . fmap fst <$> getMigrationScripts migrationDir
    pure $ List.foldl' func (SchemaVersion 0 0 0) xs
  where
    func :: SchemaVersion -> MigrationVersion -> SchemaVersion
    func (SchemaVersion a b c) mv =
      case mvStage mv of
        1 -> SchemaVersion (max (mvVersion mv) a) b c
        2 -> SchemaVersion a (max (mvVersion mv) b) c
        3 -> SchemaVersion a b (max (mvVersion mv) c)
        _otherwise -> SchemaVersion a b c

    -- Ignore all migrations with version numbers of 9000 or above.
    ignoreVersion :: MigrationVersion -> Bool
    ignoreVersion mv = mvVersion mv < 9000

showSchemaVersion :: SchemaVersion -> String
showSchemaVersion (SchemaVersion a b c) =
  List.intercalate "." [show a, show b, show c]
