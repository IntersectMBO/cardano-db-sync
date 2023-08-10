{-# LANGUAGE OverloadedStrings #-}

module Test.IO.Cardano.Db.Migration (
  tests,
) where

import Cardano.Db (
  LogFileDir (..),
  MigrationDir (..),
  MigrationToRun (..),
  MigrationValidate (..),
  MigrationValidateError (..),
  MigrationVersion (..),
  SchemaVersion (..),
  getMigrationScripts,
  querySchemaVersion,
  readPGPassDefault,
  runDbNoLoggingEnv,
  runMigrations,
  runOrThrowIODb,
  validateMigrations,
 )
import Control.Monad (unless, when)
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Migration"
    [ testCase "Migration script names do not clash" migrationScriptNameTest
    , testCase "Migration is idempotent" migrationTest
    , testCase "Migration validation - unknown official migration found" unknownMigrationValidate
    , testCase "Migration validation - mismatched hash for migration" invalidHashMigrationValidate
    , testCase "Migration validation - mismatched hash for migration 2" invalidHashMigrationValidate'
    ]

unknownMigrationValidate :: IO ()
unknownMigrationValidate = do
  result <- validateMigrations testSchemaDir knownTestMigrations
  unless (result == Just (expected, False)) $
    error $
      mconcat
        [ "Schema version mismatch. Expected "
        , show expected
        , " but got "
        , show result
        , "."
        ]
  where
    expected :: MigrationValidateError
    expected =
      UnknownMigrationsFound
        { missingMigrations =
            [ MigrationValidate
                { mvHash = "hash"
                , mvFilepath = "migration-1-0000-20190730.sql"
                }
            ]
        , extraMigrations =
            [ MigrationValidate
                { mvHash = "395187b4157ef5307b7d95e0150542e09bb19679055eee8017a34bcca89a691d"
                , mvFilepath = "migration-1-0000-20190730.sql"
                }
            ]
        }

invalidHashMigrationValidate :: IO ()
invalidHashMigrationValidate = do
  result <- validateMigrations testSchemaDir knownTestMigrations
  unless (result == Just (expected, False)) $
    error $
      mconcat
        [ "Schema version mismatch. Expected "
        , show expected
        , " but got "
        , show result
        , "."
        ]
  where
    expected :: MigrationValidateError
    expected =
      UnknownMigrationsFound
        { missingMigrations =
            [ MigrationValidate
                { mvHash = "hash"
                , mvFilepath = "migration-1-0000-20190730.sql"
                }
            ]
        , extraMigrations =
            [ MigrationValidate
                { mvHash = "395187b4157ef5307b7d95e0150542e09bb19679055eee8017a34bcca89a691d"
                , mvFilepath = "migration-1-0000-20190730.sql"
                }
            ]
        }

invalidHashMigrationValidate' :: IO ()
invalidHashMigrationValidate' = do
  let emptyMigrations = [] -- No known migrations from compiling
  result <- validateMigrations testSchemaDir emptyMigrations
  unless (result == Just (expected, False)) $
    error $
      mconcat
        [ "Schema version mismatch. Expected "
        , show expected
        , " but got "
        , show result
        , "."
        ]
  where
    expected :: MigrationValidateError
    expected =
      UnknownMigrationsFound
        { missingMigrations = []
        , extraMigrations =
            [ MigrationValidate
                { mvHash = "395187b4157ef5307b7d95e0150542e09bb19679055eee8017a34bcca89a691d"
                , mvFilepath = "migration-1-0000-20190730.sql"
                }
            ]
        }

-- Really just make sure that the migrations do actually run correctly.
-- If they fail the file path of the log file (in /tmp) will be printed.
migrationTest :: IO ()
migrationTest = do
  let schemaDir = MigrationDir "../schema"
  pgConfig <- runOrThrowIODb readPGPassDefault
  _ <- runMigrations pgConfig True schemaDir (Just $ LogFileDir "/tmp") Initial
  expected <- readSchemaVersion schemaDir
  actual <- getDbSchemaVersion
  unless (expected == actual) $
    error $
      mconcat
        [ "Schema version mismatch. Expected "
        , showSchemaVersion expected
        , " but got "
        , showSchemaVersion actual
        , "."
        ]

-- Check that the migration script names do not clash, where clash means two scripts have the
-- same `mvStage` and `mvVersion` components.
migrationScriptNameTest :: IO ()
migrationScriptNameTest = do
  xs <- fmap fst <$> getMigrationScripts (MigrationDir "../schema")
  when (null xs) $ error "Unable to find schema migration files"
  mapM_ validate $ List.groupOn mvStage xs
  where
    validate :: [MigrationVersion] -> IO ()
    validate xs =
      case xs of
        [] -> pure () -- Impossible
        (x : _) ->
          let versions = map mvVersion xs
           in when (List.nubOrd versions /= versions)
                . error
                $ "Stage " ++ show (mvStage x) ++ " migration scripts do not have unique version numbers."

getDbSchemaVersion :: IO SchemaVersion
getDbSchemaVersion =
  runDbNoLoggingEnv $
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
        _otherwise -> SchemaVersion a b c

    -- Ignore all migrations with version numbers of 9000 or above.
    ignoreVersion :: MigrationVersion -> Bool
    ignoreVersion mv = mvVersion mv < 9000

showSchemaVersion :: SchemaVersion -> String
showSchemaVersion (SchemaVersion a b c) =
  List.intercalate "." [show a, show b, show c]

testSchemaDir :: MigrationDir
testSchemaDir = MigrationDir "test/schema" -- Migration directory with single migration

knownTestMigrations :: [(Text, Text)]
knownTestMigrations = [("hash", "migration-1-0000-20190730.sql")]
