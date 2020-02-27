{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Cardano.Db.Migration
  ( tests
  ) where

import           Cardano.Db

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
migrationTest =
  runMigrations id True (MigrationDir "../schema") (LogFileDir "/tmp")
