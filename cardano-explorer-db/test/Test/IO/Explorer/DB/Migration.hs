{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Explorer.DB.Migration
  ( tests
  ) where

import           Explorer.DB

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
  runMigrations True (MigrationDir "../schema") (LogFileDir "/tmp")
