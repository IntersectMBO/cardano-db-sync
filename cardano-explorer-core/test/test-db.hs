
import           Explorer.Core (LogFileDir (..), MigrationDir (..), PGPassFile (..), runMigrations)

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase)

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Database"
    [ testCase "Migration" testMigration
    ]

testMigration :: IO ()
testMigration =
  runMigrations True (PGPassFile "../config/pgpass") (MigrationDir "../schema") (LogFileDir "..")
