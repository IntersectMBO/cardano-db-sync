{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as BS
import           Data.Either (isLeft, isRight)

import           Explorer.Core

import           Test.HUnit.Base (assertBool, assertEqual, assertString)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase)

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Database"
    [ testCase "Migration is idempotent" testMigration
    , testCase "Simple insert test" testInsert
    ]

testMigration :: IO ()
testMigration =
  runMigrations True (PGPassFile "../config/pgpass") (MigrationDir "../schema") (LogFileDir "..")

testInsert :: IO ()
testInsert = do
  runDbAction (PGPassFile "../config/pgpass") $ do
    -- Delete the block if it exists.
    void $ deleteBlock dummyBlock
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    one <- insertBlock dummyBlock
    liftIO $ assertBool "Should be Righ" (isRight one)
    two <- insertBlock dummyBlock
    liftIO $ assertBool "Should be Left" (isLeft two)
    liftIO $ case (one, two) of
                (Right a, Left b) -> assertEqual (show a ++ " /= " ++ show b) a b
                _ -> assertString ("This is wrong: " ++ show (one, two))



dummyBlock :: Block
dummyBlock =
    Block zeroHash 1 Nothing 2 Nothing zeroHash 42
  where
    zeroHash = BS.pack $ replicate 32 '0'
