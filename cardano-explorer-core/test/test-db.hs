{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString.Char8 (ByteString)
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
    [ testCase "Migration is idempotent" migrationTest
    , testCase "Insert zeroth block" insertZeroTest
    , testCase "Insert first block" insertFirstTest
    ]

migrationTest :: IO ()
migrationTest =
  runMigrations True (MigrationDir "../schema") (LogFileDir "..")

insertZeroTest :: IO ()
insertZeroTest =
  runDbAction $ do
    -- Delete the blocks if they exist.
    void $ deleteBlock blockOne
    void $ deleteBlock blockZero
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    one <- insertBlock blockZero
    liftIO $ assertBool "Should be Right" (isRight one)
    two <- insertBlock blockZero
    liftIO $ assertBool "Should be Left" (isLeft two)
    liftIO $ case (one, two) of
                (Right a, Left b) -> assertEqual (show a ++ " /= " ++ show b) a b
                _ -> assertString ("This is wrong: " ++ show (one, two))


insertFirstTest :: IO ()
insertFirstTest =
  runDbAction $ do
    -- Delete the block if it exists.
    void $ deleteBlock blockOne
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    bid <- queryBlockId (blockHash blockZero)
    one <- insertBlock $ blockOne { blockPrevious = bid }
    liftIO $ assertBool "Should be Right" (isRight one)



blockZero :: Block
blockZero = Block (mkHash '\0') 0 Nothing 0 Nothing merkelHash 42

blockOne :: Block
blockOne = Block (mkHash '\1') 0 (Just 0) 1 Nothing merkelHash 42

mkHash :: Char -> ByteString
mkHash = BS.pack . replicate 32

merkelHash :: ByteString
merkelHash = BS.pack $ replicate 32 'a'
