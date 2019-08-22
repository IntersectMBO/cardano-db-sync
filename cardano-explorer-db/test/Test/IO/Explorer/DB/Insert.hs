{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Explorer.DB.Insert
  ( tests
  ) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Either (isLeft, isRight)

import           Explorer.DB

import           Test.HUnit.Base (assertBool, assertEqual, assertString)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup "Insert"
    [ testCase "Insert zeroth block" insertZeroTest
    , testCase "Insert first block" insertFirstTest
    ]

insertZeroTest :: IO ()
insertZeroTest =
  runDbNoLogging $ do
    -- Delete the blocks if they exist.
    void $ deleteBlock blockOne
    void $ deleteBlock blockZero
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    bid1 <- insertBlock blockZero
    liftIO $ assertBool "Should be Right" (isRight bid1)
    bid2 <- insertBlock blockZero
    liftIO $ assertBool "Should be Left" (isLeft bid2)
    liftIO $ case (bid1, bid2) of
                (Right a, Left b) -> assertEqual (show a ++ " /= " ++ show b) a b
                _ -> assertString ("This is wrong: " ++ show (bid1, bid2))


insertFirstTest :: IO ()
insertFirstTest =
  runDbNoLogging $ do
    -- Delete the block if it exists.
    void $ deleteBlock blockOne
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    bid1 <- queryBlockId (blockHash blockZero)
    bid2 <- insertBlock $ blockOne { blockPrevious = bid1 }
    liftIO $ assertBool "Should be Right" (isRight bid2)


blockZero :: Block
blockZero = Block (mkHash '\0') Nothing 0 Nothing Nothing 42

blockOne :: Block
blockOne = Block (mkHash '\1') (Just 0) 1 Nothing (Just merkelHash) 42

mkHash :: Char -> ByteString
mkHash = BS.pack . replicate 32

merkelHash :: ByteString
merkelHash = BS.pack $ replicate 32 'a'
