{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Explorer.DB.Insert
  ( tests
  ) where

import           Control.Monad (void)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Explorer.DB

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Test.IO.Explorer.DB.Util


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
    void $ deleteCascadeBlock blockOne
    void $ deleteCascadeBlock blockZero
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    bid0 <- insertBlock blockZero
    bid1 <- insertBlock blockZero
    assertBool (show bid0 ++ " /= " ++ show bid1) (bid0 == bid1)


insertFirstTest :: IO ()
insertFirstTest =
  runDbNoLogging $ do
    -- Delete the block if it exists.
    void $ deleteCascadeBlock blockOne
    -- Insert the same block twice.
    bid0 <- insertBlock blockZero
    bid1 <- insertBlock $ blockOne { blockPrevious = Just bid0 }
    assertBool (show bid0 ++ " == " ++ show bid1) (bid0 /= bid1)


blockZero :: Block
blockZero = Block (mkHash '\0') Nothing 0 Nothing Nothing 42

blockOne :: Block
blockOne = Block (mkHash '\1') (Just 0) 1 Nothing (Just $ mkMerkelRoot 1) 42

mkHash :: Char -> ByteString
mkHash = BS.pack . replicate 32

