{-# LANGUAGE OverloadedStrings #-}
module Test.IO.Cardano.Db.Insert
  ( tests
  ) where

import           Control.Monad (void)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Cardano.Db

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Test.IO.Cardano.Db.Util


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
    slid <- insertSlotLeader testSlotLeader
    void $ deleteCascadeBlock (blockOne slid)
    void $ deleteCascadeBlock (blockZero slid)
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    bid0 <- insertBlock (blockZero slid)
    bid1 <- insertBlock (blockZero slid)
    assertBool (show bid0 ++ " /= " ++ show bid1) (bid0 == bid1)


insertFirstTest :: IO ()
insertFirstTest =
  runDbNoLogging $ do
    -- Delete the block if it exists.
    slid <- insertSlotLeader testSlotLeader
    void $ deleteCascadeBlock (blockOne slid)
    -- Insert the same block twice.
    bid0 <- insertBlock (blockZero slid)
    bid1 <- insertBlock $ (\b -> b { blockPrevious = Just bid0 }) (blockOne slid)
    assertBool (show bid0 ++ " == " ++ show bid1) (bid0 /= bid1)


blockZero :: SlotLeaderId -> Block
blockZero slid =
  Block (mkHash '\0') (Just 0) Nothing Nothing Nothing Nothing slid 42 dummyUTCTime 0

blockOne :: SlotLeaderId -> Block
blockOne slid =
  Block (mkHash '\1') (Just 0) (Just 0) (Just 1) Nothing (Just $ mkMerkelRoot 1) slid 42 dummyUTCTime 0

mkHash :: Char -> ByteString
mkHash = BS.pack . replicate 32

