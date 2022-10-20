{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IO.Cardano.Db.Insert
  ( tests
  ) where

import           Cardano.Db
import           Cardano.Slotting.Block (BlockNo (..))
import           Control.Monad (void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Either (fromRight)
import           Test.IO.Cardano.Db.Util
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

-- TODO: redo these tests so that the db is cleaned between each test.
tests :: TestTree
tests =
  testGroup "Insert"
    [ testCase "Insert zeroth block" insertZeroTest
    , testCase "Insert first block" insertFirstTest
    , testCase "Insert twice" insertTwice
    ]

insertZeroTest :: IO ()
insertZeroTest =
  runDbNoLoggingEnv $ do
    -- Delete the blocks if they exist.
    slid <- insertSlotLeader testSlotLeader
    -- Insert a block and query it with its hash
    let blk = blockZero slid
    _ <- insertBlock blk
    blockNo <- unBlockNo . fromRight (error "Failed to find block hash") <$> queryBlockHashBlockNo (blockHash blk)
    assertBool (show (blockBlockNo blk) ++ " /= " ++ show blockNo) (blockBlockNo blk == fromIntegral blockNo)


insertFirstTest :: IO ()
insertFirstTest =
  runDbNoLoggingEnv $ do
    -- Insert the same block twice.
    bid1 <- insertBlock $ (\b -> b { blockBlockNo = 1 }) (blockOne slid)
    blockNo <- unBlockNo . fromRight (error "Failed to find block hash") <$> queryBlockHashBlockNo (blockHash blk)
    assertBool (show bid0 ++ " == " ++ show bid1) (bid0 /= bid1)

insertTwice :: IO ()
insertTwice =
  runDbNoLoggingEnv $ do
    let adaPots = adaPotsZero 0
    _ <- insertAdaPots adaPots
    Just pots0 <- queryAdaPots 0
    -- Insert with same Unique key, different first field
    _ <- insertAdaPots (adaPots { adaPotsSlotNo = 1 + adaPotsSlotNo adaPots })
    Just pots0' <- queryAdaPots 0
    assertBool (show (adaPotsSlotNo pots0) ++ " /= " ++ show (adaPotsSlotNo pots0'))
      (adaPotsSlotNo pots0 == adaPotsSlotNo pots0')

blockZero :: SlotLeaderId -> Block
blockZero slid =
  Block
    { blockHash = mkHash 32 '\0'
    , blockEpochNo = Just 0
    , blockSlotNo = Just 0
    , blockEpochSlotNo = Just 0
    , blockBlockNo = 0
    , blockSlotLeaderId = slid
    , blockSize = 42
    , blockTime = dummyUTCTime
    , blockTxCount = 0
    , blockProtoMajor = 1
    , blockProtoMinor = 0
    , blockVrfKey = Nothing
    , blockOpCert = Nothing
    , blockOpCertCounter = Nothing
    }


blockOne :: SlotLeaderId -> Block
blockOne slid =
  Block
    { blockHash = mkHash 32 '\1'
    , blockEpochNo = Just 0
    , blockSlotNo = Just 1
    , blockEpochSlotNo = Just 1
    , blockBlockNo = 1
    , blockSlotLeaderId = slid
    , blockSize = 42
    , blockTime = dummyUTCTime
    , blockTxCount = 0
    , blockProtoMajor = 1
    , blockProtoMinor = 0
    , blockVrfKey = Nothing
    , blockOpCert = Nothing
    , blockOpCertCounter = Nothing
    }

adaPotsZero :: BlockNo -> AdaPots
adaPotsZero (BlockNo blkNo) =
  AdaPots
    { adaPotsSlotNo = 0
    , adaPotsEpochNo = 0
    , adaPotsTreasury = DbLovelace 0
    , adaPotsReserves = DbLovelace 0
    , adaPotsRewards = DbLovelace 0
    , adaPotsUtxo = DbLovelace 0
    , adaPotsDeposits = DbLovelace 0
    , adaPotsFees = DbLovelace 0
    , adaPotsBlockNo = fromIntegral blkNo
    }

mkHash :: Int -> Char -> ByteString
mkHash n = BS.pack . replicate n

