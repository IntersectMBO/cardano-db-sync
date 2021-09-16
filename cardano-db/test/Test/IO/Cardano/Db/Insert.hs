{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IO.Cardano.Db.Insert
  ( tests
  ) where

import           Control.Monad (void)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock

import           Database.Persist.Sql (Entity, delete, selectList)

import           Cardano.Db

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Test.IO.Cardano.Db.Util


tests :: TestTree
tests =
  testGroup "Insert"
    [ testCase "Insert zeroth block" insertZeroTest
    , testCase "Insert first block" insertFirstTest
    , testCase "Insert twice" insertTwice
    , testCase "Insert foreign key" insertForeignKey
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
    bid0 <- insertBlockChecked (blockZero slid)
    bid1 <- insertBlockChecked (blockZero slid)
    assertBool (show bid0 ++ " /= " ++ show bid1) (bid0 == bid1)


insertFirstTest :: IO ()
insertFirstTest =
  runDbNoLogging $ do
    -- Delete the block if it exists.
    slid <- insertSlotLeader testSlotLeader
    void $ deleteCascadeBlock (blockOne slid)
    -- Insert the same block twice.
    bid0 <- insertBlockChecked (blockZero slid)
    bid1 <- insertBlockChecked $ (\b -> b { blockPreviousId = Just bid0 }) (blockOne slid)
    assertBool (show bid0 ++ " == " ++ show bid1) (bid0 /= bid1)

insertTwice :: IO ()
insertTwice =
  runDbNoLogging $ do
    slid <- insertSlotLeader testSlotLeader
    bid <- insertBlockChecked (blockZero slid)
    let adaPots = adaPotsZero bid
    _ <- insertAdaPots adaPots
    Just pots0 <- queryAdaPots bid
    -- Insert with same Unique key, different first field
    _ <- insertAdaPots (adaPots {adaPotsSlotNo = 1 + adaPotsSlotNo adaPots})
    Just pots0' <- queryAdaPots bid
    assertBool (show (adaPotsSlotNo pots0) ++ " /= " ++ show (adaPotsSlotNo pots0'))
      (adaPotsSlotNo pots0 == adaPotsSlotNo pots0')

insertForeignKey :: IO ()
insertForeignKey = do
  time <- getCurrentTime
  runDbNoLogging $ do
    slid <- insertSlotLeader testSlotLeader
    bid <- insertBlockChecked (blockZero slid)
    txid <- insertTx (txZero bid)
    phid <- insertPoolHash poolHash0
    pmrid <- insertPoolMetadataRef $ poolMetadataRef txid phid
    void $ insertAbortForeignKey print $ void $ insertPoolOfflineFetchError $
      poolOfflineFetchError phid pmrid time
    delete pmrid
    void $ insertAbortForeignKey print $ void $ insertPoolOfflineFetchError $
      poolOfflineFetchError phid pmrid time
    ls :: [Entity PoolOfflineFetchError] <- selectList [] []
    assertBool (show (length ls) ++ "/= 0") (length ls == 0)

blockZero :: SlotLeaderId -> Block
blockZero slid =
  Block
    { blockHash = mkHash 32 '\0'
    , blockEpochNo = Just 0
    , blockSlotNo = Just 0
    , blockEpochSlotNo = Just 0
    , blockBlockNo = Just 0
    , blockPreviousId = Nothing
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
    , blockBlockNo = Just 1
    , blockPreviousId = Nothing
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

txZero :: BlockId -> Tx
txZero bid =
  Tx
    { txHash = mkHash 32 '2'
    , txBlockId = bid
    , txBlockIndex = 0
    , txOutSum = DbLovelace 0
    , txFee = DbLovelace 0
    , txDeposit = 0
    , txSize = 0
    , txInvalidBefore = Nothing
    , txInvalidHereafter = Nothing
    , txValidContract = True
    , txScriptSize = 0
    }

poolHash0 :: PoolHash
poolHash0 =
  PoolHash
    { poolHashHashRaw = mkHash 28 '0'
    , poolHashView = "best pool"
    }

poolMetadataRef :: TxId -> PoolHashId -> PoolMetadataRef
poolMetadataRef txid phid =
  PoolMetadataRef
    { poolMetadataRefPoolId = phid
    , poolMetadataRefUrl = "best.pool.com"
    , poolMetadataRefHash = mkHash 32 '4'
    , poolMetadataRefRegisteredTxId = txid
    }

adaPotsZero :: BlockId -> AdaPots
adaPotsZero bid =
  AdaPots
    { adaPotsSlotNo = 0
    , adaPotsEpochNo = 0
    , adaPotsTreasury = DbLovelace 0
    , adaPotsReserves = DbLovelace 0
    , adaPotsRewards = DbLovelace 0
    , adaPotsUtxo = DbLovelace 0
    , adaPotsDeposits = DbLovelace 0
    , adaPotsFees = DbLovelace 0
    , adaPotsBlockId = bid
    }

poolOfflineFetchError :: PoolHashId -> PoolMetadataRefId -> UTCTime -> PoolOfflineFetchError
poolOfflineFetchError phid pmrid time =
  PoolOfflineFetchError
    { poolOfflineFetchErrorPoolId = phid
    , poolOfflineFetchErrorFetchTime = time
    , poolOfflineFetchErrorPmrId = pmrid
    , poolOfflineFetchErrorFetchError = "too good"
    , poolOfflineFetchErrorRetryCount = 5
    }

mkHash :: Int -> Char -> ByteString
mkHash n = BS.pack . replicate n

