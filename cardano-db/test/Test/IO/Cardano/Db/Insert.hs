{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Insert (
  tests,
) where

import Cardano.Db
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Time.Clock
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Insert"
    [ testCase "Insert zeroth block" insertZeroTest
    , testCase "Insert first block" insertFirstTest
    , testCase "Insert twice" insertTwice
    , testCase "Insert foreign key missing" insertForeignKeyMissing
    , testCase "Off-chain vote data re-fetch does not duplicate children" insertOffChainVoteDataNoDuplicateChildren
    ]

insertZeroTest :: IO ()
insertZeroTest =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    -- Delete the blocks if they exist.
    slid <- insertSlotLeader testSlotLeader
    void $ deleteBlock TxOutVariantCore (blockOne slid)
    void $ deleteBlock TxOutVariantCore (blockZero slid)
    -- Insert the same block twice. The first should be successful (resulting
    -- in a 'Right') and the second should return the same value in a 'Left'.
    bid0 <- insertCheckUniqueBlock (blockZero slid)
    bid1 <- insertCheckUniqueBlock (blockZero slid)
    assertBool (show bid0 ++ " /= " ++ show bid1) (bid0 == bid1)

insertFirstTest :: IO ()
insertFirstTest =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    -- Delete the block if it exists.
    slid <- insertSlotLeader testSlotLeader
    void $ deleteBlock TxOutVariantCore (blockOne slid)
    -- Insert the same block twice.
    bid0 <- insertCheckUniqueBlock (blockZero slid)
    bid1 <- insertCheckUniqueBlock $ (\b -> b {blockPreviousId = Just bid0}) (blockOne slid)
    assertBool (show bid0 ++ " == " ++ show bid1) (bid0 /= bid1)

insertTwice :: IO ()
insertTwice =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertCheckUniqueBlock (blockZero slid)
    let adaPots = adaPotsZero bid
    _ <- insertAdaPots adaPots
    pots0 <- fromJust <$> queryAdaPotsIdTest bid
    -- Insert with same Unique key, different first field
    _ <- insertAdaPots (adaPots {adaPotsSlotNo = 1 + adaPotsSlotNo adaPots})
    pots0' <- fromJust <$> queryAdaPotsIdTest bid
    assertBool
      (show (adaPotsSlotNo pots0) ++ " /= " ++ show (adaPotsSlotNo pots0'))
      (adaPotsSlotNo pots0 == adaPotsSlotNo pots0')

insertForeignKeyMissing :: IO ()
insertForeignKeyMissing = do
  time <- getCurrentTime
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertCheckUniqueBlock (blockZero slid)
    txid <- insertTx (txZero bid)
    phid <- insertPoolHash poolHash0
    pmrid <- insertPoolMetadataRef (poolMetadataRef txid phid)

    let fe = offChainPoolFetchError phid pmrid time
    insertCheckOffChainPoolFetchError fe

    count0 <- countOffChainPoolFetchError
    assertBool (show count0 ++ "/= 1") (count0 == 1)

    -- Delete with extracted functions
    deleteOffChainPoolFetchErrorByPmrId pmrid
    deletePoolMetadataRefById pmrid

    count1 <- countOffChainPoolFetchError
    assertBool (show count1 ++ "/= 0") (count1 == 0)

    insertCheckOffChainPoolFetchError fe

    count2 <- countOffChainPoolFetchError
    assertBool (show count2 ++ "/= 0") (count2 == 0)

-- Regression test for #1966: re-fetching an already-stored anchor must not duplicate its children.
insertOffChainVoteDataNoDuplicateChildren :: IO ()
insertOffChainVoteDataNoDuplicateChildren = do
  t <- getCurrentTime
  -- Unique per run so the test is independent of rows left by other cases.
  let stamp = filter (/= ' ') (show t)
      vaHash = BS.take 32 (BS.pack stamp <> BS.replicate 32 'v')
      ocvdHash = BS.take 32 (BS.pack stamp <> BS.replicate 32 'o')
      vaUrl = VoteUrl ("anchor://dedup/" <> Text.pack stamp)
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertCheckUniqueBlock (blockZero slid)
    vaid <- insertVotingAnchor (drepVotingAnchor vaUrl vaHash bid)
    let ocvd = drepOffChainVoteData vaid ocvdHash

    parentsBefore <- countOffChainVoteData
    childrenBefore <- countOffChainVoteDrepData

    -- First fetch: parent is new, so one child is inserted.
    new1 <- insertBulkOffChainVoteDataReturningNew [ocvd]
    insertBulkOffChainVoteDrepData [drepData ocvdId | (_, _, ocvdId) <- new1]

    -- Re-fetch the same anchor: parent already exists, so nothing new -> no extra child.
    new2 <- insertBulkOffChainVoteDataReturningNew [ocvd]
    insertBulkOffChainVoteDrepData [drepData ocvdId | (_, _, ocvdId) <- new2]

    parentsAfter <- countOffChainVoteData
    childrenAfter <- countOffChainVoteDrepData

    assertBool
      ("re-fetch should report no new parents, got " ++ show (length new2))
      (null new2)
    assertBool
      ("off_chain_vote_data delta should be 1, got " ++ show (parentsAfter - parentsBefore))
      (parentsAfter == parentsBefore + 1)
    assertBool
      ("off_chain_vote_drep_data delta should be 1 (not 2), got " ++ show (childrenAfter - childrenBefore))
      (childrenAfter == childrenBefore + 1)

drepVotingAnchor :: VoteUrl -> ByteString -> BlockId -> VotingAnchor
drepVotingAnchor url dataHash bid =
  VotingAnchor
    { votingAnchorUrl = url
    , votingAnchorDataHash = dataHash
    , votingAnchorType = DrepAnchor
    , votingAnchorBlockId = bid
    }

drepOffChainVoteData :: VotingAnchorId -> ByteString -> OffChainVoteData
drepOffChainVoteData vaid dataHash =
  OffChainVoteData
    { offChainVoteDataVotingAnchorId = vaid
    , offChainVoteDataHash = dataHash
    , offChainVoteDataJson = "{}"
    , offChainVoteDataBytes = "{}"
    , offChainVoteDataWarning = Nothing
    , offChainVoteDataLanguage = ""
    , offChainVoteDataComment = Nothing
    , offChainVoteDataIsValid = Just True
    }

drepData :: OffChainVoteDataId -> OffChainVoteDrepData
drepData ocvdId =
  OffChainVoteDrepData
    { offChainVoteDrepDataOffChainVoteDataId = ocvdId
    , offChainVoteDrepDataPaymentAddress = Nothing
    , offChainVoteDrepDataGivenName = "Test DRep"
    , offChainVoteDrepDataObjectives = Nothing
    , offChainVoteDrepDataMotivations = Nothing
    , offChainVoteDrepDataQualifications = Nothing
    , offChainVoteDrepDataImageUrl = Nothing
    , offChainVoteDrepDataImageHash = Nothing
    }

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

adaPotsZero :: BlockId -> AdaPots
adaPotsZero bid =
  AdaPots
    { adaPotsSlotNo = 0
    , adaPotsEpochNo = 0
    , adaPotsTreasury = DbLovelace 0
    , adaPotsReserves = DbLovelace 0
    , adaPotsRewards = DbLovelace 0
    , adaPotsUtxo = DbLovelace 0
    , adaPotsDepositsStake = DbLovelace 0
    , adaPotsDepositsDrep = DbLovelace 0
    , adaPotsDepositsProposal = DbLovelace 0
    , adaPotsFees = DbLovelace 0
    , adaPotsBlockId = bid
    }

txZero :: BlockId -> Tx
txZero bid =
  Tx
    { txHash = mkHash 32 '2'
    , txBlockId = bid
    , txBlockIndex = 0
    , txOutSum = DbLovelace 0
    , txFee = DbLovelace 0
    , txDeposit = Just 0
    , txSize = 0
    , txInvalidBefore = Nothing
    , txInvalidHereafter = Nothing
    , txValidContract = True
    , txScriptSize = 0
    , txTreasuryDonation = DbLovelace 0
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
    , poolMetadataRefUrl = PoolUrl "best.pool.com"
    , poolMetadataRefHash = mkHash 32 '4'
    , poolMetadataRefRegisteredTxId = txid
    }

offChainPoolFetchError :: PoolHashId -> PoolMetadataRefId -> UTCTime -> OffChainPoolFetchError
offChainPoolFetchError phid pmrid time =
  OffChainPoolFetchError
    { offChainPoolFetchErrorPoolId = phid
    , offChainPoolFetchErrorFetchTime = time
    , offChainPoolFetchErrorPmrId = pmrid
    , offChainPoolFetchErrorFetchError = "too good"
    , offChainPoolFetchErrorRetryCount = 5
    }

mkHash :: Int -> Char -> ByteString
mkHash n = BS.pack . replicate n
