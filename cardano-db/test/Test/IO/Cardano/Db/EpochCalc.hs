{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database integration tests covering the encoding/decoding of large numeric
-- values in the @epoch@ table and the SUM-based 'queryCalcEpochEntry' fallback.
--
-- These tests exist because of a regression introduced by the Persistent ->
-- Hasql migration where:
--
-- * 'word128Decoder' was reading the @coefficient@ of the 'Scientific' returned
--   by Hasql's @numeric@ decoder without honouring @base10Exponent@. PostgreSQL
--   strips trailing zeros in its binary @numeric@ representation, so a value
--   like @380_000_000_000_000_000@ comes back as @Scientific 38 16@ and was
--   being decoded as @38@.
--
-- * 'queryCalcEpochEntryStmt' was decoding @SUM(tx.out_sum)@ and @SUM(tx.fee)@
--   (both @numeric@) as @int8@ and packing the result into the low 64 bits of
--   a 'Word128' via @Word128 0 (fromIntegral outSum)@.
module Test.IO.Cardano.Db.EpochCalc (
  tests,
) where

import Cardano.Db
import Control.Monad (void)
import Data.WideWord.Word128 (Word128)
import Data.Word (Word64)
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "EpochCalc"
    [ testCase "Word128 round-trip with trailing zeros" word128RoundTripWithTrailingZerosTest
    , testCase "queryCalcEpochEntry handles realistic large sums" queryCalcEpochEntryLargeSumsTest
    ]

-- | Insert an 'Epoch' row whose @out_sum@ has many trailing zeros - the kind of
-- value PostgreSQL is most likely to return in normalised form. Round-trip
-- through 'queryEpochEntry' must preserve the exact value.
word128RoundTripWithTrailingZerosTest :: IO ()
word128RoundTripWithTrailingZerosTest =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    -- The bug shows up most visibly with values that have many trailing zeros,
    -- because PostgreSQL stores them with a non-zero base10 exponent.
    let originalEpoch =
          Epoch
            { epochOutSum = 380_000_000_000_000_000 -- ~38B ADA in lovelace
            , epochFees = DbLovelace 36_000_000_000 -- ~36k ADA in lovelace
            , epochTxCount = 100
            , epochBlkCount = 21_600
            , epochNo = 999_999 -- Use a high epoch number unlikely to clash
            , epochStartTime = dummyUTCTime
            , epochEndTime = dummyUTCTime
            }
    void $ insertEpoch originalEpoch

    result <- queryEpochEntry (epochNo originalEpoch)
    decoded <- extractDbResult result
    assertBool
      ( "epoch round-trip mismatch:\n  expected: "
          ++ show originalEpoch
          ++ "\n  got:      "
          ++ show decoded
      )
      (decoded == originalEpoch)

-- | Insert a block plus several txs whose @out_sum@ and @fee@ totals are big
-- (but realistic for a busy epoch), then call 'queryCalcEpochEntry' and verify
-- the SUM is decoded correctly.
--
-- This exercises the cold-cache fallback that 'cardano-db-sync' uses when the
-- in-memory epoch cache is empty (server restart, rollback, @--disable-cache@,
-- or first epoch boundary after upgrade).
queryCalcEpochEntryLargeSumsTest :: IO ()
queryCalcEpochEntryLargeSumsTest =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertBlock (mkBlock 0 slid)

    -- Insert 10 txs with realistic large per-tx values. Per-tx out_sum of
    -- 1e15 lovelace (1M ADA) summed across 10 txs gives 1e16 lovelace - well
    -- above Int32 range, exercising the SUM path properly without straining
    -- Word64 limits.
    let perTxOutSum :: Word64
        perTxOutSum = 1_000_000_000_000_000 -- 1e15 lovelace = 1M ADA
        perTxFee :: Word64
        perTxFee = 3_600_000_000 -- 3,600 ADA
        nTxs :: Word64
        nTxs = 10
        expectedOutSum :: Word128
        expectedOutSum = fromIntegral (perTxOutSum * nTxs)
        expectedFees :: DbLovelace
        expectedFees = DbLovelace (perTxFee * nTxs)

    let bigTx i =
          Tx
            { txHash = mkTxHash bid i
            , txBlockId = bid
            , txBlockIndex = fromIntegral i
            , txOutSum = DbLovelace perTxOutSum
            , txFee = DbLovelace perTxFee
            , txDeposit = Just 0
            , txSize = 12
            , txInvalidHereafter = Nothing
            , txInvalidBefore = Nothing
            , txValidContract = True
            , txScriptSize = 0
            , txTreasuryDonation = DbLovelace 0
            }
    mapM_ (insertTx . bigTx) [0 .. nTxs - 1]

    -- mkBlock hard-codes epoch_no = 0
    calcEpoch <- queryCalcEpochEntry 0

    assertBool
      ( "epochOutSum mismatch:\n  expected: "
          ++ show expectedOutSum
          ++ "\n  got:      "
          ++ show (epochOutSum calcEpoch)
      )
      (epochOutSum calcEpoch == expectedOutSum)
    assertBool
      ( "epochFees mismatch:\n  expected: "
          ++ show expectedFees
          ++ "\n  got:      "
          ++ show (epochFees calcEpoch)
      )
      (epochFees calcEpoch == expectedFees)
    assertBool
      ( "epochTxCount mismatch:\n  expected: "
          ++ show nTxs
          ++ "\n  got:      "
          ++ show (epochTxCount calcEpoch)
      )
      (epochTxCount calcEpoch == nTxs)
