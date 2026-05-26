{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for the @epoch@ view that aggregates @block@/@tx@ into
-- per-epoch totals. Covers the encoding paths used by the view's decoder
-- (e.g. @numeric@ values with non-zero @base10Exponent@).
module Test.IO.Cardano.Db.EpochCalc (
  tests,
) where

import Cardano.Db
import Data.WideWord.Word128 (Word128)
import Data.Word (Word64)
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "EpochCalc"
    [ testCase "epoch view decodes Word128 sums with trailing zeros" word128TrailingZerosTest
    , testCase "epoch view aggregates realistic large sums" epochViewLargeSumsTest
    ]

-- | Drive a tx@out_sum@ with many trailing zeros through the view so the
-- aggregated @SUM@ is returned by PostgreSQL with a non-zero @base10Exponent@.
-- This exercises 'word128Decoder' against the normalised numeric form.
word128TrailingZerosTest :: IO ()
word128TrailingZerosTest =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertBlock (mkBlock 0 slid)

    -- 380_000_000_000_000_000 lovelace = ~38B ADA. PostgreSQL stores this as
    -- a numeric whose coefficient is 38 with a base10 exponent of 16.
    let bigOutSum :: Word64
        bigOutSum = 380_000_000_000_000_000
        bigFee :: Word64
        bigFee = 36_000_000_000
        expectedOutSum :: Word128
        expectedOutSum = fromIntegral bigOutSum
        expectedFees :: DbLovelace
        expectedFees = DbLovelace bigFee

    _ <-
      insertTx
        Tx
          { txHash = mkTxHash bid 0
          , txBlockId = bid
          , txBlockIndex = 0
          , txOutSum = DbLovelace bigOutSum
          , txFee = DbLovelace bigFee
          , txDeposit = Just 0
          , txSize = 12
          , txInvalidHereafter = Nothing
          , txInvalidBefore = Nothing
          , txValidContract = True
          , txScriptSize = 0
          , txTreasuryDonation = DbLovelace 0
          }

    setEpochSyncEnabled True
    appendEpochFinalized 0
    decoded <- queryEpochEntry 0 >>= extractDbResult

    assertBool
      ( "epochOutSum mismatch:\n  expected: "
          ++ show expectedOutSum
          ++ "\n  got:      "
          ++ show (epochOutSum decoded)
      )
      (epochOutSum decoded == expectedOutSum)
    assertBool
      ( "epochFees mismatch:\n  expected: "
          ++ show expectedFees
          ++ "\n  got:      "
          ++ show (epochFees decoded)
      )
      (epochFees decoded == expectedFees)

-- | Insert a block plus several txs whose @out_sum@ and @fee@ totals are big
-- (but realistic for a busy epoch), then verify the @epoch@ view aggregates
-- and decodes them correctly.
epochViewLargeSumsTest :: IO ()
epochViewLargeSumsTest =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertBlock (mkBlock 0 slid)

    -- Per-tx out_sum of 1e15 lovelace (1M ADA) summed across 10 txs gives
    -- 1e16 lovelace - well above Int32 range, exercising the SUM path
    -- properly without straining Word64 limits.
    let perTxOutSum :: Word64
        perTxOutSum = 1_000_000_000_000_000
        perTxFee :: Word64
        perTxFee = 3_600_000_000
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

    setEpochSyncEnabled True
    appendEpochFinalized 0
    calcEpoch <- queryEpochEntry 0 >>= extractDbResult

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
