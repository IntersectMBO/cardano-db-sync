{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
#endif

module Test.IO.Cardano.Db.TotalSupply (
  tests,
) where

import Cardano.Db
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Data.Text as Text
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "TotalSupply"
    [ testCase "Initial supply correct" initialSupplyTest
    ]

initialSupplyTest :: IO ()
initialSupplyTest =
  runDbNoLoggingEnv $ do
    -- Delete the blocks if they exist.
    deleteAllBlocks

    -- Set up initial supply.
    slid <- insertSlotLeader testSlotLeader
    let bid0 = BlockKey 0
    insertBlock bid0 (mkBlock 0 slid)
    let tx0s = mkTxs bid0 4
    mapM_ (uncurry insertTx) tx0s
    mapM_ (insertTxOut . mkTxOutCore bid0) (fst <$> tx0s)
    count <- queryBlockCount
    assertBool ("Block count should be 1, got " ++ show count) (count == 1)
    supply0 <- queryTotalSupply TxOutCore
    assertBool "Total supply should not be > 0" (supply0 > Ada 0)

    -- Spend from the Utxo set.
    let bid1 = BlockKey 1
    insertBlock bid1 (mkBlock 1 slid)
    let tx1Id = toTxId bid1 0
    insertTx tx1Id $
      Tx
        { txHash = mkTxHash bid1 1
        , txBlockId = bid1
        , txBlockIndex = 0
        , txOutSum = DbLovelace 500000000
        , txFee = DbLovelace 100
        , txDeposit = Just 0
        , txSize = 123
        , txInvalidHereafter = Nothing
        , txInvalidBefore = Nothing
        , txValidContract = True
        , txScriptSize = 0
        , txTreasuryDonation = DbLovelace 0
        }
    _ <- insertTxIn (TxIn tx1Id (fst $ head tx0s) 0 Nothing)
    let addr = mkAddressHash bid1 tx1Id
    _ <-
      insertTxOut $
        CTxOutW $
          C.TxOut
            { C.txOutTxId = tx1Id
            , C.txOutIndex = 0
            , C.txOutAddress = Text.pack addr
            , C.txOutAddressHasScript = False
            , C.txOutPaymentCred = Nothing
            , C.txOutStakeAddressId = Nothing
            , C.txOutValue = DbLovelace 500000000
            , C.txOutDataHash = Nothing
            , C.txOutInlineDatumId = Nothing
            , C.txOutReferenceScriptId = Nothing
            , C.txOutConsumedByTxId = Nothing
            }
    supply1 <- queryTotalSupply TxOutCore
    assertBool ("Total supply should be < " ++ show supply0) (supply1 < supply0)
