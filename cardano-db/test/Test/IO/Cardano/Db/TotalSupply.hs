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
    bid0 <- insertBlock (mkBlock 0 slid)
    (tx0Ids :: [TxId]) <- mapM insertTx $ mkTxs bid0 4
    mapM_ (insertTxOut . mkTxOutCore bid0) tx0Ids
    count <- queryBlockCount
    assertBool ("Block count should be 1, got " ++ show count) (count == 1)
    supply0 <- queryTotalSupply TxOutCore
    assertBool "Total supply should not be > 0" (supply0 > Ada 0)

    -- Spend from the Utxo set.
    bid1 <- insertBlock (mkBlock 1 slid)
    tx1Id <-
      insertTx $
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
    _ <- insertTxIn (TxIn tx1Id (head tx0Ids) 0 Nothing)
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
            , C.txOutMaTxOut = Nothing
            }
    supply1 <- queryTotalSupply TxOutCore
    assertBool ("Total supply should be < " ++ show supply0) (supply1 < supply0)
