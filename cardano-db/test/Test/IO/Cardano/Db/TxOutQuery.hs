{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.TxOutQuery (
  tests,
) where

import Cardano.Db
import Cardano.Db.Schema.Variants.TxOutCore (TxOutCore (..))
import Control.Monad (void)
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "TxOutQuery"
    [ testCase "queryTxOutputs decodes an enterprise output" queryTxOutputsEnterprise
    , testCase "utxo-set query decodes an enterprise output" queryUtxoAtSlotEnterprise
    ]

-- Core variant only; the IO schema has no address table, so the Address path is
-- covered by the chain-gen suite.

-- Enterprise output has a null stake_address_id, so the decoder must consume the
-- selected id column or every field shifts by one (#2162).
queryTxOutputsEnterprise :: IO ()
queryTxOutputsEnterprise =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertCheckUniqueBlock (mkBlock 0 slid)
    txId <- case mkTxs bid 1 of
      (tx : _) -> insertTx tx
      [] -> error "mkTxs returned empty list"
    void $ insertTxOut (mkTxOutCore bid txId)
    outs <- queryTxOutputs TxOutVariantCore (fromIntegral $ getTxId txId)
    case outs of
      [VCTxOutW txOut] ->
        assertBool
          ("unexpected value: " ++ show (txOutCoreValue txOut))
          (txOutCoreValue txOut == DbLovelace 1000000000)
      _ -> assertBool ("expected one output, got " ++ show (length outs)) False

-- utxo-set uses a different decoder (queryUtxoAtBlockId), so it needs its own case.
queryUtxoAtSlotEnterprise :: IO ()
queryUtxoAtSlotEnterprise =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    bid <- insertCheckUniqueBlock (mkBlock 0 slid)
    txId <- case mkTxs bid 1 of
      (tx : _) -> insertTx tx
      [] -> error "mkTxs returned empty list"
    void $ insertTxOut (mkTxOutCore bid txId)
    utxos <- queryUtxoAtSlotNo TxOutVariantCore 0
    case utxos of
      [utxo] -> case utxoTxOutW utxo of
        VCTxOutW txOut ->
          assertBool
            ("unexpected value: " ++ show (txOutCoreValue txOut))
            (txOutCoreValue txOut == DbLovelace 1000000000)
        _ -> assertBool "expected a core txout" False
      _ -> assertBool ("expected one utxo, got " ++ show (length utxos)) False
