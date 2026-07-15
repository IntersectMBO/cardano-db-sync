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
    ]

-- An enterprise output has a null stake_address_id. The query selects txout.*
-- (including the id column), so the decoder must consume id or every field
-- shifts and the value decoder hits the nullable stake_address_id column.
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
