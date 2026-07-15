{-# LANGUAGE OverloadedStrings #-}

module Test.IO.Cardano.Db.EpochBlockTxs (
  tests,
) where

import Cardano.Db
import Control.Monad (void)
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "EpochBlockTxs"
    [ testCase "epoch block tx count is looked up by block id" epochBlockTxCountUsesId
    ]

-- block_no and the surrogate block.id diverge on a real chain (an EBB has a null
-- block_no but a positive id). The epoch block-tx check must count transactions
-- by block.id, not by block_no, or it reads the wrong (or no) block.
epochBlockTxCountUsesId :: IO ()
epochBlockTxCountUsesId =
  runDbStandaloneSilent $ do
    deleteAllBlocks
    slid <- insertSlotLeader testSlotLeader
    let blk = (mkBlock 0 slid) {blockTxCount = 1}
    bid <- insertCheckUniqueBlock blk
    case mkTxs bid 1 of
      (tx : _) -> void $ insertTx tx
      [] -> error "mkTxs returned empty list"
    rows <- queryEpochBlockNumbers 0
    case rows of
      [(blockNo, expected)] -> do
        actual <- queryBlockTxCount (BlockId (fromIntegral blockNo))
        assertBool
          ("expected tx count " ++ show expected ++ " but got " ++ show actual)
          (actual == expected)
      _ -> assertBool ("expected one block row, got " ++ show (length rows)) False
