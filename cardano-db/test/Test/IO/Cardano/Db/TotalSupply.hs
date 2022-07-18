{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.TotalSupply
  ( tests
  ) where

import           Cardano.Db

import           Control.Monad (void)

import qualified Data.ByteString.Char8 as BS
import           Data.Int (Int64)
import qualified Data.Text as Text

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Test.IO.Cardano.Db.Util

tests :: TestTree
tests =
  testGroup "TotalSupply"
    [ testCase "Initial supply correct" initialSupplyTest
    ]


initialSupplyTest :: IO ()
initialSupplyTest =
  runDbNoLoggingEnv $ do
    -- Delete the blocks if they exist.
    deleteEverything

    -- Set up initial supply.
    slid <- insertSlotLeader testSlotLeader
    let blkNo0 = 0 :: Int64
    void $ insertBlock (mkBlock blkNo0 slid)
    (tx0Ids :: [TxId]) <- mapM insertTx $ mkTxs blkNo0 4
    mapM_ (insertTxOut . mkTxOut blkNo0) tx0Ids
    count <- queryBlockCount
    assertBool ("Block count should be 1, got " ++ show count) (count == 1)
    supply0 <- queryTotalSupply
    assertBool "Total supply should not be > 0" (supply0 > Ada 0)

    -- Spend from the Utxo set.
    let blkNo1 = 1 :: Int64
    void $ insertBlock (mkBlock blkNo1 slid)
    tx1Id <- insertTx $
                Tx
                  { txHash = mkTxHash blkNo1 1
                  , txBlockNo = 1
                  , txBlockIndex = 0
                  , txOutSum = DbLovelace 500000000
                  , txFee = DbLovelace 100
                  , txDeposit = 0
                  , txSize = 123
                  , txInvalidHereafter = Nothing
                  , txInvalidBefore = Nothing
                  , txValidContract = True
                  , txScriptSize = 0
                  }
    void $ insertTxIn (TxIn tx1Id (head tx0Ids) 0 Nothing blkNo1)
    let addr = mkAddressHash blkNo1 tx1Id
    void $ insertTxOut $ TxOut tx1Id 0 (Text.pack addr) (BS.pack addr) False Nothing Nothing (DbLovelace 500000000) Nothing Nothing Nothing blkNo1
    supply1 <- queryTotalSupply
    assertBool ("Total supply should be < " ++ show supply0) (supply1 < supply0)
