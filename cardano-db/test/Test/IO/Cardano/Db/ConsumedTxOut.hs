{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.ConsumedTxOut (
  tests,
) where

import Cardano.Db
import Cardano.Db.Schema.Variants.TxOutCore (TxOutCore (..))
import Control.Monad (forM_, void)
import Data.Maybe (isNothing)
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "ConsumedTxOut"
    [ testCase "paging migrates every input when the count is a multiple of the page" (migrateAllInputs 2)
    , testCase "paging migrates every input when the last page is short" (migrateAllInputs 4)
    , testCase "re-running the migration is idempotent" migrateTwice
    ]

-- The migration pages through tx_in by id. A page boundary is where it can skip or
-- repeat a row, so each case runs with a bulk size far smaller than the row count: 6
-- inputs at a bulk size of 2 ends on an exact multiple (the loop must then issue one
-- more query and stop on the empty page), and at 4 it ends on a short page.
--
-- Gaps in tx_in.id are not covered here, as removing rows would need a raw delete and
-- so a hasql dependency for this suite; `WHERE id > $1` treats a gap exactly as it
-- treats consecutive ids.

-- | One block of @n@ transactions, each with a single tx_out at index 0, where
-- transaction @i+1@ spends the output of transaction @i@. Returns the (producer,
-- consumer) pairs the migration is expected to join up, and the one unspent output.
setupChain :: Word -> DbM ([(TxId, TxId)], TxId)
setupChain n = do
  deleteAllBlocks
  slid <- insertSlotLeader testSlotLeader
  bid <- insertCheckUniqueBlock (mkBlock 0 slid)
  txIds <- mapM insertTx (mkTxs bid n)
  forM_ txIds $ \txId -> void $ insertTxOut (mkTxOutCore bid txId)
  let spends = zip txIds (drop 1 txIds)
  forM_ spends $ \(producer, consumer) ->
    void $
      insertTxIn
        TxIn
          { txInTxInId = consumer
          , txInTxOutId = producer
          , txInTxOutIndex = 0
          , txInRedeemerId = Nothing
          }
  case reverse txIds of
    (unspent : _) -> pure (spends, unspent)
    [] -> error "setupChain: no transactions"

consumedBy :: TxId -> DbM (Maybe TxId)
consumedBy producer = do
  outs <- queryTxOutputs TxOutVariantCore (fromIntegral $ getTxId producer)
  case outs of
    [VCTxOutW txOut] -> pure (txOutCoreConsumedByTxId txOut)
    _ -> do
      assertBool ("expected exactly one output for tx " ++ show (getTxId producer)) False
      pure Nothing

-- | Every spent output carries its consumer, and the unspent one is left alone.
assertMigrated :: [(TxId, TxId)] -> TxId -> DbM ()
assertMigrated spends unspent = do
  forM_ spends $ \(producer, consumer) -> do
    actual <- consumedBy producer
    assertBool
      ( "tx_out of tx "
          ++ show (getTxId producer)
          ++ ": consumed_by_tx_id = "
          ++ show (fmap getTxId actual)
          ++ ", expected "
          ++ show (Just (getTxId consumer))
      )
      (actual == Just consumer)
  stillUnspent <- consumedBy unspent
  assertBool
    ("unspent tx_out of tx " ++ show (getTxId unspent) ++ " was set to " ++ show (fmap getTxId stillUnspent))
    (isNothing stillUnspent)

migrateAllInputs :: Int -> IO ()
migrateAllInputs bulkSize =
  runDbStandaloneSilent $ do
    (spends, unspent) <- setupChain 7
    migrateTxOutDbTool bulkSize TxOutVariantCore
    assertMigrated spends unspent

-- The interrupted-migration recovery path re-runs the whole migration, so running it
-- over already migrated rows has to be a no-op rather than an error.
migrateTwice :: IO ()
migrateTwice =
  runDbStandaloneSilent $ do
    (spends, unspent) <- setupChain 7
    migrateTxOutDbTool 2 TxOutVariantCore
    migrateTxOutDbTool 4 TxOutVariantCore
    assertMigrated spends unspent
