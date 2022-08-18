{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Rollback
  ( tests
  ) where

import           Cardano.Db
import           Cardano.Slotting.Block (BlockNo (..))

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Int (Int64)
import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Test.IO.Cardano.Db.Util


tests :: TestTree
tests =
  testGroup "Rollback"
    [ testCase "Can rollback" rollbackTest
    ]


rollbackTest :: IO ()
rollbackTest =
  runDbNoLoggingEnv $ do
    -- Delete the blocks if they exist.
    deleteEverything
    setupBlockCount <- queryBlockCount
    assertBool ("Block on setup is " ++ show setupBlockCount ++ " but should be 0.") $ setupBlockCount == 0
    -- Set up state before rollback and assert expected counts.
    createAndInsertBlocks 10
    beforeBlocks <- queryBlockCount
    assertBool ("Block count before rollback is " ++ show beforeBlocks ++ " but should be 10.") $ beforeBlocks == 10
    beforeTxCount <- queryTxCount
    assertBool ("Tx count before rollback is " ++ show beforeTxCount ++ " but should be 9.") $ beforeTxCount == 9
    beforeTxOutCount <- queryTxOutCount
    assertBool ("TxOut count before rollback is " ++ show beforeTxOutCount ++ " but should be 2.") $ beforeTxOutCount == 2
    beforeTxInCount <- queryTxInCount
    assertBool ("TxIn count before rollback is " ++ show beforeTxInCount ++ " but should be 1.") $ beforeTxInCount == 1
    -- Rollback a set of blocks.
    latestBlockNo <- queryLatestBlockNo
    let pBlockNo = queryWalkChain 5 (BlockNo $ latestBlockNo - 1)
    void $ deleteAfterBlockNo (BlockNo pBlockNo)
    -- Assert the expected final state.
    afterBlocks <- queryBlockCount
    assertBool ("Block count after rollback is " ++ show afterBlocks ++ " but should be 4") $ afterBlocks == 4
    afterTxCount <- queryTxCount
    assertBool ("Tx count after rollback is " ++ show afterTxCount ++ " but should be 1") $ afterTxCount == 1
    afterTxOutCount <- queryTxOutCount
    assertBool ("TxOut count after rollback is " ++ show afterTxOutCount ++ " but should be 1.") $ afterTxOutCount == 1
    afterTxInCount <- queryTxInCount
    assertBool ("TxIn count after rollback is " ++ show afterTxInCount ++ " but should be 0.") $ afterTxInCount == 0

-- -----------------------------------------------------------------------------

queryWalkChain :: Word64 -> BlockNo -> Word64
queryWalkChain count (BlockNo blkNo) =
  if blkNo > count then blkNo - count else blkNo

createAndInsertBlocks :: (MonadBaseControl IO m, MonadIO m) => Int64 -> ReaderT SqlBackend m ()
createAndInsertBlocks blockCount =
    void $ loop (0, Nothing)
  where
    loop
        :: (MonadBaseControl IO m, MonadIO m)
        => (Int64, Maybe TxId)
        -> ReaderT SqlBackend m (Word64, Maybe TxId)
    loop (indx, mOutId) =
      if indx < blockCount
        then loop =<< createAndInsert (indx, mOutId)
        else pure (0, Nothing)

    createAndInsert
        :: (MonadBaseControl IO m, MonadIO m)
        => (Int64, Maybe TxId)
        -> ReaderT SqlBackend m (Int64, Maybe TxId)
    createAndInsert (blkNo, mTxOutId) = do
        slid <- insertSlotLeader testSlotLeader
        let newBlock = Block
                        { blockHash = mkBlockHash blkNo
                        , blockEpochNo = Just 0
                        , blockSlotNo = Just (fromIntegral blkNo)
                        , blockEpochSlotNo = Just (fromIntegral blkNo)
                        , blockBlockNo = fromIntegral blkNo
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

        void $ insertBlock newBlock
        newMTxOutId <-
                if blkNo /= 0
                  then pure mTxOutId
                  else do
                    txId <- insertTx $ Tx (mkTxHash blkNo 0) blkNo 0 (DbLovelace 0) (DbLovelace 0) 0 12 Nothing Nothing True 0
                    void $ insertTxOut (mkTxOut blkNo txId)
                    pure $ Just txId
        case (blkNo, mTxOutId) of
            (8, Just txOutId) -> do
                -- Insert Txs here to test that they are cascade deleted when the blocks
                -- they are associcated with are deleted.

                txId <- head <$> mapM insertTx (mkTxs blkNo 8)
                void $ insertTxIn (TxIn txId txOutId 0 Nothing blkNo)
                void $ insertTxOut (mkTxOut blkNo txId)
            _ -> pure ()
        pure (blkNo + 1, newMTxOutId)

