{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Rollback (
  tests,
) where

import Cardano.Db
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Word (Word64)
import Database.Persist.Sql (SqlBackend)

-- import           Test.Tasty.HUnit (testCase)

import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Rollback"
    []

-- testCase "Can rollback" rollbackTest TODO: fix

_rollbackTest :: IO ()
_rollbackTest =
  runDbNoLoggingEnv $ do
    -- Delete the blocks if they exist.
    deleteAllBlocks
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
    latestSlotNo <- queryLatestSlotNo
    Just pSlotNo <- queryWalkChain 5 latestSlotNo
    void $ deleteBlocksSlotNoNoTrace (SlotNo pSlotNo)
    -- Assert the expected final state.
    afterBlocks <- queryBlockCount
    assertBool ("Block count after rollback is " ++ show afterBlocks ++ " but should be 10") $ afterBlocks == 4
    afterTxCount <- queryTxCount
    assertBool ("Tx count after rollback is " ++ show afterTxCount ++ " but should be 10") $ afterTxCount == 1
    afterTxOutCount <- queryTxOutCount
    assertBool ("TxOut count after rollback is " ++ show afterTxOutCount ++ " but should be 1.") $ afterTxOutCount == 1
    afterTxInCount <- queryTxInCount
    assertBool ("TxIn count after rollback is " ++ show afterTxInCount ++ " but should be 0.") $ afterTxInCount == 0

-- -----------------------------------------------------------------------------

queryWalkChain :: (MonadBaseControl IO m, MonadIO m) => Int -> Word64 -> ReaderT SqlBackend m (Maybe Word64)
queryWalkChain count blkNo
  | count <= 0 = pure $ Just blkNo
  | otherwise = do
      mpBlkNo <- queryPreviousSlotNo blkNo
      case mpBlkNo of
        Nothing -> pure Nothing
        Just pBlkNo -> queryWalkChain (count - 1) pBlkNo

createAndInsertBlocks :: (MonadBaseControl IO m, MonadIO m) => Word64 -> ReaderT SqlBackend m ()
createAndInsertBlocks blockCount =
  void $ loop (0, Nothing, Nothing)
  where
    loop ::
      (MonadBaseControl IO m, MonadIO m) =>
      (Word64, Maybe BlockId, Maybe TxId) ->
      ReaderT SqlBackend m (Word64, Maybe BlockId, Maybe TxId)
    loop (indx, mPrevId, mOutId) =
      if indx < blockCount
        then loop =<< createAndInsert (indx, mPrevId, mOutId)
        else pure (0, Nothing, Nothing)

    createAndInsert ::
      (MonadBaseControl IO m, MonadIO m) =>
      (Word64, Maybe BlockId, Maybe TxId) ->
      ReaderT SqlBackend m (Word64, Maybe BlockId, Maybe TxId)
    createAndInsert (indx, mPrevId, mTxOutId) = do
      slid <- insertSlotLeader testSlotLeader
      let newBlock =
            Block
              { blockHash = mkBlockHash indx
              , blockEpochNo = Just 0
              , blockSlotNo = Just indx
              , blockEpochSlotNo = Just indx
              , blockBlockNo = Just indx
              , blockPreviousId = mPrevId
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

      blkId <- insertBlock newBlock
      newMTxOutId <-
        if indx /= 0
          then pure mTxOutId
          else do
            txId <- insertTx $ Tx (mkTxHash blkId 0) blkId 0 (DbLovelace 0) (DbLovelace 0) (Just 0) 12 Nothing Nothing True 0
            void $ insertTxOut (mkTxOut blkId txId)
            pure $ Just txId
      case (indx, mTxOutId) of
        (8, Just txOutId) -> do
          -- Insert Txs here to test that they are cascade deleted when the blocks
          -- they are associcated with are deleted.

          txId <- head <$> mapM insertTx (mkTxs blkId 8)
          void $ insertTxIn (TxIn txId txOutId 0 Nothing)
          void $ insertTxOut (mkTxOut blkId txId)
        _ -> pure ()
      pure (indx + 1, Just blkId, newMTxOutId)
