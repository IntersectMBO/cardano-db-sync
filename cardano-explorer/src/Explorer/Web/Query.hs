{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Query
  ( queryBlockHash
  , queryBlockSummary
  , queryNextBlock
  , queryOneBlock
  , queryTxSummary
  , queryTx
  , queryBlockTxs
  , queryUtxoSnapshot
  , queryBlockIdFromHeight
  , TxWithInputsOutputs(txwTx, txwInputs, txwOutputs, TxWithInputsOutputs)
  ) where

import           Database.Esqueleto ((^.), val, valList, (==.), where_, from, unValue, select, Value, InnerJoin(InnerJoin), sum_, on, (&&.), in_, subList_select, limit, offset, LeftOuterJoin(LeftOuterJoin), (>.), (||.), (<=.), isNothing)
import           Database.Persist.Sql       (SqlBackend, entityVal, entityKey, Entity)

import           Data.ByteString (ByteString)
import           Data.Word (Word64)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Explorer.DB (Block, BlockId, blockPrevious, listToMaybe
                            , EntityField(BlockHash, BlockPrevious, BlockId, TxHash, TxOutValue, TxOutAddress, TxInTxInId, TxOutIndex, TxInTxOutIndex, TxOutTxId, TxInTxOutId, TxBlock, TxFee, TxBlock, TxId, BlockSlotNo, BlockBlockNo)
                            , TxId
                            , entityPair, Tx, TxOut, Ada, LookupFail(DbLookupTxHash), maybeToEither, unValueSumAda, txBlock, querySelectCount, txOutTxId)

queryOneBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (BlockId, Block))
queryOneBlock blkHash = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockHash ==. val blkHash
    pure blk
  pure $ fmap entityPair (listToMaybe rows)

queryBlockById :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe Block)
queryBlockById blockid = do
  rows <- select . from $ \blk -> do
      where_ $ blk ^. BlockId ==. val blockid
      pure blk
  pure $ fmap entityVal (listToMaybe rows)

queryOutputsByTxId :: MonadIO m => TxId -> ReaderT SqlBackend m [TxOut]
queryOutputsByTxId txid = do
  rows <- select . from $ \txout -> do
    where_ $ txout ^. TxOutTxId ==. val txid
    pure txout
  pure $ map entityVal rows

queryBlockHash :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe ByteString)
queryBlockHash blkid = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockId ==. val blkid
    pure $ blk ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryNextBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe ByteString)
queryNextBlock blkid = do
  rows <- select . from $ \blk2 -> do
    where_ $ blk2 ^. BlockPrevious ==. val (Just blkid)
    pure $ blk2 ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryCountTxInBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m Word
queryCountTxInBlock blkid = querySelectCount $ \tx -> where_ (tx ^. TxBlock ==. val blkid)

queryBlockSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Block, Maybe ByteString, Maybe ByteString, Word, Ada, Ada))
queryBlockSummary blkHash = do
  maybeBlock <- queryOneBlock blkHash
  case maybeBlock of
    Just (blkid, blk) -> do
      tx_count <- queryCountTxInBlock blkid
      fees <- queryTotalFeeInBlock blkid
      total_out <- queryTotalOutputCoinInBlock blkid
      case blockPrevious blk of
        Just prevblkid -> do
          previousHash <- queryBlockHash prevblkid
          nextHash <- queryNextBlock blkid
          pure $ Just (blk, previousHash, nextHash, tx_count, fees, total_out)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

queryTxSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Tx, Block, [(Text, Word64)], [TxOut]))
queryTxSummary txhash = do
  eTx <- queryTx txhash
  case eTx of
    Right (txid, tx) -> do
      mBlock <- queryBlockById (txBlock tx)
      case mBlock of
        Just block -> do
          inputs <- queryGetInputOutputs txid
          outputs <- queryOutputsByTxId txid
          pure $ Just (tx, block, inputs, outputs)
        Nothing -> pure Nothing
    Left _ -> pure Nothing -- TODO

queryTotalFeeInBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m Ada
queryTotalFeeInBlock blockid = do
  res <- select . from $ \ tx -> do
          where_ (tx ^. TxBlock ==. val blockid )
          pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda $ listToMaybe res

queryTotalOutputCoinInBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m Ada
queryTotalOutputCoinInBlock blockid = do
  let
    subQuery = subList_select . from $ \ tx -> do
        where_ (tx ^. TxBlock ==. val blockid )
        pure $ tx ^. TxId
  res <- select . from $ \ tx_out -> do
          where_ $ tx_out ^. TxOutTxId `in_` subQuery
          pure $ sum_ (tx_out ^. TxOutValue)
  pure $ unValueSumAda $ listToMaybe res

queryUtxoSnapshot :: MonadIO m => BlockId -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoSnapshot blkid = do
    -- tx1 refers to the tx of the input spending this output (if it is ever spent)
    -- tx2 refers to the tx of the output
    outputs <- select . from $ \(txout `LeftOuterJoin` txin `LeftOuterJoin` tx1 `LeftOuterJoin` blk `LeftOuterJoin` tx2) -> do
      on $ txout ^. TxOutTxId ==. tx2 ^. TxId
      on $ tx1 ^. TxBlock ==. blk ^. BlockId
      on $ txin ^. TxInTxInId ==. tx1 ^. TxId
      on $ (txout ^. TxOutTxId ==. txin ^. TxInTxOutId) &&. (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
      where_ $ (txout ^. TxOutTxId `in_` txLessEqual) &&. ((isNothing $ blk ^. BlockBlockNo) ||. (blk ^. BlockId >. val blkid))
      pure (txout, tx2 ^. TxHash)
    pure $ map convertResult outputs
  where
    -- every block made before or at the snapshot time
    blockLessEqual = subList_select . from $ \blk -> do
      where_ $ blk ^. BlockId <=. val blkid
      pure $ blk ^. BlockId
    -- every tx made before or at the snapshot time
    txLessEqual = subList_select . from $ \tx -> do
      where_ $ tx ^. TxBlock `in_` blockLessEqual
      pure $ tx ^. TxId
    convertResult :: (Entity TxOut, Value ByteString) -> (TxOut, ByteString)
    convertResult (out, hash) = (entityVal out, unValue hash)

queryGetInputOutputs :: MonadIO m => TxId -> ReaderT SqlBackend m [(Text, Word64)]
queryGetInputOutputs txid = do
  rows <- select . from $ \(txin `InnerJoin` txout) -> do
    on ((txin ^. TxInTxOutId ==. txout ^. TxOutTxId) &&. (txin ^. TxInTxOutIndex ==. txout ^. TxOutIndex))
    where_ $ txin ^. TxInTxInId ==. val txid
    pure (txout ^. TxOutAddress, txout ^. TxOutValue)
  let
    unvalues :: (Value Text, Value Word64) -> (Text, Word64)
    unvalues (a,b) = (unValue a, unValue b)
  pure $ map unvalues rows

queryTx :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (TxId, Tx))
queryTx hash = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxHash ==. val hash)
            pure tx
  pure $ maybeToEither (DbLookupTxHash hash) entityPair (listToMaybe res)

data TxWithInputsOutputs = TxWithInputsOutputs
  { txwTx :: Tx
  , txwInputs :: [(Text, Word64)]
  , txwOutputs :: [TxOut]
  }

queryBlockTxs :: MonadIO m => ByteString -> Int64 -> Int64 -> ReaderT SqlBackend m ([ TxWithInputsOutputs ], Maybe Word64)
queryBlockTxs blkHash limitNum offsetNum = do
    maybeSlotNo <- select . from $ \blk -> do
      where_ (blk ^. BlockHash ==. val blkHash)
      pure $ blk ^. BlockSlotNo
    res <- select . from $ \tx -> do
      where_ (tx ^. TxBlock `in_` blockid)
      limit limitNum
      offset offsetNum
      pure tx
    let txids = map entityKey res
    --inputs <- select . from $ \txin -> do
    --  where_ (txin ^. TxInTxInId `in_` valList txids)
    --  pure txin
    outputs <- select . from $ \txout -> do
      where_ (txout ^. TxOutTxId `in_` valList txids)
      pure txout
    case (listToMaybe $ map unValue maybeSlotNo) of
      Just (Just slot) -> do
        txs <- mapM (txToTxWith outputs) res
        pure (txs, Just slot)
      _ -> do
        txs <- mapM (txToTxWith outputs) res
        pure (txs, Nothing)
  where
    blockid = subList_select . from $ \blk -> do
      where_ (blk ^. BlockHash ==. val blkHash)
      pure $ blk ^. BlockId
    txToTxWith :: MonadIO m => [Entity TxOut] -> Entity Tx -> ReaderT SqlBackend m TxWithInputsOutputs
    txToTxWith outputs tx = do
      -- TODO, use the commented out inputs query above?
      inputs <- queryGetInputOutputs (entityKey tx)
      pure $ TxWithInputsOutputs
        { txwTx = entityVal tx
        --, txwInputs = filter (\txin -> (txInTxInId txin) == (entityKey tx)) (map entityVal inputs)
        , txwInputs = inputs
        , txwOutputs = filter (\txout -> (txOutTxId txout) == (entityKey tx)) (map entityVal outputs)
        }

queryBlockIdFromHeight :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockIdFromHeight height = do
  res <- select . from $ \blk -> do
    where_ (blk ^. BlockBlockNo ==. val (Just height))
    pure $ blk ^. BlockId
  pure $ fmap unValue $ listToMaybe res
