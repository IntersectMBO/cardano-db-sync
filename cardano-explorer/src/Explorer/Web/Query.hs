{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Query
  ( queryBlockHash
  , queryBlockSummary
  , queryNextBlock
  , queryOneBlock
  , queryTxSummary
  , queryTx
  ) where

import           Database.Esqueleto ((^.), val, (==.), where_, from, unValue, select, Value, InnerJoin(InnerJoin), sum_, on, (&&.), in_, subList_select, countRows)
import           Database.Persist.Sql       (SqlBackend, entityVal)

import           Data.ByteString (ByteString)
import           Data.Word (Word64)
import           Data.Text (Text)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Explorer.DB (Block, BlockId, Key, blockPrevious, listToMaybe, EntityField(BlockHash, BlockPrevious, BlockId, TxHash, TxOutValue, TxOutAddress, TxInTxInId, TxOutIndex, TxInTxOutIndex, TxOutTxId, TxInTxOutId, TxBlock, TxFee, TxBlock, TxId), entityPair, Tx, TxOut, Ada, LookupFail(DbLookupTxHash), maybeToEither, unValueSumAda, txBlock)

queryOneBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Key Block, Block))
queryOneBlock blkHash = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockHash ==. val blkHash
    pure blk
  pure $ fmap entityPair (listToMaybe rows)

queryBlockById :: MonadIO m => Key Block -> ReaderT SqlBackend m (Maybe Block)
queryBlockById blockid = do
  rows <- select . from $ \blk -> do
      where_ $ blk ^. BlockId ==. val blockid
      pure blk
  pure $ fmap entityVal (listToMaybe rows)

queryOutputsByTxId :: MonadIO m => Key Tx -> ReaderT SqlBackend m [TxOut]
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

queryNextBlock :: MonadIO m => Key Block -> ReaderT SqlBackend m (Maybe ByteString)
queryNextBlock blkid = do
  rows <- select . from $ \blk2 -> do
    where_ $ blk2 ^. BlockPrevious ==. val (Just blkid)
    pure $ blk2 ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryBlockSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Block, Maybe ByteString, Maybe ByteString, Word, Ada, Ada))
queryBlockSummary blkHash = do
  maybeBlock <- queryOneBlock blkHash
  case maybeBlock of
    Just (blkid, blk) -> do
      tx_count <- queryTxCountInBlock blkid
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

queryTxCountInBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m Word
queryTxCountInBlock blockid = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxBlock ==. val blockid )
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

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

queryGetInputOutputs :: MonadIO m => Key Tx -> ReaderT SqlBackend m [(Text, Word64)]
queryGetInputOutputs txid = do
  rows <- select . from $ \(txin `InnerJoin` txout) -> do
    on ((txin ^. TxInTxOutId ==. txout ^. TxOutTxId) &&. (txin ^. TxInTxOutIndex ==. txout ^. TxOutIndex))
    where_ $ txin ^. TxInTxInId ==. val txid
    pure (txout ^. TxOutAddress, txout ^. TxOutValue)
  let
    unvalues :: (Value Text, Value Word64) -> (Text, Word64)
    unvalues (a,b) = (unValue a, unValue b)
  pure $ map unvalues rows

queryTx :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (Key Tx, Tx))
queryTx hash = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxHash ==. val hash)
            pure tx
  pure $ maybeToEither (DbLookupTxHash hash) entityPair (listToMaybe res)
