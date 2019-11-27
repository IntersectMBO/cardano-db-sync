{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Query
  ( queryBlockHash
  , queryBlockSummary
  , queryChainTip
  , queryNextBlock
  , queryUtxoSnapshot
  , queryBlockIdFromHeight
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString (ByteString)
import           Data.Maybe (listToMaybe)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), LeftOuterJoin (..), Value (..), ValueList,
                    SqlExpr, (^.), (==.), (&&.), (>.), (||.), (<=.),
                    desc, from, in_, isNothing, limit, on, orderBy, select, subList_select, sum_,
                    unValue, val, where_)
import           Database.Persist.Sql (Entity (..), SqlBackend)

import           Explorer.DB (Ada, Block, BlockId, EntityField (..), TxId, TxOut,
                    blockPrevious, blockSlotNo, isJust, querySlotPosixTime,
                    querySelectCount, unValueSumAda)


import           Explorer.Web.ClientTypes (CChainTip (..), CHash (..))
import           Explorer.Web.Api.Legacy.Util (bsBase16Encode)

queryBlockHash :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe ByteString)
queryBlockHash blkid = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockId ==. val blkid
    pure $ blk ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryChainTip :: MonadIO m => ReaderT SqlBackend m CChainTip
queryChainTip = do
    rows <- select . from $ \ blk -> do
              where_ (isJust (blk ^. BlockBlockNo))
              orderBy [desc (blk ^. BlockBlockNo)]
              limit 1
              pure (blk ^. BlockBlockNo, blk ^. BlockSlotNo, blk ^. BlockHash)
    pure $ maybe defTip convert (listToMaybe rows)
  where
    convert :: (Value (Maybe Word64), Value (Maybe Word64), Value ByteString) -> CChainTip
    convert (Value mBlkNo, Value mSlotNo, Value blkHash) =
      CChainTip
        { ctBlockNo = maybe 0 fromIntegral mBlkNo
        , ctSlotNo = maybe 0 fromIntegral mSlotNo
        , ctBlockHash = CHash (bsBase16Encode blkHash)
        }

    defTip :: CChainTip
    defTip = CChainTip 0 0 (CHash "unknown")

queryNextBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe ByteString)
queryNextBlock blkid = do
  rows <- select . from $ \blk2 -> do
    where_ $ blk2 ^. BlockPrevious ==. val (Just blkid)
    pure $ blk2 ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryBlockTxInCount :: MonadIO m => BlockId -> ReaderT SqlBackend m Word
queryBlockTxInCount blkid =
  querySelectCount $ \tx -> where_ (tx ^. TxBlock ==. val blkid)

queryBlockByHash :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (BlockId, Block, ByteString))
queryBlockByHash blkHash = do
    rows <- select . from $ \ (blk `InnerJoin` sl)-> do
              on (blk ^. BlockSlotLeader ==. sl ^. SlotLeaderId)
              where_ $ blk ^. BlockHash ==. val blkHash
              pure (blk, sl ^. SlotLeaderHash)
    pure $ fmap convert (listToMaybe rows)
  where
    convert :: (Entity Block, Value ByteString) -> (BlockId, Block, ByteString)
    convert (eb, vsh) = (entityKey eb, entityVal eb, unValue vsh)

queryBlockSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Block, ByteString, Maybe ByteString, Word, Ada, Ada, ByteString, Maybe POSIXTime))
queryBlockSummary blkHash = do
  maybeBlock <- queryBlockByHash blkHash
  case maybeBlock of
    Just (blkid, blk, slh) -> do
      txCount <- queryBlockTxInCount blkid
      fees <- queryTotalFeeInBlock blkid
      totalOut <- queryTotalOutputCoinInBlock blkid
      timestamp <- maybe (pure Nothing) querySlotTimeSeconds $ blockSlotNo blk
      case blockPrevious blk of
        Just prevblkid -> do
          mPrevHash <- queryBlockHash prevblkid
          nextHash <- queryNextBlock blkid
          case mPrevHash of
            Nothing -> pure Nothing
            Just previousHash -> pure $ Just (blk, previousHash, nextHash, txCount, fees, totalOut, slh, timestamp)
        Nothing -> pure Nothing
    Nothing -> pure Nothing

querySlotTimeSeconds :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe POSIXTime)
querySlotTimeSeconds slotNo =
  either (const Nothing) Just <$> querySlotPosixTime slotNo

queryTotalFeeInBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m Ada
queryTotalFeeInBlock blockid = do
  res <- select . from $ \ tx -> do
          where_ (tx ^. TxBlock ==. val blockid)
          pure $ sum_ (tx ^. TxFee)
  pure $ unValueSumAda $ listToMaybe res

queryTotalOutputCoinInBlock :: MonadIO m => BlockId -> ReaderT SqlBackend m Ada
queryTotalOutputCoinInBlock blockid = do
    res <- select . from $ \ txOut -> do
            where_ $ txOut ^. TxOutTxId `in_` subQuery
            pure $ sum_ (txOut ^. TxOutValue)
    pure $ unValueSumAda (listToMaybe res)
  where
    subQuery = subList_select . from $ \ tx -> do
        where_ (tx ^. TxBlock ==. val blockid)
        pure $ tx ^. TxId

queryUtxoSnapshot :: MonadIO m => BlockId -> ReaderT SqlBackend m [(TxOut, ByteString)]
queryUtxoSnapshot blkid = do
    -- tx1 refers to the tx of the input spending this output (if it is ever spent)
    -- tx2 refers to the tx of the output
    outputs <- select . from $ \(txout `LeftOuterJoin` txin `LeftOuterJoin` tx1 `LeftOuterJoin` blk `LeftOuterJoin` tx2) -> do
      on $ txout ^. TxOutTxId ==. tx2 ^. TxId
      on $ tx1 ^. TxBlock ==. blk ^. BlockId
      on $ txin ^. TxInTxInId ==. tx1 ^. TxId
      on $ (txout ^. TxOutTxId ==. txin ^. TxInTxOutId) &&. (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
      where_ $ (txout ^. TxOutTxId `in_` txLessEqual) &&. (isNothing (blk ^. BlockBlockNo) ||. (blk ^. BlockId >. val blkid))
      pure (txout, tx2 ^. TxHash)
    pure $ map convertResult outputs
  where
    -- every block made before or at the snapshot time
    blockLessEqual :: SqlExpr (ValueList BlockId)
    blockLessEqual =
      subList_select . from $ \blk -> do
        where_ $ blk ^. BlockId <=. val blkid
        pure $ blk ^. BlockId
    -- every tx made before or at the snapshot time
    txLessEqual :: SqlExpr (ValueList TxId)
    txLessEqual =
      subList_select . from $ \tx -> do
        where_ $ tx ^. TxBlock `in_` blockLessEqual
        pure $ tx ^. TxId
    convertResult :: (Entity TxOut, Value ByteString) -> (TxOut, ByteString)
    convertResult (out, hash) = (entityVal out, unValue hash)

queryBlockIdFromHeight :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockIdFromHeight height = do
  res <- select . from $ \blk -> do
    where_ (blk ^. BlockBlockNo ==. val (Just height))
    pure $ blk ^. BlockId
  pure $ fmap unValue $ listToMaybe res
