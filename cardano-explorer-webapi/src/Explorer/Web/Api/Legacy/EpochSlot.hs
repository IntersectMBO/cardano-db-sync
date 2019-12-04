{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.EpochSlot
  ( epochSlot
  ) where

import           Cardano.Chain.Slotting (EpochNumber (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString (ByteString)
import           Data.Fixed (Fixed (..), Uni)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Word (Word16, Word64)

import           Database.Esqueleto (Entity (..), InnerJoin (..), Value (..), (^.), (==.),
                    countRows, from, just, on, select, sum_, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (BlockId, Block (..), EntityField (..))
import           Explorer.Web.ClientTypes (CBlockEntry (..), CHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy.Util

import           Servant (Handler)


-- Example queries:
--
--  /api/epochs/0/0
--  /api/epochs/10/84
--  /api/epochs/27/1672

-- Why does this return a list? There can only be a single block at a give epoch/slot pair.
epochSlot
    :: SqlBackend -> EpochNumber -> Word16
    -> Handler (Either ExplorerError [CBlockEntry])
epochSlot backend (EpochNumber epochNo) slotInEpoch =
  if fromIntegral slotInEpoch >= 10 * k
    then pure $ Left (Internal "Invalid slot number")
    else
      runQuery backend $ do
        xs <- queryBlockBySlotNo $ 10 * k * epochNo + fromIntegral slotInEpoch
        case xs of
          [] -> pure $ Left (Internal "Not found")
          _ -> Right <$> mapM queryBlockTx xs

-- This query is a pain in the neck. Was not to figure out how to do it
-- in less than three separate select statements.
queryBlockBySlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(BlockId, CBlockEntry)]
queryBlockBySlotNo flatSlotNo = do
    rows <- select . from $ \ (blk `InnerJoin` sl) -> do
              on (blk ^. BlockSlotLeader ==. sl ^. SlotLeaderId)
              where_ (blk ^. BlockSlotNo ==. just (val flatSlotNo))
              pure (blk, sl ^. SlotLeaderHash)
    pure $ map convert rows
  where
    convert :: (Entity Block, Value ByteString) -> (BlockId, CBlockEntry)
    convert (Entity blkId block, Value sl) =
      (blkId, CBlockEntry
                { cbeEpoch = fromMaybe 0 $ blockEpochNo block
                , cbeSlot = maybe 0 unflattenSlotNo $ blockSlotNo block
                , cbeBlkHeight = maybe 0 fromIntegral $ blockBlockNo block
                , cbeBlkHash = CHash $ bsBase16Encode (blockHash block)
                , cbeTimeIssued = Just $ blockPosixTime block
                , cbeTxNum = 0
                , cbeTotalSent = mkCCoin 0
                , cbeSize = blockSize block
                , cbeBlockLead = Just $ bsBase16Encode sl
                , cbeFees = mkCCoin 0
                })

queryBlockTx :: MonadIO m => (BlockId, CBlockEntry) -> ReaderT SqlBackend m CBlockEntry
queryBlockTx (blkId, entry) = do
    res <- select . from $ \ (blk `InnerJoin` tx) -> do
              on (tx ^. TxBlock ==. blk ^. BlockId)
              where_ (blk ^. BlockId ==. val blkId)
              pure (countRows, sum_ (tx ^. TxFee))
    maybe (pure entry) queryBlockTxOutValue (listToMaybe res)
  where
    queryBlockTxOutValue :: MonadIO m => (Value Word, Value (Maybe Uni)) -> ReaderT SqlBackend m CBlockEntry
    queryBlockTxOutValue (Value txCount, mFee) = do
      res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                on (tx ^. TxBlock ==. blk ^. BlockId)
                where_ (blk ^. BlockId ==. val blkId)
                pure (sum_ (txOut ^. TxOutValue))
      pure $ maybe entry (convert txCount (unSumValue mFee)) (listToMaybe res)

    convert :: Word -> Integer -> Value (Maybe Uni) -> CBlockEntry
    convert txCount fee mValue =
      entry
        { cbeTxNum = txCount
        , cbeTotalSent = mkCCoin $ unSumValue mValue
        , cbeFees = mkCCoin fee
        }

unSumValue :: Value (Maybe Uni) -> Integer
unSumValue mvi =
  case unValue mvi of
    Just (MkFixed x) -> x
    _ -> 0
