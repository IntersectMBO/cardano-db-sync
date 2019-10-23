{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Explorer.Web.Server.BlocksTxs
  ( blocksTxs
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)

import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..),
                    (^.), (==.), (&&.), from, in_, limit, offset, on, select,
                    unValue, val, valList, where_)
import           Database.Persist.Sql (Entity (..), SqlBackend)

import           Explorer.DB (BlockId, EntityField (..), TxId, TxOut (..),unValue2)

import           Explorer.Web.ClientTypes (CAddress (..), CCoin, CHash (..),
                    CTxBrief (..), CTxHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Server.Util (bsBase16Encode, runQuery, textBase16Decode)

import           Servant (Handler)


-- Example queries:
--
--  /api/blocks/txs/not-valid
--  /api/blocks/txs/d30117e2e488cb3f496a47305eee3c8ea01e83e9e91e2719f1677de07f902e9a
--  /api/blocks/txs/e22e8771de60d44820c72b10114a7aee7cf98e3b188936e8601f9a12637edf63


blocksTxs
    :: SqlBackend -> CHash
    -> Maybe Int64
    -> Maybe Int64
    -> Handler (Either ExplorerError [CTxBrief])
blocksTxs backend (CHash blkHashTxt) mLimit mOffset =
    case textBase16Decode blkHashTxt of
      Left err -> pure $ Left err
      Right blkHash -> runQuery backend $ queryBlockByHash blkHash pageSize page
  where
    pageSize = fromMaybe 10 mLimit
    page = fromMaybe 0 mOffset


queryBlockByHash :: MonadIO m => ByteString -> Int64 -> Int64 -> ReaderT SqlBackend m (Either ExplorerError [CTxBrief])
queryBlockByHash blkHash limitNum offsetNum  = do
    res <- select . from $ \blk -> do
            where_ (blk ^. BlockHash ==. val blkHash)
            pure (blk ^. BlockId, blk ^. BlockTime)
    case map unValue2 res of
      [] -> pure $ Left (Internal "No block found")
      ((blkid, utctime):_) -> Right <$> queryBlockTxs blkid utctime limitNum offsetNum

queryBlockTxs :: MonadIO m => BlockId -> UTCTime -> Int64 -> Int64 -> ReaderT SqlBackend m [CTxBrief]
queryBlockTxs blkId utctime limitNum offsetNum = do
    res <- select . from $ \ (blk `InnerJoin` tx) -> do
            on (blk ^. BlockId ==. tx ^. TxBlock)
            where_ (blk ^. BlockId ==. val blkId)
            limit limitNum
            offset offsetNum
            pure (tx ^. TxId, tx ^. TxHash)
    let txids = map (unValue. fst) res
    outputs <- select . from $ \txout -> do
                where_ (txout ^. TxOutTxId `in_` valList txids)
                pure txout
    mapM (queryCTxBrief utctime (map entityVal outputs) . unValue2) res

queryCTxBrief :: MonadIO m => UTCTime -> [TxOut] -> (TxId, ByteString) -> ReaderT SqlBackend m CTxBrief
queryCTxBrief utctime outputs (txid, txhash) = do
  inputs <- queryTxInputs txid
  pure $ CTxBrief
          { ctbId = CTxHash . CHash $ bsBase16Encode txhash
          , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds utctime
          , ctbInputs = map convertInput inputs
          , ctbOutputs = map convertTxOut outputs
          , ctbInputSum = mkCCoin . sum $ map (fromIntegral . snd) inputs
          , ctbOutputSum = mkCCoin . sum $ map (fromIntegral . txOutValue) outputs
          }


queryTxInputs :: MonadIO m => TxId -> ReaderT SqlBackend m [(Text, Word64)]
queryTxInputs txid = do
  rows <- select . from $ \(txin `InnerJoin` txout) -> do
    on (txin ^. TxInTxOutId ==. txout ^. TxOutTxId &&. txin ^. TxInTxOutIndex ==. txout ^. TxOutIndex)
    where_ $ txin ^. TxInTxInId ==. val txid
    pure (txout ^. TxOutAddress, txout ^. TxOutValue)
  pure $ map unValue2 rows

convertTxOut :: TxOut -> (CAddress, CCoin)
convertTxOut txo = (CAddress $ txOutAddress txo, mkCCoin $ fromIntegral (txOutValue txo))

convertInput :: (Text, Word64) -> Maybe (CAddress, CCoin)
convertInput (addr, coin) = Just (CAddress $ addr, mkCCoin $ fromIntegral coin)
