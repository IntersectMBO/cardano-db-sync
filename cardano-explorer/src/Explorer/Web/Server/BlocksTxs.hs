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
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Text (Text)
import           Data.Word (Word64)


import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.), (&&.),
                    from, limit, in_, offset, on, select, val, valList, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), TxId, unValue3)

import           Explorer.Web.ClientTypes (CAddress (..), CHash (..), CTxBrief (..), CTxHash (..),
                    mkCCoin)
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
    res <- select . from $ \  (blk `InnerJoin` tx) -> do
            on (blk ^. BlockId ==. tx ^. TxBlock)
            where_ (blk ^. BlockHash ==. val blkHash)
            limit limitNum
            offset offsetNum
            pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)
    case map unValue3 res of
      [] -> pure $ Left (Internal "No block found")
      -- TODO: This can still do with some improvement.
      xs -> Right <$> mapM (queryCTxBrief (map fst3 xs)) xs
  where
    fst3 :: (a, b, c) -> a
    fst3 (a, _, _) = a

queryCTxBrief :: MonadIO m => [TxId] -> (TxId, ByteString, UTCTime) -> ReaderT SqlBackend m CTxBrief
queryCTxBrief txOutIds (txId, txhash, utctime) = do
    inrows <- select . from $ \(txIn `InnerJoin` txOut) -> do
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                where_ (txIn ^. TxInTxInId ==. val txId)
                pure (txOut ^. TxOutAddress, txOut ^. TxOutValue)
    let inputs = map convert inrows
    outrows <- select . from $ \txOut -> do
                  where_ (txOut ^. TxOutTxId `in_` valList txOutIds)
                  pure (txOut ^. TxOutAddress, txOut ^. TxOutValue)
    let outputs = map convert outrows
        inSum = sum $ map snd inputs
        outSum = sum $ map snd outputs
    pure $ CTxBrief
            { ctbId = CTxHash . CHash $ bsBase16Encode txhash
            , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds utctime
            , ctbInputs = map (Just . fmap (mkCCoin . fromIntegral)) inputs
            , ctbOutputs = map (fmap (mkCCoin . fromIntegral)) outputs
            , ctbInputSum = mkCCoin $ fromIntegral inSum
            , ctbOutputSum = mkCCoin $ fromIntegral outSum
            , ctbFees = mkCCoin $ fromIntegral (if inSum == 0 then 0 else inSum - outSum)
            }
  where
    convert :: (Value Text, Value Word64) -> (CAddress, Word64)
    convert (Value addr, Value coin) = (CAddress addr, coin)
