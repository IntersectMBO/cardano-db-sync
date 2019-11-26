{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Explorer.Web.Api.Legacy.BlocksTxs
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
import           Data.Word (Word16, Word64)


import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.), (&&.),
                    from, limit, in_, offset, on, select, val, valList, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), TxId, unValue3)

import           Explorer.Web.Api.Legacy.Util (bsBase16Encode, genesisDistributionTxHash,
                    runQuery, textBase16Decode)
import           Explorer.Web.ClientTypes (CAddress (..), CCoin (..), CHash (..),
                    CTxAddressBrief (..), CTxBrief (..), CTxHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))

import           Servant (Handler)

-- Example queries:
--
--  /api/blocks/txs/not-valid
--  /api/blocks/txs/d30117e2e488cb3f496a47305eee3c8ea01e83e9e91e2719f1677de07f902e9a
--  /api/blocks/txs/c25f5468195e95dc6e7acbc0f0da794113b0edbfe1f998e10c85e2a1ec679e83
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
    inrows <- select . from $ \(tx `InnerJoin` txIn `InnerJoin` txOut `InnerJoin` txInTx) -> do
                on (txInTx ^. TxId ==. txIn ^. TxInTxOutId)
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                on (tx ^. TxId ==. txIn ^. TxInTxOutId)
                where_ (txIn ^. TxInTxInId ==. val txId)
                -- A Tx with a size of zero is a transaction to create a Geneisis Distribution output.
                pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, txInTx ^. TxHash, txOut ^. TxOutIndex, tx ^. TxSize ==. val 0)
    let inputs = map convertIn inrows
    outrows <- select . from $ \(tx `InnerJoin` txOut) -> do
                  on (tx ^. TxId ==. txOut ^. TxOutTxId)
                  where_ (txOut ^. TxOutTxId `in_` valList txOutIds)
                  pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, tx ^. TxHash, txOut ^. TxOutIndex)
    let outputs = map convertOut outrows
        inSum = sum $ map (unCCoin . ctaAmount) inputs
        outSum = sum $ map (unCCoin . ctaAmount) outputs
    pure $ CTxBrief
            { ctbId = CTxHash $ CHash (bsBase16Encode txhash)
            , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds utctime
            , ctbInputs = inputs
            , ctbOutputs = outputs
            , ctbInputSum = mkCCoin inSum
            , ctbOutputSum = mkCCoin outSum
            -- Only redeem address have zero input and zero fees.
            , ctbFees = mkCCoin $ if inSum == 0 then 0 else inSum - outSum
            }
  where
    convertIn :: (Value Text, Value Word64, Value ByteString, Value Word16, Value Bool) -> CTxAddressBrief
    convertIn (Value addr, Value coin, Value txh, Value index, Value isGenesisTx) =
      if isGenesisTx
        then
          CTxAddressBrief
            { ctaAddress = CAddress addr
            , ctaAmount = mkCCoin $ fromIntegral coin
            , ctaTxHash = genesisDistributionTxHash
            , ctaTxIndex = 0
            }
        else
          CTxAddressBrief
            { ctaAddress = CAddress addr
            , ctaAmount = mkCCoin $ fromIntegral coin
            , ctaTxHash = CTxHash $ CHash (bsBase16Encode txh)
            , ctaTxIndex = fromIntegral index
            }

    convertOut :: (Value Text, Value Word64, Value ByteString, Value Word16) -> CTxAddressBrief
    convertOut (Value addr, Value coin, Value txh, Value index) =
      CTxAddressBrief
        { ctaAddress = CAddress addr
        , ctaAmount = mkCCoin $ fromIntegral coin
        , ctaTxHash = CTxHash $ CHash (bsBase16Encode txh)
        , ctaTxIndex = fromIntegral index
        }
