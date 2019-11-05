{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Server.AddressSummary
  ( addressSummary
  ) where

import           Cardano.Chain.Common (isRedeemAddress)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (hoistEither, runExceptT, newExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..),
                    (^.), (==.), (&&.), distinct, from, on, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), TxId, unValue3)

import           Explorer.Web.ClientTypes (CAddress (..), CAddressSummary (..), CAddressType (..),
                    CCoin (..), CHash (..), CTxBrief (..), CTxHash (..), mkCCoin, sumCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Server.Util (bsBase16Encode, decodeTextAddress, runQuery)

import           Servant (Handler)

-- Example redeem addresses:
--    /api/addresses/summary/Ae2tdPwUPEZAAvxJ9nQ1nx88X9Jq6dskyG9uFUWG69wC6TJ7DKNCp6QCEM2 (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZKvgpCjEe7XvYMFtxYEdvEJRkY1UBkhYFVfW7sxeZtRMeax4A (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZ3N2AuMYpqerifycLiLPtsV8B72VgAsPjPAwrLa7xyfWBUJ2t (spent)
--    /api/addresses/summary/Ae2tdPwUPEZHhgPpDi2g5nE1UhHn6hCqihNxwkhTgxQayQ6FwE3eKEypDZU (spent)

-- Example regular addresses:
--    /api/addresses/summary/DdzFFzCqrhsszHTvbjTmYje5hehGbadkT6WgWbaqCy5XNxNttsPNF13eAjjBHYT7JaLJz2XVxiucam1EvwBRPSTiCrT4TNCBas4hfzic
--    /api/addresses/summary/DdzFFzCqrht1XitMM2mA1Qp4iW2jcX1Gx6f7tcJ4mfzXdS5VUW9sM28UejDGxz6eUDvdMTeuTqwZdSUwDiezcTafs89DNKEsSoCAX9ji
--    /api/addresses/summary/DdzFFzCqrhsq7KBnN2VngWtvPMriYFCNwWxW5PSnooLeNhNaz6Q2GhyfjHn7sY3BNiymhAkcCSTRhTBe8PZXSEEewuyWryURhmozdd6b
--    /api/addresses/summary/DdzFFzCqrhtCMkF4akdRbon1FFXStuc3YjW1gvYdE8sQKVUsgtUAwF5wf8EtSdCQ4biCRueMSDQSouqrSdhWbz2u41Xzt6WKEShgBynd
--    /api/addresses/summary/DdzFFzCqrht7YAnA3PW3RQEdYdQMQdodHG87H6vF766xVZuXjc3hkBN9PH8oQZGxAdQhiqgTYm24KAZyEGh3N6yxjj6Y5LfWJE83ngLp
--    /api/addresses/summary/DdzFFzCqrhsqz23SkTxevzJ3Dn4ee14BpQVe5T9LX2yWJpcjHToP2qxnzaEiy5qiHwNVtX5ANXtLJyBwKz8PvjJZYq2n8fyy7Dp9RqXa

-- Binance cold storage address (huge number of inputs and outputs):
--    /api/addresses/summary/DdzFFzCqrhtBatWqyFge4w6M6VLgNUwRHiXTAg3xfQCUdTcjJxSrPHVZJBsQprUEc5pRhgMWQaGciTssoZVwrSKmG1fneZ1AeCtLgs5Y


addressSummary
    :: SqlBackend -> CAddress
    -> Handler (Either ExplorerError CAddressSummary)
addressSummary backend (CAddress addrTxt) =
    runExceptT $ do
      addr <- hoistEither $ decodeTextAddress addrTxt
      newExceptT .
        runQuery backend $
          if isRedeemAddress addr
            then queryRedeemSummary addrTxt
            else Right <$> queryAddressSummary addrTxt

-- -------------------------------------------------------------------------------------------------

queryAddressSummary :: MonadIO m => Text -> ReaderT SqlBackend m CAddressSummary
queryAddressSummary addr = do
    inrows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (txOut ^. TxOutAddress ==. val addr)
                pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)
    -- This needs to be distinct to avoid duplicate rows.
    outrows <- select . distinct . from $ \ (blk `InnerJoin` tx `InnerJoin` txIn `InnerJoin` txOut) -> do
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                on (tx ^. TxId ==. txIn ^. TxInTxInId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (txOut ^. TxOutAddress ==. val addr)
                pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)

    cAddressSummary
        <$> mapM (queryCTxBrief . unValue3) inrows
        <*> mapM (queryCTxBrief . unValue3) outrows
  where
    cAddressSummary :: [CTxBrief] -> [CTxBrief] -> CAddressSummary
    cAddressSummary itxs otxs =
      let insum = sumCCoin . map snd $ filter isTargetAddress (concatMap ctbOutputs itxs)
          outsum = sumCCoin . map snd . filter isTargetAddress $ catMaybes (concatMap ctbInputs otxs)
          txs = List.sortOn ctbTimeIssued (itxs ++ otxs)
          fees = sumCCoin $ map ctbFees (itxs ++ otxs)
      in
      CAddressSummary
        { caAddress = CAddress addr
        , caType = CPubKeyAddress
        , caTxNum = fromIntegral $ length txs
        , caBalance = mkCCoin $ unCCoin insum - unCCoin outsum
        , caTotalInput = insum
        , caTotalOutput = outsum
        , caTotalFee = fees
        , caTxList = txs
        }

    isTargetAddress :: (CAddress, a) -> Bool
    isTargetAddress (CAddress tst, _) = tst == addr

-- -------------------------------------------------------------------------------------------------

-- | Redeem addresses are sufficiently different to warrant their own query.
queryRedeemSummary :: MonadIO m => Text -> ReaderT SqlBackend m (Either ExplorerError CAddressSummary)
queryRedeemSummary addrTxt = do
    -- Find the initial value assigned to this address at Genesis
    rows <- select . from $ \ txOut -> do
              where_ (txOut ^. TxOutAddress ==. val addrTxt)
              pure (txOut ^. TxOutValue)
    case rows of
      [] -> pure $ Left (Internal "queryRedeemSummary: Address not found")
      [value] -> Right <$> queryRedeemed (unValue value)
      _ -> pure $ Left (Internal "queryRedeemSummary: More than one entry")
  where
    queryRedeemed :: MonadIO m => Word64 -> ReaderT SqlBackend m CAddressSummary
    queryRedeemed value = do
      -- Query to see if the Genesis value has been spent.
      -- Will return [] if unspent and otherwise a single row.
      outrows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txIn `InnerJoin` txOut) -> do
                    on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                        &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                    on (tx ^. TxId ==. txIn ^. TxInTxInId)
                    on (blk ^. BlockId ==. tx ^. TxBlock)
                    where_ (txOut ^. TxOutAddress ==. val addrTxt)
                    pure (tx ^. TxId, tx ^. TxHash, blk ^. BlockTime)
      case outrows of
        [] -> pure $ convertUnspent value
        _ -> convertSpent <$> mapM (queryCTxBrief . unValue3) outrows

    convertUnspent :: Word64 -> CAddressSummary
    convertUnspent balance =
      CAddressSummary
        { caAddress = CAddress addrTxt
        , caType = CRedeemAddress
        , caTxNum = 0
        , caBalance = mkCCoin $ fromIntegral balance
        , caTotalInput = mkCCoin 0
        , caTotalOutput = mkCCoin 0
        , caTotalFee = mkCCoin 0
        , caTxList = []
        }

    convertSpent :: [CTxBrief] -> CAddressSummary
    convertSpent txs =
      CAddressSummary
        { caAddress = CAddress addrTxt
        , caType = CRedeemAddress
        , caTxNum = fromIntegral $ length txs
        , caBalance = mkCCoin 0
        , caTotalInput = mkCCoin 0
        , caTotalOutput = mkCCoin 0
        , caTotalFee = mkCCoin 0
        , caTxList = txs
        }

-- -------------------------------------------------------------------------------------------------

-- This is similar but different from the query of the same name in Explorer.Web.Server.BlocksTxs.
queryCTxBrief :: MonadIO m => (TxId, ByteString, UTCTime) -> ReaderT SqlBackend m CTxBrief
queryCTxBrief (txid, txhash, utctime) = do
    inrows <- select . from $ \(tx `InnerJoin` txIn `InnerJoin` txOut) -> do
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                on (tx ^. TxId ==. val txid)
                where_ (txIn ^. TxInTxInId ==. val txid)
                pure (txOut ^. TxOutAddress, txOut ^. TxOutValue)
    let inputs = map convert inrows

    outrows <- select . from $ \ (tx `InnerJoin` txOut) -> do
                  on (tx ^. TxId ==. txOut ^. TxOutTxId)
                  where_ (tx ^. TxId ==. val txid)
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
            , ctbFees = mkCCoin $ fromIntegral (inSum - outSum)
            }
  where
    convert :: (Value Text, Value Word64) -> (CAddress, Word64)
    convert (Value addr, Value coin) = (CAddress addr, coin)
