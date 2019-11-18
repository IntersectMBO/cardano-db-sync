{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.AddressSummary
  ( addressSummary
  ) where

import           Cardano.Chain.Common (isRedeemAddress)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (hoistEither, runExceptT, newExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List.Extra (groupOn)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..),
                    (^.), (==.), (&&.), distinct, from, in_, on, select, val, valList, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), TxId, unValue3)
import           Explorer.Web.ClientTypes (CAddress (..), CAddressSummary (..), CAddressType (..),
                    CCoin (..), CHash (..), CTxBrief (..), CTxHash (..), mkCCoin, sumCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy.RedeemSummary (queryRedeemSummary)
import           Explorer.Web.Api.Legacy.Util (bsBase16Encode, decodeTextAddress, runQuery)

import           Servant (Handler)

-- Example redeem addresses:
--    /api/addresses/summary/Ae2tdPwUPEZAAvxJ9nQ1nx88X9Jq6dskyG9uFUWG69wC6TJ7DKNCp6QCEM2 (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZKvgpCjEe7XvYMFtxYEdvEJRkY1UBkhYFVfW7sxeZtRMeax4A (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZ3N2AuMYpqerifycLiLPtsV8B72VgAsPjPAwrLa7xyfWBUJ2t (spent)
--    /api/addresses/summary/Ae2tdPwUPEZHhgPpDi2g5nE1UhHn6hCqihNxwkhTgxQayQ6FwE3eKEypDZU (spent)

-- Example regular addresses:
--    /api/addresses/summary/DdzFFzCqrhsszHTvbjTmYje5hehGbadkT6WgWbaqCy5XNxNttsPNF13eAjjBHYT7JaLJz2XVxiucam1EvwBRPSTiCrT4TNCBas4hfzic
--    /api/addresses/summary/DdzFFzCqrhsq7KBnN2VngWtvPMriYFCNwWxW5PSnooLeNhNaz6Q2GhyfjHn7sY3BNiymhAkcCSTRhTBe8PZXSEEewuyWryURhmozdd6b
--    /api/addresses/summary/DdzFFzCqrht7YAnA3PW3RQEdYdQMQdodHG87H6vF766xVZuXjc3hkBN9PH8oQZGxAdQhiqgTYm24KAZyEGh3N6yxjj6Y5LfWJE83ngLp
--    /api/addresses/summary/DdzFFzCqrhsqz23SkTxevzJ3Dn4ee14BpQVe5T9LX2yWJpcjHToP2qxnzaEiy5qiHwNVtX5ANXtLJyBwKz8PvjJZYq2n8fyy7Dp9RqXa

--    /api/addresses/summary/DdzFFzCqrht1XitMM2mA1Qp4iW2jcX1Gx6f7tcJ4mfzXdS5VUW9sM28UejDGxz6eUDvdMTeuTqwZdSUwDiezcTafs89DNKEsSoCAX9ji
--    /api/addresses/summary/DdzFFzCqrhtCMkF4akdRbon1FFXStuc3YjW1gvYdE8sQKVUsgtUAwF5wf8EtSdCQ4biCRueMSDQSouqrSdhWbz2u41Xzt6WKEShgBynd

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
        <$> queryCTxBriefs (map unValue3 inrows)
        <*> queryCTxBriefs (map unValue3 outrows)
  where
    cAddressSummary :: [CTxBrief] -> [CTxBrief] -> CAddressSummary
    cAddressSummary itxs otxs =
      let insum = sumCCoin . map snd $ filter isTargetAddress (concatMap ctbOutputs itxs)
          outsum = sumCCoin . map snd . filter isTargetAddress $ catMaybes (concatMap ctbInputs otxs)
          txs = List.sortOn ctbTimeIssued (itxs ++ otxs)
          fees = sumCCoin $ map ctbFees txs
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

queryCTxBriefs :: MonadIO m => [(TxId, ByteString, UTCTime)] -> ReaderT SqlBackend m [CTxBrief]
queryCTxBriefs [] = pure []
queryCTxBriefs xs = do
  let txids = map fst3 xs
  zipTxBrief xs <$> queryTxInputs txids <*> queryTxOutputs txids

queryTxInputs :: MonadIO m => [TxId] -> ReaderT SqlBackend m [(TxId, [(CAddress, Word64)])]
queryTxInputs txids = do
    rows <- select . distinct . from $ \(tx `InnerJoin` txIn `InnerJoin` txOut) -> do
                on (txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId
                    &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                on (tx ^. TxId ==. txIn ^. TxInTxInId)
                where_ (txIn ^. TxInTxInId `in_` valList txids)
                pure (tx ^. TxId, txOut ^. TxOutAddress, txOut ^. TxOutValue)
    case groupOn fst (map convert rows) of
      [] -> pure []
      xs -> pure $ map collapseTxGroup xs
  where
    convert :: (Value TxId, Value Text, Value Word64) -> (TxId, (CAddress, Word64))
    convert (Value txid, Value addr, Value coin) = (txid, (CAddress addr, coin))

queryTxOutputs :: MonadIO m => [TxId] -> ReaderT SqlBackend m [(TxId, [(CAddress, Word64)])]
queryTxOutputs txids = do
    rows <- select . from $ \ (tx `InnerJoin` txOut) -> do
                on (tx ^. TxId ==. txOut ^. TxOutTxId)
                where_ (tx ^. TxId `in_` valList txids)
                pure (tx ^. TxId, txOut ^. TxOutAddress, txOut ^. TxOutValue)
    case groupOn fst (map convert rows) of
      [] -> pure []
      xs -> pure $ map collapseTxGroup xs
  where
    convert :: (Value TxId, Value Text, Value Word64) -> (TxId, (CAddress, Word64))
    convert (Value txid, Value addr, Value coin) = (txid, (CAddress addr, coin))

-- -------------------------------------------------------------------------------------------------

collapseTxGroup :: [(TxId, (CAddress, Word64))] -> (TxId, [(CAddress, Word64)])
collapseTxGroup xs =
  case xs of
    [] -> error "collapseTxGroup: groupOn produced [] on non-empty list (impossible)"
    (x:_) -> (fst x, map snd xs)

zipTxBrief :: [(TxId, ByteString, UTCTime)] -> [(TxId, [(CAddress, Word64)])] -> [(TxId, [(CAddress, Word64)])] -> [CTxBrief]
zipTxBrief xs ins outs =
    mapMaybe build $ map fst3 xs
  where
    idMap :: Map TxId (ByteString, UTCTime)
    idMap = Map.fromList $ map (\(a, b, c) -> (a, (b, c))) xs

    inMap, outMap :: Map TxId [(CAddress, Word64)]
    inMap = Map.fromList ins
    outMap = Map.fromList outs

    build :: TxId -> Maybe CTxBrief
    build txid = do
      (hash, time) <- Map.lookup txid idMap
      inputs <- Map.lookup txid inMap
      outputs <- Map.lookup txid outMap
      inSum <- Just $ sum (map snd inputs)
      outSum <- Just $ sum (map snd outputs)
      pure $ CTxBrief
              { ctbId = CTxHash . CHash $ bsBase16Encode hash
              , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds time
              , ctbInputs = map (Just . fmap (mkCCoin . fromIntegral)) inputs
              , ctbOutputs = map (fmap (mkCCoin . fromIntegral)) outputs
              , ctbInputSum = mkCCoin $ fromIntegral inSum
              , ctbOutputSum = mkCCoin $ fromIntegral outSum
              , ctbFees = mkCCoin $ fromIntegral (inSum - outSum)
              }

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
