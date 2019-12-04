{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.GenesisSummary
  ( genesisSummary
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Fixed (Fixed (..), Uni)
import           Data.Maybe (listToMaybe)

import           Database.Esqueleto (InnerJoin (..), Value,
                    (^.), (==.), countRows, from, on, select, sum_, unValue,
                    val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), txOutSpentP)

import           Explorer.Web.ClientTypes (CGenesisSummary (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy.Util (runQuery)

import           Servant (Handler)


genesisSummary :: SqlBackend -> Handler (Either ExplorerError CGenesisSummary)
genesisSummary backend =
  runQuery backend $ Right <$> do
    (numTotal,valTotal) <- queryInitialGenesis
    (redTotal, valRedeemed) <- queryGenesisRedeemed
    pure $ CGenesisSummary
            { cgsNumTotal = numTotal
            , cgsNumRedeemed = redTotal
            , cgsNumNotRedeemed = numTotal - redTotal
            , cgsRedeemedAmountTotal = mkCCoin valRedeemed
            , cgsNonRedeemedAmountTotal = mkCCoin $ valTotal - valRedeemed
            }

queryInitialGenesis :: MonadIO m => ReaderT SqlBackend m (Word, Integer)
queryInitialGenesis = do
    res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              on (blk ^. BlockId ==. tx ^. TxBlock)
              -- Only the initial genesis block has a size of 0.
              where_ (blk ^. BlockSize ==. val 0)
              pure (countRows, sum_ (txOut ^. TxOutValue))
    pure $ maybe (0, 0) convertPair (listToMaybe res)

queryGenesisRedeemed :: MonadIO m => ReaderT SqlBackend m (Word, Integer)
queryGenesisRedeemed = do
    res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              on (blk ^. BlockId ==. tx ^. TxBlock)
              txOutSpentP txOut
              -- Only the initial genesis block has a size of 0.
              where_ (blk ^. BlockSize ==. val 0)
              pure (countRows, sum_ (txOut ^. TxOutValue))
    pure $ maybe (0, 0) convertPair (listToMaybe res)

convertPair :: (Value Word, Value (Maybe Uni)) -> (Word, Integer)
convertPair (vcount, vtotal) = (unValue vcount, unTotal vtotal)

unTotal :: Value (Maybe Uni) -> Integer
unTotal mvi =
  case unValue mvi of
    Just (MkFixed x) -> x
    _ -> 0
