{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Explorer.Web.Server.TxLast
  ( getLastTxs
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Fixed (Fixed (..), Uni)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin(..), Value,
                    (^.), (==.),
                    desc, from, limit, on, orderBy, select, sum_, val, where_, unValue)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), Meta, TxId, isJust, listToMaybe, queryMeta,
                    slotPosixTime)
import           Explorer.Web.ClientTypes (CHash (..), CTxEntry (..), CTxHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Server.Util

import           Servant (Handler)


getLastTxs :: SqlBackend -> Handler (Either ExplorerError [CTxEntry])
getLastTxs backend =
  runQuery backend $ do
    emeta <- queryMeta
    case emeta of
      Left err -> pure $ Left (EELookupFail err)
      Right meta -> Right <$> queryCTxEntry meta


queryCTxEntry :: MonadIO m => Meta -> ReaderT SqlBackend m [CTxEntry]
queryCTxEntry meta = do
    txRows <- select . from $ \ (blk `InnerJoin` tx) -> do
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (isJust $ blk ^. BlockSlotNo)
                orderBy [desc (blk ^. BlockSlotNo)]
                limit 20
                pure (blk ^. BlockSlotNo, tx ^. TxHash, tx ^. TxId)
    mapM (queryTxOutputs meta) $ map convert txRows
  where
    convert :: (Value (Maybe Word64), Value ByteString, Value TxId) -> (Maybe Word64, ByteString, TxId)
    convert (a, b, c) = (unValue a, unValue b, unValue c)

-- TODO : It should be possible to do this as part of the above in a single qeuery.
queryTxOutputs :: MonadIO m => Meta -> (Maybe Word64, ByteString, TxId) -> ReaderT SqlBackend m CTxEntry
queryTxOutputs meta (mSlotNo, hash, txid) = do
    total <- select . from $ \ txOut -> do
                where_ (txOut ^. TxOutTxId ==. val txid)
                pure $ sum_ (txOut ^. TxOutValue)
    pure $ CTxEntry
            { cteId = CTxHash $ CHash (bsBase16Encode hash)
            , cteTimeIssued = fmap (slotPosixTime meta) mSlotNo
            , cteAmount = mkCCoin (unTotal $ listToMaybe total)
            }

unTotal :: Maybe (Value (Maybe Uni)) -> Integer
unTotal mvi =
  case fmap unValue mvi of
    Just (Just (MkFixed x)) -> x
    _ -> 0
