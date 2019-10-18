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

import           Database.Esqueleto (Entity, InnerJoin(..), SqlExpr, Value,
                    (^.), (==.),
                    desc, from, limit, on, orderBy, select, sub_select, sum_, where_, unValue)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), Meta, Tx, isJust, queryMeta, slotPosixTime)
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
                pure (blk ^. BlockSlotNo, tx ^. TxHash, txOutValue tx)
    pure $ map convert txRows
  where
    convert :: (Value (Maybe Word64), Value ByteString, Value (Maybe Uni)) -> CTxEntry
    convert (vslot, vhash, vtotal) =
      CTxEntry
        { cteId = CTxHash . CHash $ bsBase16Encode (unValue vhash)
        , cteTimeIssued = fmap (slotPosixTime meta) (unValue vslot)
        , cteAmount = mkCCoin (unTotal vtotal)
        }

txOutValue :: SqlExpr (Entity Tx) -> SqlExpr (Value (Maybe Uni))
txOutValue tx =
  sub_select . from $ \ txOut -> do
    where_ (txOut ^. TxOutTxId ==. tx ^. TxId)
    pure $ sum_ (txOut ^. TxOutValue)

unTotal :: Value (Maybe Uni) -> Integer
unTotal mvi =
  case unValue mvi of
    Just (MkFixed x) -> x
    _ -> 0
