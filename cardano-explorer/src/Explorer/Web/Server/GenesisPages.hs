module Explorer.Web.Server.GenesisPages
  ( genesisPages
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Esqueleto (InnerJoin (..), Value,
                    (^.), (==.), count, from, on, select, unValue, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), listToMaybe, txOutSpent, txOutUnspent)

import           Explorer.Web.ClientTypes (CAddressesFilter (..))
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.LegacyApi (PageNumber)
import           Explorer.Web.Server.Util (divRoundUp, runQuery, toPageSize)

import           Servant (Handler)


genesisPages
    :: SqlBackend -> Maybe PageNumber
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError PageNumber)
genesisPages backend mPageSize mAddrFilter =
    runQuery backend $
      case mAddrFilter of
        Just RedeemedAddresses -> Right <$> queryRedeemedGenesisAddressCount pageSize
        Just NonRedeemedAddresses -> Right <$> queryUnRedeemedGenesisAddressCount pageSize
        _ -> Right <$> queryGenesisAddressCount pageSize
  where
    pageSize = toPageSize mPageSize

queryGenesisAddressCount :: MonadIO m => Word -> ReaderT SqlBackend m Word
queryGenesisAddressCount pageSize = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure (count (tx ^. TxFee))
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)

queryRedeemedGenesisAddressCount :: MonadIO m => Word -> ReaderT SqlBackend m Word
queryRedeemedGenesisAddressCount pageSize = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            txOutSpent txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure (count (tx ^. TxFee))
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)

queryUnRedeemedGenesisAddressCount :: MonadIO m => Word -> ReaderT SqlBackend m Word
queryUnRedeemedGenesisAddressCount pageSize = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            txOutUnspent txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure (count (tx ^. TxFee))
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)


dividePageSize :: Word -> Value Word -> Word
dividePageSize pageSize vw =
  divRoundUp (unValue vw) pageSize
