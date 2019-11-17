module Explorer.Web.Api.Legacy.GenesisPages
  ( genesisPages
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (listToMaybe)
import           Database.Esqueleto (InnerJoin (..), Value,
                    (^.), (==.), countRows, from, on, select, unValue, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), txOutSpentP, txOutUnspentP)

import           Explorer.Web.ClientTypes (CAddressesFilter (..))
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy (PageNumber)
import           Explorer.Web.Api.Legacy.Util (divRoundUp, runQuery, toPageSize)
import           Explorer.Web.Api.Legacy.Types (PageSize (..))

import           Servant (Handler)


genesisPages
    :: SqlBackend -> Maybe PageSize
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

queryGenesisAddressCount :: MonadIO m => PageSize -> ReaderT SqlBackend m Word
queryGenesisAddressCount (PageSize pageSize) = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure countRows
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)

queryRedeemedGenesisAddressCount :: MonadIO m => PageSize -> ReaderT SqlBackend m Word
queryRedeemedGenesisAddressCount (PageSize pageSize) = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            txOutSpentP txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure countRows
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)

queryUnRedeemedGenesisAddressCount :: MonadIO m => PageSize -> ReaderT SqlBackend m Word
queryUnRedeemedGenesisAddressCount (PageSize pageSize) = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            txOutUnspentP txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure countRows
  pure $ maybe 0 (dividePageSize pageSize) (listToMaybe res)


dividePageSize :: Word -> Value Word -> Word
dividePageSize pageSize vw =
  divRoundUp (unValue vw) pageSize
