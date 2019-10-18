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
import           Explorer.Web.Server.Util (defaultPageSize, divRoundUp, runQuery)

import           Servant (Handler)


genesisPages
    :: SqlBackend -> Maybe PageNumber
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError PageNumber)
genesisPages backend _ mAddrFilter =
  runQuery backend $
    case mAddrFilter of
      Just RedeemedAddresses -> Right <$> queryRedeemedGenesisAddressCount
      Just NonRedeemedAddresses -> Right <$> queryUnRedeemedGenesisAddressCount
      _ -> Right <$> queryGenesisAddressCount

queryGenesisAddressCount :: MonadIO m => ReaderT SqlBackend m Word
queryGenesisAddressCount = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure (count (tx ^. TxFee))
  pure $ maybe 0 dividePageSize (listToMaybe res)

queryRedeemedGenesisAddressCount :: MonadIO m => ReaderT SqlBackend m Word
queryRedeemedGenesisAddressCount = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            txOutSpent txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure (count (tx ^. TxFee))
  pure $ maybe 0 dividePageSize (listToMaybe res)

queryUnRedeemedGenesisAddressCount :: MonadIO m => ReaderT SqlBackend m Word
queryUnRedeemedGenesisAddressCount = do
  res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            txOutUnspent txOut
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            pure (count (tx ^. TxFee))
  pure $ maybe 0 dividePageSize (listToMaybe res)


dividePageSize :: Value Word -> Word
dividePageSize vw =
  divRoundUp (unValue vw) defaultPageSize
