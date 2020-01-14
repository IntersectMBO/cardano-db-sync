{-# LANGUAGE OverloadedStrings #-}
module Explorer.Web.Api.Legacy.GenesisAddress
  ( genesisAddressInfo
  , queryAllGenesisAddresses
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), SqlQuery, Value, (^.), (==.),
                    desc, from, limit, offset, on, orderBy, select, unValue, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), txOutSpentB, txOutSpentP, txOutUnspentP)

import           Explorer.Web.ClientTypes (CAddress (..), CAddressesFilter (..),
                    CGenesisAddressInfo (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy.Types (PageNo (..), PageSize (..))
import           Explorer.Web.Api.Legacy.Util (runQuery, toPageSize)

import           Servant (Handler)


genesisAddressInfo
    :: SqlBackend -> Maybe PageNo -> Maybe PageSize -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError [CGenesisAddressInfo])
-- filter redeemed addresses
genesisAddressInfo backend mPage mPageSize mAddrFilter =
   if unPageSize pageSize < 1
     then pure $ Left (Internal "Page size must be greater than 1.")
     else genesisAddrInfo backend page pageSize addrFilter
  where
    pageSize = toPageSize mPageSize
    page = fromMaybe (PageNo 0) mPage
    addrFilter = fromMaybe AllAddresses mAddrFilter

genesisAddrInfo
    :: SqlBackend -> PageNo -> PageSize -> CAddressesFilter
    -> Handler (Either ExplorerError [CGenesisAddressInfo])
-- filter redeemed addresses
genesisAddrInfo backend page pageSize addrFilter =
  runQuery backend $
    case addrFilter of
      RedeemedAddresses -> Right <$> queryRedeemedGenesisAddresses page pageSize
      NonRedeemedAddresses -> Right <$> queryNonRedeemedGenesisAddresses page pageSize
      AllAddresses -> Right <$> queryAllGenesisAddresses page pageSize

queryRedeemedGenesisAddresses
    :: MonadIO m
    => PageNo -> PageSize
    -> ReaderT SqlBackend m [CGenesisAddressInfo]
queryRedeemedGenesisAddresses pageNo pageSize = do
  rows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            txOutSpentP txOut
            orderBy [desc (txOut ^. TxOutValue)]
            applyPaging pageNo pageSize
            pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, txOutSpentB txOut)
  pure $ map mkCGenesisAddressInfo rows

queryNonRedeemedGenesisAddresses
    :: MonadIO m
    => PageNo -> PageSize
    -> ReaderT SqlBackend m [CGenesisAddressInfo]
queryNonRedeemedGenesisAddresses pageNo pageSize = do
  rows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            txOutUnspentP txOut
            orderBy [desc (txOut ^. TxOutValue)]
            applyPaging pageNo pageSize
            pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, txOutSpentB txOut)
  pure $ map mkCGenesisAddressInfo rows

queryAllGenesisAddresses
    :: MonadIO m
    => PageNo -> PageSize
    -> ReaderT SqlBackend m [CGenesisAddressInfo]
queryAllGenesisAddresses pageNo pageSize = do
  rows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txOut) -> do
            on (tx ^. TxId ==. txOut ^. TxOutTxId)
            on (blk ^. BlockId ==. tx ^. TxBlock)
            -- Only the initial genesis block has a size of 0.
            where_ (blk ^. BlockSize ==. val 0)
            orderBy [desc (txOut ^. TxOutValue)]
            applyPaging pageNo pageSize
            pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, txOutSpentB txOut)
  pure $ map mkCGenesisAddressInfo rows

applyPaging :: PageNo -> PageSize -> SqlQuery ()
applyPaging (PageNo page) (PageSize pageSize) = do
  when (page > 0) $
    offset (fromIntegral $ page * pageSize)
  limit (fromIntegral pageSize)

mkCGenesisAddressInfo :: (Value Text, Value Word64, Value Bool) -> CGenesisAddressInfo
mkCGenesisAddressInfo (vaddr, vvalue, vRedeemed) =
  CGenesisAddressInfo
    { cgaiCardanoAddress = CAddress (unValue vaddr)
    , cgaiGenesisAmount = mkCCoin (fromIntegral $ unValue vvalue)
    , cgaiIsRedeemed = unValue vRedeemed
    }
