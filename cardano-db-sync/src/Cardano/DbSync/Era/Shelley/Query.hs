{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Era.Shelley.Query
  ( queryPoolHashId
  , queryStakeAddress
  , queryStakeAddressAndPool
  , queryStakePoolKeyHash
  , queryTxInputSum
  ) where


import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Db
import           Cardano.DbSync.Types
import           Cardano.DbSync.Era.Shelley.Util (unKeyHash)
import           Cardano.DbSync.Util

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Either (fromRight)
import           Data.Maybe (listToMaybe)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..),  (^.), (==.), (<=.),
                    desc, from, on, orderBy, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import qualified Shelley.Spec.Ledger.TxBody as Shelley

queryPoolHashId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe PoolHashId)
queryPoolHashId hash = do
  res <- select . from $ \ phash -> do
            where_ (phash ^. PoolHashHash ==. val hash)
            pure (phash ^. PoolHashId)
  pure $ unValue <$> listToMaybe res

queryStakeAddress :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail StakeAddressId)
queryStakeAddress addr = do
  res <- select . from $ \ saddr -> do
            where_ (saddr ^. StakeAddressHashRaw ==. val addr)
            pure (saddr ^. StakeAddressId)
  pure $ maybeToEither (DbLookupMessage $ "StakeAddress " <> renderByteArray addr) unValue (listToMaybe res)

-- Get the stake address id and the pool hash id the stake address is delegated to in the specified
-- epoch. This is called when populating the reward table. Most rewards are the result of a
-- delegation which is caught in the first query below. However, if a reward address is specified
-- as the reward address for a pool, then no explicit delegation is needed. The second part of the
-- query catches this situation.
queryStakeAddressAndPool :: MonadIO m => Word64 -> ByteString -> ReaderT SqlBackend m (Either LookupFail (StakeAddressId, PoolHashId))
queryStakeAddressAndPool epoch addr = do
    res <- select . from $ \ (saddr `InnerJoin` dlg) -> do
            on (saddr ^. StakeAddressId ==. dlg ^. DelegationAddrId)
            where_ (saddr ^. StakeAddressHashRaw ==. val addr)
            where_ (dlg ^. DelegationActiveEpochNo <=. val epoch)
            orderBy [desc (dlg ^. DelegationActiveEpochNo)]
            pure (saddr ^. StakeAddressId, dlg ^. DelegationPoolHashId)
    maybe queryPool (pure . Right . unValue2) (listToMaybe res)
  where
    queryPool :: MonadIO m => ReaderT SqlBackend m (Either LookupFail (StakeAddressId, PoolHashId))
    queryPool = do
      res <- select . from $ \ (saddr `InnerJoin` pu) -> do
                on (saddr ^. StakeAddressId ==. pu ^. PoolUpdateRewardAddrId)
                where_ (saddr ^. StakeAddressHashRaw ==. val addr)
                where_ (pu ^. PoolUpdateActiveEpochNo <=. val epoch)
                orderBy [desc (pu ^. PoolUpdateActiveEpochNo)]
                pure (saddr ^. StakeAddressId, pu ^. PoolUpdateHashId)
      pure $ maybeToEither (DbLookupMessage $ "StakeAddressAndPool " <> renderByteArray addr) unValue2 (listToMaybe res)

queryStakePoolKeyHash :: MonadIO m => ShelleyStakePoolKeyHash -> ReaderT SqlBackend m (Either LookupFail PoolHashId)
queryStakePoolKeyHash kh = do
  res <- select . from $ \ (poolUpdate `InnerJoin` poolHash `InnerJoin` tx `InnerJoin` blk) -> do
            on (blk ^. BlockId ==. tx ^. TxBlock)
            on (tx ^. TxId ==. poolUpdate ^. PoolUpdateRegisteredTxId)
            on (poolUpdate ^. PoolUpdateHashId ==. poolHash ^. PoolHashId)
            where_ (poolHash ^. PoolHashHash ==. val (unKeyHash kh))
            orderBy [desc (blk ^. BlockSlotNo)]
            pure (poolHash ^. PoolHashId)
  pure $ maybeToEither (DbLookupMessage "StakePoolKeyHash") unValue (listToMaybe res)

queryTxInputSum :: MonadIO m => [ShelleyTxIn] -> ReaderT SqlBackend m Word64
queryTxInputSum txins =
    sum <$> mapM queryTxInputValue txins
  where
    queryTxInputValue :: MonadIO m => ShelleyTxIn -> ReaderT SqlBackend m Word64
    queryTxInputValue (Shelley.TxIn (Shelley.TxId hash) index) =
      fromRight 0 <$> queryTxOutValue (Crypto.hashToBytes hash, fromIntegral index)
