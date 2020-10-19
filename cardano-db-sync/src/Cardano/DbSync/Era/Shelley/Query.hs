{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Era.Shelley.Query
  ( queryPoolHashId
  , queryStakeAddress
  , queryStakeAddressAndPool
  , queryStakePoolKeyHash
  , queryStakeAddressRef
  , queryTxInputSum
  ) where


import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Db
import           Cardano.DbSync.Era.Shelley.Util (unKeyHashRaw)
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Either (fromRight)
import           Data.Maybe (listToMaybe)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), desc, from, just, on, orderBy,
                   select, val, where_, (<=.), (==.), (^.))
import           Database.Persist.Sql (SqlBackend)

import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.Credential (Ptr (..), StakeReference (..))
import qualified Shelley.Spec.Ledger.TxBody as Shelley

queryPoolHashId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe PoolHashId)
queryPoolHashId hash = do
  res <- select . from $ \ phash -> do
            where_ (phash ^. PoolHashHashRaw ==. val hash)
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
            where_ (poolHash ^. PoolHashHashRaw ==. val (unKeyHashRaw kh))
            orderBy [desc (blk ^. BlockSlotNo)]
            pure (poolHash ^. PoolHashId)
  pure $ maybeToEither (DbLookupMessage "StakePoolKeyHash") unValue (listToMaybe res)

queryStakeAddressRef :: MonadIO m => Shelley.Addr StandardShelley -> ReaderT SqlBackend m (Maybe StakeAddressId)
queryStakeAddressRef addr =
    case addr of
      Shelley.AddrBootstrap {} -> pure Nothing
      Shelley.Addr nw _pcred sref ->
        case sref of
          StakeRefBase cred -> do
            eres <- queryStakeAddress $ Shelley.serialiseRewardAcnt (Shelley.RewardAcnt nw cred)
            pure $ either (const Nothing) Just eres
          StakeRefPtr (Ptr slotNo txIx certIx) -> queryStakeDelegation slotNo txIx certIx
          StakeRefNull -> pure Nothing
  where
    queryStakeDelegation :: MonadIO m => SlotNo -> Natural -> Natural -> ReaderT SqlBackend m (Maybe StakeAddressId)
    queryStakeDelegation (SlotNo slot) txIx certIx = do
      res <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` dlg) -> do
                on (tx ^. TxId ==. dlg ^. DelegationTxId)
                on (blk ^. BlockId ==. tx ^. TxBlock)
                where_ (blk ^. BlockSlotNo ==. just (val slot))
                where_ (tx ^. TxBlockIndex ==. val (fromIntegral txIx))
                where_ (dlg ^. DelegationCertIndex ==. val (fromIntegral certIx))
                pure (dlg ^. DelegationAddrId)
      pure $ unValue <$> listToMaybe res

queryTxInputSum :: MonadIO m => [ShelleyTxIn] -> ReaderT SqlBackend m DbLovelace
queryTxInputSum txins =
    DbLovelace . sum . map unDbLovelace <$> mapM queryTxInputValue txins
  where
    queryTxInputValue :: MonadIO m => ShelleyTxIn -> ReaderT SqlBackend m DbLovelace
    queryTxInputValue (Shelley.TxIn (Shelley.TxId hash) index) =
      fromRight (DbLovelace 0) <$> queryTxOutValue (Crypto.hashToBytes hash, fromIntegral index)
