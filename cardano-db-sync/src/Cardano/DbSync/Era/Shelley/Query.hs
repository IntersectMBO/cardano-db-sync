{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Query
  ( queryPoolHashId
  , queryStakeAddress
  , queryStakeRefPtr
  , queryStakeDelegation
  , queryResolveInput
  , queryResolveInputCredentials

  , queryPoolUpdateByBlock
  ) where

import           Cardano.Prelude hiding (Ptr, from, maybeToEither, on)

import           Cardano.Db
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.Ledger.BaseTypes

import           Cardano.Ledger.Credential (Ptr (..))

import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Database.Esqueleto.Experimental (SqlBackend, Value (..), desc, from, innerJoin,
                   just, limit, on, orderBy, select, table, val, where_, (:&) ((:&)), (==.), (^.))

{- HLINT ignore "Fuse on/on" -}

queryPoolHashId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe PoolHashId)
queryPoolHashId hash = do
  res <- select $ do
    phash <- from $ table @PoolHash
    where_ (phash ^. PoolHashHashRaw ==. val hash)
    pure (phash ^. PoolHashId)
  pure $ unValue <$> listToMaybe res

queryStakeAddress
    :: MonadIO m
    => ByteString
    -> ReaderT SqlBackend m (Either LookupFail StakeAddressId)
queryStakeAddress addr = do
  res <- select $ do
    saddr <- from $ table @StakeAddress
    where_ (saddr ^. StakeAddressHashRaw ==. val addr)
    pure (saddr ^. StakeAddressId)
  pure $ maybeToEither (DbLookupMessage $ "StakeAddress " <> renderByteArray addr) unValue (listToMaybe res)

queryStakeDelegation
    :: MonadIO m
    => Ptr
    -> ReaderT SqlBackend m (Maybe StakeAddressId)
queryStakeDelegation (Ptr (SlotNo slot) (TxIx txIx) (CertIx certIx)) = do
  res <- select $ do
    (dlg :& tx :& blk) <-
      from $ table @Delegation
      `innerJoin` table @Tx
      `on` (\(dlg :& tx) -> tx ^. TxId ==. dlg ^. DelegationTxId)
      `innerJoin` table @Block
      `on` (\(_dlg :& tx :& blk) -> blk ^. BlockId ==. tx ^. TxBlockId)
    where_ (blk ^. BlockSlotNo ==. just (val slot))
    where_ (tx ^. TxBlockIndex ==. val (fromIntegral txIx))
    where_ (dlg ^. DelegationCertIndex ==. val (fromIntegral certIx))
    -- Need to order by BlockSlotNo descending for correct behavior when there are two
    -- or more delegation certificates in a single epoch.
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (dlg ^. DelegationAddrId)
  pure $ unValue <$> listToMaybe res

queryResolveInput :: MonadIO m => Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (TxId, DbLovelace))
queryResolveInput txIn =
  queryTxOutValue (Generic.txInHash txIn, fromIntegral (Generic.txInIndex txIn))

queryResolveInputCredentials :: MonadIO m => Generic.TxIn -> ReaderT SqlBackend m (Either LookupFail (Maybe ByteString, Bool))
queryResolveInputCredentials txIn = do
    queryTxOutCredentials (Generic.txInHash txIn, fromIntegral (Generic.txInIndex txIn))

queryStakeRefPtr :: MonadIO m => Ptr -> ReaderT SqlBackend m (Maybe StakeAddressId)
queryStakeRefPtr (Ptr (SlotNo slot) (TxIx txIx) (CertIx certIx)) = do
  res <- select $ do
    (blk :& tx :& sr) <-
      from $ table @Block
      `innerJoin` table @Tx
      `on` (\(blk :& tx) -> blk ^. BlockId ==. tx ^. TxBlockId)
      `innerJoin` table @StakeRegistration
      `on` (\(_blk :& tx :& sr) -> sr ^. StakeRegistrationTxId ==. tx ^. TxId)

    where_ (blk ^. BlockSlotNo ==. just (val slot))
    where_ (tx ^. TxBlockIndex ==. val (fromIntegral txIx))
    where_ (sr ^. StakeRegistrationCertIndex ==. val (fromIntegral certIx))
    -- Need to order by DelegationSlotNo descending for correct behavior when there are two
    -- or more delegation certificates in a single epoch.
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (sr ^. StakeRegistrationAddrId)
  pure $ unValue <$> listToMaybe res

-- Check if there are other PoolUpdates in the same blocks for the same pool
queryPoolUpdateByBlock :: MonadIO m => BlockId -> PoolHashId -> ReaderT SqlBackend m Bool
queryPoolUpdateByBlock blkId poolHashId = do
    res <- select $ do
      (blk :& _tx :& poolUpdate) <-
        from $ table @Block
        `innerJoin` table @Tx
        `on` (\(blk :& tx) -> blk ^. BlockId ==. tx ^. TxBlockId)
        `innerJoin` table @PoolUpdate
        `on` (\(_blk :& tx :& poolUpdate) -> tx ^. TxId ==. poolUpdate ^. PoolUpdateRegisteredTxId)
      where_ (poolUpdate ^. PoolUpdateHashId ==. val poolHashId)
      where_ (blk ^. BlockId ==. val blkId)
      limit 1
      pure (blk ^. BlockEpochNo)
    pure $ not (null res)
