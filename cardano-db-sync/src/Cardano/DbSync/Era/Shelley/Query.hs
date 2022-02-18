{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Query
  ( queryPoolHashId
  , queryStakeAddress
  , queryStakePoolKeyHash
  , queryStakeRefPtr
  , queryStakeAddressRef
  , queryResolveInput
  , queryResolveInputCredentials

  , queryStakeAddressIdPair
  , queryPoolHashIdPair
  , queryPoolUpdateByBlock
  ) where

import           Cardano.Prelude hiding (Ptr, from, maybeToEither, on)

import           Cardano.Db
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic

import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.Credential (Ptr (..), StakeReference (..))
import qualified Cardano.Ledger.Keys as Ledger

import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Database.Esqueleto.Experimental (SqlBackend, Value (..), desc, from, innerJoin,
                   just, limit, on, orderBy, select, table, val, where_, (:&) ((:&)), (==.), (^.))

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

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

queryStakePoolKeyHash
    :: forall era m. MonadIO m
    => Ledger.KeyHash 'Ledger.StakePool era
    -> ReaderT SqlBackend m (Either LookupFail PoolHashId)
queryStakePoolKeyHash kh = do
  res <- select $ do
    (blk :& _ :& _ :& poolHash) <-
      from $ table @Block
      `innerJoin` table @Tx
      `on` (\(blk :& tx) -> blk ^. BlockId ==. tx ^. TxBlockId)
      `innerJoin` table @PoolUpdate
      `on` (\(_ :& tx :& poolUpdate) -> tx ^. TxId ==. poolUpdate ^. PoolUpdateRegisteredTxId)
      `innerJoin` table @PoolHash
      `on` (\(_ :& _ :& poolUpdate :& poolHash) -> poolUpdate ^. PoolUpdateHashId ==. poolHash ^. PoolHashId)
    where_ (poolHash ^. PoolHashHashRaw ==. val (Generic.unKeyHashRaw kh))
    orderBy [desc (blk ^. BlockSlotNo)]
    limit 1
    pure (poolHash ^. PoolHashId)

  pure $ maybeToEither (DbLookupMessage "StakePoolKeyHash") unValue (listToMaybe res)

queryStakeAddressRef
    :: MonadIO m
    => Ledger.Addr StandardCrypto
    -> ReaderT SqlBackend m (Maybe StakeAddressId)
queryStakeAddressRef addr =
    case addr of
      Ledger.AddrBootstrap {} -> pure Nothing
      Ledger.Addr nw _pcred sref ->
        case sref of
          StakeRefBase cred -> do
            eres <- queryStakeAddress $ Ledger.serialiseRewardAcnt (Ledger.RewardAcnt nw cred)
            pure $ either (const Nothing) Just eres
          StakeRefPtr ptr -> queryStakeDelegation ptr
          StakeRefNull -> pure Nothing
  where
    queryStakeDelegation
        :: MonadIO m
        => Ptr
        -> ReaderT SqlBackend m (Maybe StakeAddressId)
    queryStakeDelegation (Ptr (SlotNo slot) txIx certIx) = do
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

queryStakeAddressIdPair :: MonadIO m => Generic.StakeCred -> ReaderT SqlBackend m (Maybe (Generic.StakeCred, StakeAddressId))
queryStakeAddressIdPair cred@(Generic.StakeCred bs) = do
    res <- select $ do
      saddr <- from $ table @StakeAddress
      where_ (saddr ^. StakeAddressHashRaw ==. val bs)
      pure $ saddr ^. StakeAddressId
    pure $ convert <$> listToMaybe res
  where
    convert :: Value StakeAddressId -> (Generic.StakeCred, StakeAddressId)
    convert (Value said) = (cred, said)

queryStakeRefPtr :: MonadIO m => Ptr -> ReaderT SqlBackend m (Maybe StakeAddressId)
queryStakeRefPtr (Ptr (SlotNo slot) txIx certIx) = do
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

queryPoolHashIdPair
    :: MonadIO m
    => Generic.StakePoolKeyHash
    -> ReaderT SqlBackend m (Maybe (Generic.StakePoolKeyHash, PoolHashId))
queryPoolHashIdPair pkh = do
    res <- select $ do
      pool <- from $ table @PoolHash
      where_ (pool ^. PoolHashHashRaw ==. val (Generic.unStakePoolKeyHash pkh))
      pure $ pool ^. PoolHashId
    pure $ convert <$> listToMaybe res
  where
    convert :: Value PoolHashId -> (Generic.StakePoolKeyHash, PoolHashId)
    convert (Value phid) = (pkh, phid)

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
