{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.Pool (
  IsPoolMember,
  insertPoolRegister,
  insertPoolRetire,
  insertPoolMetaDataRef,
  insertPoolOwner,
  insertPoolRelay,
  insertPoolCert,
) where

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Db (PoolUrl (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache (
  insertPoolKeyWithCache,
  queryOrInsertRewardAccount,
  queryOrInsertStakeAddress,
  queryPoolKeyOrInsert,
 )
import Cardano.DbSync.Cache.Types (Cache, CacheNew (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Error
import Cardano.DbSync.Types (PoolKeyHash)
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Core (PoolCert (..))
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.PoolParams as PoolP
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Prelude
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

type IsPoolMember = PoolKeyHash -> Bool

insertPoolRegister ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  IsPoolMember ->
  Ledger.Network ->
  EpochNo ->
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  PoolP.PoolParams StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister syncEnv cache isMember network (EpochNo epoch) blkId txId idx params = do
  poolHashId <- lift $ insertPoolKeyWithCache cache CacheNew (PoolP.ppId params)
  mdId <- case strictMaybeToMaybe $ PoolP.ppMetadata params of
    Just md -> Just <$> insertPoolMetaDataRef poolHashId txId md
    Nothing -> pure Nothing
  epochActivationDelay <- mkEpochActivationDelay poolHashId
  mSaId <- lift $ queryOrInsertRewardAccount syncEnv cache CacheNew (adjustNetworkTag $ PoolP.ppRewardAcnt params)
  case mSaId of
    Nothing -> pure ()
    Just saId -> do
      poolUpdateId <-
        lift
          . DB.insertPoolUpdate
          $ DB.PoolUpdate
            { DB.poolUpdateHashId = poolHashId
            , DB.poolUpdateCertIndex = idx
            , DB.poolUpdateVrfKeyHash = hashToBytes (PoolP.ppVrf params)
            , DB.poolUpdatePledge = Generic.coinToDbLovelace (PoolP.ppPledge params)
            , DB.poolUpdateRewardAddrId = saId
            , DB.poolUpdateActiveEpochNo = epoch + epochActivationDelay
            , DB.poolUpdateMetaId = mdId
            , DB.poolUpdateMargin = realToFrac $ Ledger.unboundRational (PoolP.ppMargin params)
            , DB.poolUpdateFixedCost = Generic.coinToDbLovelace (PoolP.ppCost params)
            , DB.poolUpdateRegisteredTxId = txId
            }

      mapM_ (insertPoolOwner syncEnv cache network poolUpdateId) $ toList (PoolP.ppOwners params)
      mapM_ (insertPoolRelay poolUpdateId) $ toList (PoolP.ppRelays params)
  where
    mkEpochActivationDelay :: MonadIO m => DB.PoolHashId -> ExceptT SyncNodeError (ReaderT SqlBackend m) Word64
    mkEpochActivationDelay poolHashId =
      if isMember (PoolP.ppId params)
        then pure 3
        else do
          -- if the pool is not registered at the end of the previous block, check for
          -- other registrations at the current block. If this is the first registration
          -- then it's +2, else it's +3.
          otherUpdates <- lift $ queryPoolUpdateByBlock blkId poolHashId
          pure $ if otherUpdates then 3 else 2

    -- Ignore the network in the `RewardAcnt` and use the provided one instead.
    -- This is a workaround for https://github.com/IntersectMBO/cardano-db-sync/issues/546
    adjustNetworkTag :: Ledger.RewardAcnt StandardCrypto -> Ledger.RewardAcnt StandardCrypto
    adjustNetworkTag (Shelley.RewardAcnt _ cred) = Shelley.RewardAcnt network cred

insertPoolRetire ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  DB.TxId ->
  EpochNo ->
  Word16 ->
  Ledger.KeyHash 'Ledger.StakePool StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire syncEnv cache txId epochNum idx keyHash = do
  poolId <- lift $ queryPoolKeyOrInsert "insertPoolRetire" syncEnv cache CacheNew True keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetireHashId = poolId
      , DB.poolRetireCertIndex = idx
      , DB.poolRetireAnnouncedTxId = txId
      , DB.poolRetireRetiringEpoch = unEpochNo epochNum
      }

insertPoolMetaDataRef ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.PoolHashId ->
  DB.TxId ->
  PoolP.PoolMetadata ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolMetadataRefId
insertPoolMetaDataRef poolId txId md =
  lift
    . DB.insertPoolMetadataRef
    $ DB.PoolMetadataRef
      { DB.poolMetadataRefPoolId = poolId
      , DB.poolMetadataRefUrl = PoolUrl $ Ledger.urlToText (PoolP.pmUrl md)
      , DB.poolMetadataRefHash = PoolP.pmHash md
      , DB.poolMetadataRefRegisteredTxId = txId
      }

insertPoolOwner ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Ledger.Network ->
  DB.PoolUpdateId ->
  Ledger.KeyHash 'Ledger.Staking StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner syncEnv cache network poolUpdateId skh = do
  mSaId <- lift $ queryOrInsertStakeAddress syncEnv cache CacheNew network (Ledger.KeyHashObj skh)
  case mSaId of
    Nothing -> pure ()
    Just saId ->
      void . lift . DB.insertPoolOwner $
        DB.PoolOwner
          { DB.poolOwnerAddrId = saId
          , DB.poolOwnerPoolUpdateId = poolUpdateId
          }

insertPoolRelay ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.PoolUpdateId ->
  PoolP.StakePoolRelay ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRelay updateId relay =
  void
    . lift
    . DB.insertPoolRelay
    $ case relay of
      PoolP.SingleHostAddr mPort mIpv4 mIpv6 ->
        DB.PoolRelay -- An IPv4 and/or IPv6 address
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = textShow <$> strictMaybeToMaybe mIpv4
          , DB.poolRelayIpv6 = textShow <$> strictMaybeToMaybe mIpv6
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Ledger.portToWord16 <$> strictMaybeToMaybe mPort
          }
      PoolP.SingleHostName mPort name ->
        DB.PoolRelay -- An A or AAAA DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Just (Ledger.dnsToText name)
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Ledger.portToWord16 <$> strictMaybeToMaybe mPort
          }
      PoolP.MultiHostName name ->
        DB.PoolRelay -- An SRV DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Just (Ledger.dnsToText name)
          , DB.poolRelayPort = Nothing
          }

insertPoolCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  IsPoolMember ->
  Ledger.Network ->
  EpochNo ->
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  PoolCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolCert syncEnv cache isMember network epoch blkId txId idx pCert =
  case pCert of
    RegPool pParams -> insertPoolRegister syncEnv (envCache syncEnv) isMember network epoch blkId txId idx pParams
    RetirePool keyHash epochNum -> insertPoolRetire syncEnv cache txId epochNum idx keyHash
