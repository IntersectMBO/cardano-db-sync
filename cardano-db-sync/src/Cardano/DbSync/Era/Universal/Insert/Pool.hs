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
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Cache (
  insertPoolKeyWithCache,
  queryOrInsertRewardAccount,
  queryOrInsertStakeAddress,
  queryPoolKeyOrInsert,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Error (SyncNodeError)
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

type IsPoolMember = PoolKeyHash -> Bool

insertPoolRegister ::
  SyncEnv ->
  IsPoolMember ->
  Maybe Generic.Deposits ->
  Ledger.Network ->
  EpochNo ->
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  PoolP.PoolParams ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolRegister syncEnv isMember mdeposits network (EpochNo epoch) blkId txId idx params = do
  poolHashId <- insertPoolKeyWithCache syncEnv UpdateCache (PoolP.ppId params)
  mdId <- case strictMaybeToMaybe $ PoolP.ppMetadata params of
    Just md -> Just <$> insertPoolMetaDataRef poolHashId txId md
    Nothing -> pure Nothing

  isRegistration <- isPoolRegistration poolHashId
  let epochActivationDelay = if isRegistration then 2 else 3
      deposit = if isRegistration then Generic.coinToDbLovelace . Generic.poolDeposit <$> mdeposits else Nothing

  saId <- queryOrInsertRewardAccount syncEnv UpdateCache (adjustNetworkTag $ PoolP.ppRewardAccount params)
  poolUpdateId <-
    lift $
      DB.insertPoolUpdate $
        DB.PoolUpdate
          { DB.poolUpdateHashId = poolHashId
          , DB.poolUpdateCertIndex = idx
          , DB.poolUpdateVrfKeyHash = hashToBytes $ Ledger.fromVRFVerKeyHash (PoolP.ppVrf params)
          , DB.poolUpdatePledge = Generic.coinToDbLovelace (PoolP.ppPledge params)
          , DB.poolUpdateRewardAddrId = saId
          , DB.poolUpdateActiveEpochNo = epoch + epochActivationDelay
          , DB.poolUpdateMetaId = mdId
          , DB.poolUpdateMargin = realToFrac $ Ledger.unboundRational (PoolP.ppMargin params)
          , DB.poolUpdateFixedCost = Generic.coinToDbLovelace (PoolP.ppCost params)
          , DB.poolUpdateDeposit = deposit
          , DB.poolUpdateRegisteredTxId = txId
          }

  mapM_ (insertPoolOwner syncEnv network poolUpdateId) $ toList (PoolP.ppOwners params)
  mapM_ (insertPoolRelay poolUpdateId) $ toList (PoolP.ppRelays params)
  where
    isPoolRegistration :: DB.PoolHashId -> ExceptT SyncNodeError DB.DbM Bool
    isPoolRegistration poolHashId =
      if isMember (PoolP.ppId params)
        then pure False
        else do
          -- if the pool is not registered at the end of the previous block, check for
          -- other registrations at the current block. If this is the first registration
          -- then it's +2, else it's +3.
          otherUpdates <- lift $ DB.queryPoolUpdateByBlock blkId poolHashId
          pure $ not otherUpdates

    -- Ignore the network in the `RewardAccount` and use the provided one instead.
    -- This is a workaround for https://github.com/IntersectMBO/cardano-db-sync/issues/546
    adjustNetworkTag :: Ledger.RewardAccount -> Ledger.RewardAccount
    adjustNetworkTag (Shelley.RewardAccount _ cred) = Shelley.RewardAccount network cred

insertPoolRetire ::
  SyncEnv ->
  DB.TxId ->
  EpochNo ->
  Word16 ->
  Ledger.KeyHash 'Ledger.StakePool ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolRetire syncEnv txId epochNum idx keyHash = do
  poolId <- queryPoolKeyOrInsert syncEnv "insertPoolRetire" UpdateCache True keyHash
  void . lift $
    DB.insertPoolRetire $
      DB.PoolRetire
        { DB.poolRetireHashId = poolId
        , DB.poolRetireCertIndex = idx
        , DB.poolRetireAnnouncedTxId = txId
        , DB.poolRetireRetiringEpoch = unEpochNo epochNum
        }

insertPoolMetaDataRef ::
  DB.PoolHashId ->
  DB.TxId ->
  PoolP.PoolMetadata ->
  ExceptT SyncNodeError DB.DbM DB.PoolMetadataRefId
insertPoolMetaDataRef poolId txId md =
  lift $
    DB.insertPoolMetadataRef $
      DB.PoolMetadataRef
        { DB.poolMetadataRefPoolId = poolId
        , DB.poolMetadataRefUrl = PoolUrl $ Ledger.urlToText (PoolP.pmUrl md)
        , DB.poolMetadataRefHash = PoolP.pmHash md
        , DB.poolMetadataRefRegisteredTxId = txId
        }

insertPoolOwner ::
  SyncEnv ->
  Ledger.Network ->
  DB.PoolUpdateId ->
  Ledger.KeyHash 'Ledger.Staking ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolOwner syncEnv network poolUpdateId skh = do
  saId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong network (Ledger.KeyHashObj skh)
  void . lift $
    DB.insertPoolOwner $
      DB.PoolOwner
        { DB.poolOwnerAddrId = saId
        , DB.poolOwnerPoolUpdateId = poolUpdateId
        }

insertPoolRelay ::
  DB.PoolUpdateId ->
  PoolP.StakePoolRelay ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolRelay updateId relay =
  void
    . lift
    $ DB.insertPoolRelay
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
  SyncEnv ->
  IsPoolMember ->
  Maybe Generic.Deposits ->
  Ledger.Network ->
  EpochNo ->
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  PoolCert ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolCert syncEnv isMember mdeposits network epoch blkId txId idx pCert =
  case pCert of
    RegPool pParams -> insertPoolRegister syncEnv isMember mdeposits network epoch blkId txId idx pParams
    RetirePool keyHash epochNum -> insertPoolRetire syncEnv txId epochNum idx keyHash
