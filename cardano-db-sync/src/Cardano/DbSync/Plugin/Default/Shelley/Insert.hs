{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.Shelley.Insert
  ( insertShelleyBlock
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT, runExceptT)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Plugin.Default.Shelley.Query
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text

import           Ouroboros.Network.Block (BlockNo (..), Tip)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley


insertShelleyBlock
    :: Trace IO Text -> ShelleyBlock -> Tip ShelleyBlock
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertShelleyBlock tracer blk tip = do
  runExceptT $ do
    meta <- liftLookupFail "insertShelleyBlock" DB.queryMeta

    pbid <- liftLookupFail "insertShelleyBlock" $ DB.queryBlockId (Shelley.blockPrevHash blk)

    let slotsPerEpoch = DB.metaSlotsPerEpoch meta

    -- liftIO . logInfo tracer $ "insertShelleyBlock " <> textShow (BS.length $ Shelley.blockHash blk)

    slid <- lift . DB.insertSlotLeader $ Shelley.mkSlotLeader blk
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Shelley.blockHash blk
                    , DB.blockEpochNo = Just $ Shelley.slotNumber blk `div` slotsPerEpoch
                    , DB.blockSlotNo = Just $ Shelley.slotNumber blk
                    , DB.blockBlockNo = Just $ Shelley.blockNumber blk
                    , DB.blockPrevious  = Just pbid
                    , DB.blockMerkelRoot = Nothing -- Shelley blocks do not have one.
                    , DB.blockSlotLeader = slid
                    , DB.blockSize = Shelley.blockSize blk
                    , DB.blockTime = DB.slotUtcTime meta (Shelley.slotNumber blk)
                    , DB.blockTxCount = Shelley.blockTxCount blk

                    -- Shelley specific
                    , DB.blockVrfKey = Nothing
                    , DB.blockNonceVrf = Nothing
                    , DB.blockLeaderVrf = Nothing
                    , DB.blockOpCert = Nothing
                    , DB.blockProtoVersion = Nothing
                    }

    zipWithM_ (insertTx tracer blkId) [0 .. ] (Shelley.blockTxs blk)

    liftIO $ do
      let epoch = Shelley.slotNumber blk `div` slotsPerEpoch
      logger tracer $ mconcat
        [ "insertShelleyBlock: epoch ", textShow epoch
        , ", slot ", textShow (Shelley.slotNumber blk)
        , ", block ", textShow (Shelley.blockNumber blk)
        , ", hash ", renderByteArray (Shelley.blockHash blk)
        ]
  where
    logger :: Trace IO a -> a -> IO ()
    logger
      | unBlockNo (tipBlockNo tip) - Shelley.blockNumber blk < 20 = logInfo
      | Shelley.slotNumber blk `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

-- -----------------------------------------------------------------------------

insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> Word64 -> ShelleyTx
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkId blockIndex tx = do
    -- Insert transaction and get txId from the DB.
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Shelley.txHash tx
                , DB.txBlock = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = Shelley.txOutputSum tx
                , DB.txFee = Shelley.txFee tx
                , DB.txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    mapM_ (insertTxOut tracer txId) (Shelley.txOutputList tx)

    -- Insert the transaction inputs.
    mapM_ (insertTxIn tracer txId) (Shelley.txInputList tx)

    mapM_ (insertPoolCert tracer txId) (Shelley.txPoolCertificates $ Shelley._body tx)
    mapM_ (insertDelegCert tracer txId) (Shelley.txDelegationCerts $ Shelley._body tx)
    mapM_ (insertMirCert tracer txId) (Shelley.txMirCertificates $ Shelley._body tx)
    mapM_ (insertWithdrawals tracer txId) (Map.toList . Shelley.unWdrl . Shelley._wdrls $ Shelley._body tx)


insertTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> (Word16, ShelleyTxOut)
    -> ExceptT e (ReaderT SqlBackend m) ()
insertTxOut _tracer txId (index, Shelley.TxOut addr value) =
  void . lift . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = index
              , DB.txOutAddress = Text.decodeUtf8 $ Base16.encode (Shelley.serialiseAddr addr)
              , DB.txOutValue = fromIntegral value
              }

insertTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyTxIn
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId (Shelley.TxIn txId index) = do
  txOutId <- liftLookupFail "insertTxIn" $ DB.queryTxId (Shelley.unTxHash txId)
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral index
              }

insertPoolCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyPoolCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolCert tracer txId pCert =
  case pCert of
    Shelley.RegPool pParams -> insertPoolRegister tracer txId pParams
    Shelley.RetirePool keyHash epochNum -> insertPoolRetire txId epochNum keyHash

insertDelegCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyDelegCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer txId dCert =
  case dCert of
    Shelley.RegKey cred -> insertStakeRegistration tracer txId cred
    Shelley.DeRegKey cred -> insertStakeDeregistration tracer txId cred
    Shelley.Delegate (Shelley.Delegation cred poolkh) -> insertDelegation tracer txId cred poolkh


insertPoolRegister
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyPoolParams
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister tracer txId params = do
  mdId <- case strictMaybeToMaybe $ Shelley._poolMD params of
            Just md -> Just <$> insertMetaData txId md
            Nothing -> pure Nothing
  rewardId <- insertStakeAddress $ Shelley.serialiseRewardAcnt (Shelley._poolRAcnt params)

  when (Shelley.unCoin (Shelley._poolPledge params) > maxLovelace) $
    liftIO . logError tracer $
      mconcat
        [ "Bad pledge amount: ", textShow (Shelley.unCoin $ Shelley._poolPledge params)
        , " > maxLovelace. See https://github.com/input-output-hk/cardano-ledger-specs/issues/1551"
        ]

  poolId <- lift . DB.insertPool $
              DB.Pool
                { DB.poolHash = Shelley.unKeyHashBS (Shelley._poolPubKey params)
                , DB.poolPledge = Shelley.unCoin $ Shelley._poolPledge params
                , DB.poolRewardAddrId = rewardId
                , DB.poolMeta = mdId
                , DB.poolMargin = realToFrac $ Shelley.intervalValue (Shelley._poolMargin params)
                , DB.poolFixedCost = Shelley.unCoin (Shelley._poolCost params)
                , DB.poolRegisteredTxId = txId
                }

  mapM_ (insertPoolOwner poolId) $ toList (Shelley._poolOwners params)
  mapM_ (insertPoolRelay poolId) $ toList (Shelley._poolRelays params)

maxLovelace :: Word64
maxLovelace = 45000000000000000

insertPoolRetire
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> EpochNo ->  ShelleyStakePoolKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire txId epochNum keyHash = do
  poolId <- firstExceptT (NELookup "insertPoolRetire") . newExceptT $ queryStakePoolKeyHash keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetirePoolId = poolId
      , DB.poolRetireAnnouncedTxId = txId
      , DB.poolRetireRetiringEpoch = unEpochNo epochNum
      }


insertMetaData
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> Shelley.PoolMetaData
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) DB.PoolMetaDataId
insertMetaData txId md =
  lift . DB.insertPoolMetaData $
    DB.PoolMetaData
      { DB.poolMetaDataUrl = Shelley.urlToText (Shelley._poolMDUrl md)
      , DB.poolMetaDataHash = Shelley._poolMDHash md
      , DB.poolMetaDataTxId = txId
      }

insertStakeAddress
    :: (MonadBaseControl IO m, MonadIO m)
    => ByteString
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) DB.StakeAddressId
insertStakeAddress stakeAddr =
  lift . DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHash = fixStakingAddr stakeAddr -- TODO: is fixStakingAddr the right approach?
      }

insertPoolOwner
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolId -> ShelleyStakingKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner poolId skh =
  void . lift . DB.insertPoolOwner $
    DB.PoolOwner
      { DB.poolOwnerHash = Shelley.unKeyHashBS skh
      , DB.poolOwnerPoolId = poolId
      }

insertStakeRegistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyStakingCred
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration _tracer txId cred = do
  scId <- insertStakeAddress $ Shelley.stakingCredHash cred
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = scId
      , DB.stakeRegistrationTxId = txId
      }

insertStakeDeregistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyStakingCred
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration _tracer txId cred = do
  scId <- firstExceptT (NELookup "insertStakeDeregistration")
            . newExceptT
            $ queryStakeAddress (fixStakingAddr $ Shelley.stakingCredHash cred)
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = scId
      , DB.stakeRegistrationTxId = txId
      }

insertDelegation
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyStakingCred -> ShelleyStakePoolKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertDelegation _tracer txId cred poolkh = do
  addrId <- firstExceptT (NELookup "insertDelegation")
                . newExceptT
                $ queryStakeAddress (fixStakingAddr $ Shelley.stakingCredHash cred)
  poolId <- firstExceptT (NELookup "insertDelegation")
                . newExceptT
                $ queryStakePoolKeyHash poolkh
  void . lift . DB.insertDelegation $
    DB.Delegation
      { DB.delegationAddrId = addrId
      , DB.delegationPoolId = poolId
      , DB.delegationTxId = txId
      }

insertMirCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyMIRCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertMirCert tracer txId mcert = do
    case Shelley.mirPot mcert of
      Shelley.ReservesMIR ->
        mapM_ insertMirReserves $ Map.toList (Shelley.mirRewards mcert)
      Shelley.TreasuryMIR ->
        mapM_ insertMirTreasury $ Map.toList (Shelley.mirRewards mcert)
  where
    insertMirReserves
        :: (MonadBaseControl IO m, MonadIO m)
        => (ShelleyStakingCred, Shelley.Coin)
        -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    insertMirReserves (cred, coin) = do
      addrId <- firstExceptT (NELookup "insertMirReserves")
                    . newExceptT
                    $ queryStakeAddress (fixStakingAddr $ Shelley.stakingCredHash cred)
      void . lift . DB.insertReward $
        DB.Reward
          { DB.rewardAddrId = addrId
          , DB.rewardTxId = txId
          , DB.rewardAmount = Shelley.unCoin coin
          }

    insertMirTreasury
        :: MonadIO m -- (MonadBaseControl IO m, MonadIO m)
        => (ShelleyStakingCred, Shelley.Coin)
        -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    insertMirTreasury _ =
      liftIO $ logError tracer "insertMirTreasury: Not handled yet"

insertWithdrawals
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> (ShelleyRewardAccount, Shelley.Coin)
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertWithdrawals _tracer txId (account, coin) = do
  addrId <- firstExceptT (NELookup "insertWithdrawals")
                . newExceptT
                $ queryStakeAddress (fixStakingAddr $ Shelley.serialiseRewardAcnt account) -- FIXME: fixStakingAddr
  void . lift . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = Shelley.unCoin coin
      }

insertPoolRelay
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolId -> Shelley.StakePoolRelay
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRelay poolId relay =
  void . lift . DB.insertPoolRelay $
    case relay of
      Shelley.SingleHostAddr mPort mIpv4 mIpv6 ->
        DB.PoolRelay -- An IPv4 and/or IPv6 address
          { DB.poolRelayPoolId = poolId
          , DB.poolRelayIpv4 = textShow <$> Shelley.strictMaybeToMaybe mIpv4
          , DB.poolRelayIpv6 = textShow <$> Shelley.strictMaybeToMaybe mIpv6
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Shelley.portToWord16 <$> Shelley.strictMaybeToMaybe mPort
          }
      Shelley.SingleHostName mPort name ->
        DB.PoolRelay -- An A or AAAA DNS record
          { DB.poolRelayPoolId = poolId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Just (Shelley.dnsToText name)
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Shelley.portToWord16 <$> Shelley.strictMaybeToMaybe mPort
          }
      Shelley.MultiHostName name ->
        DB.PoolRelay -- An SRV DNS record
          { DB.poolRelayPoolId = poolId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Just (Shelley.dnsToText name)
          , DB.poolRelayPort = Nothing
          }

-- -------------------------------------------------------------------------------------------------

-- | StakeAddress values in the PoolParam are 29 bytes long while addresses in
-- ShelleyStakingCred are 28 bytes long. We therefore test the length of the ByteString
-- and drop the first byte if the length is 29 bytes in length.
-- This leading byte contains the network id and something that discriminates between
-- a KeyHash or a ScriptHash. It will need to be handled at some point.
fixStakingAddr :: ByteString -> ByteString
fixStakingAddr bs =
  if BS.length bs == 29
    then BS.tail bs
    else bs
