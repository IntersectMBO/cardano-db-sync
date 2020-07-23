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

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Plugin.Default.Shelley.Query
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley


insertShelleyBlock
    :: Trace IO Text -> DbSyncEnv -> ShelleyBlock TPraosStandardCrypto -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertShelleyBlock tracer env blk details = do
  runExceptT $ do
    pbid <- liftLookupFail "insertShelleyBlock" $ DB.queryBlockId (Shelley.blockPrevHash blk)
    mPhid <- lift $ queryPoolHashId (Shelley.blockVrfKeyToPoolHash blk)

    slid <- lift . DB.insertSlotLeader $ Shelley.mkSlotLeader blk mPhid
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Shelley.blockHash blk
                    , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
                    , DB.blockSlotNo = Just $ Shelley.slotNumber blk
                    , DB.blockBlockNo = Just $ Shelley.blockNumber blk
                    , DB.blockPrevious  = Just pbid
                    , DB.blockMerkelRoot = Nothing -- Shelley blocks do not have one.
                    , DB.blockSlotLeader = slid
                    , DB.blockSize = Shelley.blockSize blk
                    , DB.blockTime = sdTime details
                    , DB.blockTxCount = Shelley.blockTxCount blk

                    -- Shelley specific
                    , DB.blockVrfKey = Just $ Shelley.blockVrfKey (Shelley.blockBody blk)
                    , DB.blockOpCert = Just $ Shelley.blockOpCert (Shelley.blockBody blk)
                    , DB.blockProtoVersion = Just $ Shelley.blockProtoVersion (Shelley.blockBody blk)
                    }

    zipWithM_ (insertTx tracer env blkId) [0 .. ] (Shelley.blockTxs blk)

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unSlotInEpoch (sdSlotInEpoch details)
      followingClosely <- DB.isFullySynced (sdTime details)

      when (followingClosely && slotWithinEpoch /= 0 && Shelley.slotNumber blk `mod` 200 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertShelleyBlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch , "/"
            , textShow (unEpochSize $ sdEpochSize details), ")"
            ]
      logger followingClosely tracer $ mconcat
        [ "insertShelleyBlock: epoch ", textShow (unEpochNo $ sdEpochNo details)
        , ", slot ", textShow (Shelley.slotNumber blk)
        , ", block ", textShow (Shelley.blockNumber blk)
        , ", hash ", renderByteArray (Shelley.blockHash blk)
        ]

  where
    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | followingClosely = logInfo
      | Shelley.slotNumber blk `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

-- -----------------------------------------------------------------------------

insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.BlockId -> Word64 -> ShelleyTx
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer env blkId blockIndex tx = do
    -- Insert transaction and get txId from the DB.
    let outSum = Shelley.txOutputSum tx
        fees = Shelley.txFee tx
    inSum <- lift $ queryTxInputSum (Shelley.txInputList tx)
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Shelley.txHash tx
                , DB.txBlock = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = outSum
                , DB.txFee = fees
                , DB.txDeposit = fromIntegral inSum - fromIntegral (outSum + fees)
                , DB.txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    mapM_ (insertTxOut tracer txId) (Shelley.txOutputList tx)

    -- Insert the transaction inputs.
    mapM_ (insertTxIn tracer txId) (Shelley.txInputList tx)

    mapM_ (insertPoolCert tracer txId) (Shelley.txPoolCertificates $ Shelley._body tx)
    mapM_ (insertDelegCert tracer env txId) (Shelley.txDelegationCerts $ Shelley._body tx)
    mapM_ (insertMirCert tracer env txId) (Shelley.txMirCertificates $ Shelley._body tx)
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
              , DB.txOutAddress = Shelley.renderAddress addr
              , DB.txOutValue = fromIntegral $ Shelley.unCoin value
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
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> ShelleyDelegCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer env txId dCert =
  case dCert of
    Shelley.RegKey cred -> insertStakeRegistration tracer env txId cred
    Shelley.DeRegKey cred -> insertStakeDeregistration tracer env txId cred
    Shelley.Delegate (Shelley.Delegation cred poolkh) -> insertDelegation tracer env txId cred poolkh


insertPoolRegister
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ShelleyPoolParams
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister tracer txId params = do
  mdId <- case strictMaybeToMaybe $ Shelley._poolMD params of
            Just md -> Just <$> insertMetaData txId md
            Nothing -> pure Nothing
  rewardId <- insertStakeAddress $ Shelley.serialiseRewardAcnt (Shelley._poolRAcnt params)

  when (fromIntegral (Shelley.unCoin $ Shelley._poolPledge params) > maxLovelace) $
    liftIO . logError tracer $
      mconcat
        [ "Bad pledge amount: ", textShow (Shelley.unCoin $ Shelley._poolPledge params)
        , " > maxLovelace. See https://github.com/input-output-hk/cardano-ledger-specs/issues/1551"
        ]

  poolHashId <- lift . DB.insertPoolHash $ DB.PoolHash (Shelley.unKeyHashBS $ Shelley._poolPubKey params)
  poolUpdateId <- lift . DB.insertPoolUpdate $
                    DB.PoolUpdate
                      { DB.poolUpdateHashId = poolHashId
                      , DB.poolUpdateVrfKey = Crypto.hashToBytes (Shelley._poolVrf params)
                      , DB.poolUpdatePledge = fromIntegral $ Shelley.unCoin (Shelley._poolPledge params)
                      , DB.poolUpdateRewardAddrId = rewardId
                      , DB.poolUpdateMeta = mdId
                      , DB.poolUpdateMargin = realToFrac $ Shelley.intervalValue (Shelley._poolMargin params)
                      , DB.poolUpdateFixedCost = fromIntegral $ Shelley.unCoin (Shelley._poolCost params)
                      , DB.poolUpdateRegisteredTxId = txId
                      }

  mapM_ (insertPoolOwner poolHashId) $ toList (Shelley._poolOwners params)
  mapM_ (insertPoolRelay poolUpdateId) $ toList (Shelley._poolRelays params)

maxLovelace :: Word64
maxLovelace = 45000000000000000

insertPoolRetire
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> EpochNo ->  ShelleyStakePoolKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire txId epochNum keyHash = do
  updateId <- firstExceptT (NELookup "insertPoolRetire") . newExceptT $ queryStakePoolKeyHash keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetireUpdateId = updateId
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
      , DB.poolMetaDataRegisteredTxId = txId
      }

insertStakeAddress
    :: (MonadBaseControl IO m, MonadIO m)
    => ByteString
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) DB.StakeAddressId
insertStakeAddress stakeAddr =
  lift . DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHash = stakeAddr
      }

insertPoolOwner
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolHashId -> ShelleyStakingKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner poolId skh =
  void . lift . DB.insertPoolOwner $
    DB.PoolOwner
      { DB.poolOwnerHash = Shelley.unKeyHashBS skh
      , DB.poolOwnerPoolId = poolId
      }

insertStakeRegistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> ShelleyStakingCred
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration _tracer env txId cred = do
  scId <- insertStakeAddress $ Shelley.stakingCredHash env cred
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = scId
      , DB.stakeRegistrationTxId = txId
      }

insertStakeDeregistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> ShelleyStakingCred
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration _tracer env txId cred = do
  scId <- firstExceptT (NELookup "insertStakeDeregistration")
            . newExceptT
            $ queryStakeAddress (Shelley.stakingCredHash env cred)
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = scId
      , DB.stakeRegistrationTxId = txId
      }

insertDelegation
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> ShelleyStakingCred -> ShelleyStakePoolKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertDelegation _tracer env txId cred poolkh = do
  addrId <- firstExceptT (NELookup "insertDelegation")
                . newExceptT
                $ queryStakeAddress (Shelley.stakingCredHash env cred)
  updateId <- firstExceptT (NELookup "insertDelegation")
                . newExceptT
                $ queryStakePoolKeyHash poolkh
  void . lift . DB.insertDelegation $
    DB.Delegation
      { DB.delegationAddrId = addrId
      , DB.delegationUpdateId = updateId
      , DB.delegationTxId = txId
      }

insertMirCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> ShelleyMIRCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertMirCert tracer env txId mcert = do
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
                    $ queryStakeAddress (Shelley.stakingCredHash env cred)
      void . lift . DB.insertReward $
        DB.Reward
          { DB.rewardAddrId = addrId
          , DB.rewardTxId = txId
          , DB.rewardAmount = fromIntegral $ Shelley.unCoin coin
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
                $ queryStakeAddress (Shelley.serialiseRewardAcnt account)
  void . lift . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = fromIntegral $ Shelley.unCoin coin
      }

insertPoolRelay
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolUpdateId -> Shelley.StakePoolRelay
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRelay updateId relay =
  void . lift . DB.insertPoolRelay $
    case relay of
      Shelley.SingleHostAddr mPort mIpv4 mIpv6 ->
        DB.PoolRelay -- An IPv4 and/or IPv6 address
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = textShow <$> Shelley.strictMaybeToMaybe mIpv4
          , DB.poolRelayIpv6 = textShow <$> Shelley.strictMaybeToMaybe mIpv6
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Shelley.portToWord16 <$> Shelley.strictMaybeToMaybe mPort
          }
      Shelley.SingleHostName mPort name ->
        DB.PoolRelay -- An A or AAAA DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Just (Shelley.dnsToText name)
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Shelley.portToWord16 <$> Shelley.strictMaybeToMaybe mPort
          }
      Shelley.MultiHostName name ->
        DB.PoolRelay -- An SRV DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Just (Shelley.dnsToText name)
          , DB.poolRelayPort = Nothing
          }
