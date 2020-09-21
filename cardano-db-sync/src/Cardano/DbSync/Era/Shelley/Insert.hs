{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Insert
  ( insertShelleyBlock
  , containsUnicodeNul
  , safeDecodeUtf8
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo, logWarning)

import           Cardano.Db (DbWord64 (..))

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT, runExceptT)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Cardano.Api.MetaData (jsonFromMetadataValue)

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.MetaData as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley

insertShelleyBlock
    :: Trace IO Text -> DbSyncEnv -> ShelleyBlock -> SlotDetails
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
                    , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
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
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
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
    let fees = Shelley.txFee tx
        outSum = Shelley.txOutputSum tx
        withdrawalSum = Shelley.txWithdrawalSum tx
    inSum <- lift $ queryTxInputSum (Shelley.txInputList tx)
    -- Insert transaction and get txId from the DB.
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Shelley.txHash tx
                , DB.txBlock = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = outSum
                , DB.txFee = fees
                , DB.txDeposit = fromIntegral (inSum + withdrawalSum) - fromIntegral (outSum + fees)
                , DB.txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    mapM_ (insertTxOut tracer txId) (Shelley.txOutputList tx)

    -- Insert the transaction inputs.
    mapM_ (insertTxIn tracer txId) (Shelley.txInputList tx)

    case Shelley.txMetadata tx of
      Nothing -> pure ()
      Just md -> insertTxMetadata tracer txId md

    mapM_ (insertCertificate tracer env txId) $ Shelley.txCertificates tx
    mapM_ (insertWithdrawals tracer txId) $ Shelley.txWithdrawals tx

    case Shelley.txParamUpdate tx of
      Nothing -> pure ()
      Just pu -> insertParamUpdate tracer txId pu

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
              , DB.txOutAddressRaw = Shelley.serialiseAddr addr
              , DB.txOutPaymentCred = Shelley.maybePaymentCred addr
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

insertCertificate
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> (Word16, ShelleyDCert)
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertCertificate tracer env txId (idx, cert) =
  case cert of
    Shelley.DCertDeleg deleg -> insertDelegCert tracer env txId idx deleg
    Shelley.DCertPool pool -> insertPoolCert tracer txId idx pool
    Shelley.DCertMir mir -> insertMirCert tracer env txId idx mir
    Shelley.DCertGenesis _gen -> do
        -- TODO : Low priority
        liftIO $ logError tracer "insertCertificate: Unhandled DCertGenesis certificate"
        pure ()


insertPoolCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Word16 -> ShelleyPoolCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolCert tracer txId idx pCert =
  case pCert of
    Shelley.RegPool pParams -> insertPoolRegister tracer txId idx pParams
    Shelley.RetirePool keyHash epochNum -> insertPoolRetire txId epochNum idx keyHash

insertDelegCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> Word16 -> ShelleyDelegCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer env txId idx dCert =
  case dCert of
    Shelley.RegKey cred -> insertStakeRegistration tracer txId idx $ Shelley.annotateStakingCred env cred
    Shelley.DeRegKey cred -> insertStakeDeregistration tracer env txId idx cred
    Shelley.Delegate (Shelley.Delegation cred poolkh) -> insertDelegation tracer env txId idx cred poolkh


insertPoolRegister
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Word16 -> ShelleyPoolParams
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister tracer txId idx params = do
  mdId <- case strictMaybeToMaybe $ Shelley._poolMD params of
            Just md -> Just <$> insertMetaData txId md
            Nothing -> pure Nothing
  rewardId <- insertStakeAddress txId $ Shelley._poolRAcnt params

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
                      , DB.poolUpdateCertIndex = idx
                      , DB.poolUpdateVrfKey = Crypto.hashToBytes (Shelley._poolVrf params)
                      , DB.poolUpdatePledge = DbWord64 $ fromIntegral (Shelley.unCoin $ Shelley._poolPledge params)
                      , DB.poolUpdateRewardAddrId = rewardId
                      , DB.poolUpdateMeta = mdId
                      , DB.poolUpdateMargin = realToFrac $ Shelley.intervalValue (Shelley._poolMargin params)
                      , DB.poolUpdateFixedCost = fromIntegral $ Shelley.unCoin (Shelley._poolCost params)
                      , DB.poolUpdateRegisteredTxId = txId
                      }

  mapM_ (insertPoolOwner poolHashId txId) $ toList (Shelley._poolOwners params)
  mapM_ (insertPoolRelay poolUpdateId) $ toList (Shelley._poolRelays params)

maxLovelace :: Word64
maxLovelace = 45000000000000000

insertPoolRetire
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> EpochNo -> Word16 -> ShelleyStakePoolKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire txId epochNum idx keyHash = do
  poolId <- firstExceptT (NELookup "insertPoolRetire") . newExceptT $ queryStakePoolKeyHash keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetireHashId = poolId
      , DB.poolRetireCertIndex = idx
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
    => DB.TxId -> Shelley.RewardAcnt StandardShelley
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) DB.StakeAddressId
insertStakeAddress txId rewardAddr =
  lift . DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHashRaw = Shelley.serialiseRewardAcnt rewardAddr
      , DB.stakeAddressView = Shelley.renderRewardAcnt rewardAddr
      , DB.stakeAddressRegisteredTxId = txId
      }

insertPoolOwner
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolHashId -> DB.TxId -> ShelleyStakingKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner poolHashId txId skh =
  void . lift . DB.insertPoolOwner $
    DB.PoolOwner
      { DB.poolOwnerHash = Shelley.unKeyHashBS skh
      , DB.poolOwnerPoolHashId = poolHashId
      , DB.poolOwnerRegisteredTxId = txId
      }

insertStakeRegistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Word16 -> Shelley.RewardAcnt StandardShelley
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration _tracer txId idx rewardAccount = do
  scId <- insertStakeAddress txId rewardAccount
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = scId
      , DB.stakeRegistrationCertIndex = idx
      , DB.stakeRegistrationTxId = txId
      }

insertStakeDeregistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> Word16 -> ShelleyStakingCred
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration _tracer env txId idx cred = do
  scId <- firstExceptT (NELookup "insertStakeDeregistration")
            . newExceptT
            $ queryStakeAddress (Shelley.stakingCredHash env cred)
  void . lift . DB.insertStakeDeregistration $
    DB.StakeDeregistration
      { DB.stakeDeregistrationAddrId = scId
      , DB.stakeDeregistrationCertIndex = idx
      , DB.stakeDeregistrationTxId = txId
      }

insertDelegation
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> Word16 -> ShelleyStakingCred -> ShelleyStakePoolKeyHash
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertDelegation _tracer env txId idx cred poolkh = do
  addrId <- firstExceptT (NELookup "insertDelegation")
                . newExceptT
                $ queryStakeAddress (Shelley.stakingCredHash env cred)
  poolHashId <- firstExceptT (NELookup "insertDelegation")
                  . newExceptT
                  $ queryStakePoolKeyHash poolkh
  void . lift . DB.insertDelegation $
    DB.Delegation
      { DB.delegationAddrId = addrId
      , DB.delegationCertIndex = idx
      , DB.delegationPoolHashId = poolHashId
      , DB.delegationTxId = txId
      }

insertMirCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DbSyncEnv -> DB.TxId -> Word16 -> ShelleyMIRCert
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertMirCert _tracer env txId idx mcert = do
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
      addrId <- insertStakeAddress txId $ Shelley.annotateStakingCred env cred
      void . lift . DB.insertReserve $
        DB.Reserve
          { DB.reserveAddrId = addrId
          , DB.reserveCertIndex = idx
          , DB.reserveTxId = txId
          , DB.reserveAmount = fromIntegral $ Shelley.unCoin coin
          }

    insertMirTreasury
        :: (MonadBaseControl IO m, MonadIO m)
        => (ShelleyStakingCred, Shelley.Coin)
        -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    insertMirTreasury (cred, coin) = do
      addrId <- insertStakeAddress txId $ Shelley.annotateStakingCred env cred
      void . lift . DB.insertTreasury $
        DB.Treasury
          { DB.treasuryAddrId = addrId
          , DB.treasuryCertIndex = idx
          , DB.treasuryTxId = txId
          , DB.treasuryAmount = fromIntegral $ Shelley.unCoin coin
          }

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

insertParamUpdate
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Shelley.Update StandardShelley
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertParamUpdate _tracer txId (Shelley.Update (Shelley.ProposedPPUpdates umap) (EpochNo epoch)) =
    mapM_ insert $ Map.toList umap
  where
    insert
      :: forall r m. (MonadBaseControl IO m, MonadIO m)
      => (Shelley.KeyHash r StandardShelley, Shelley.PParams' Shelley.StrictMaybe)
      -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    insert (key, pmap) =
      void . lift . DB.insertParamUpdate $
        DB.ParamUpdate
          { DB.paramUpdateEpochNo = epoch
          , DB.paramUpdateKey = Shelley.unKeyHashBS key
          , DB.paramUpdateMinFeeA = fromIntegral <$> strictMaybeToMaybe (Shelley._minfeeA pmap)
          , DB.paramUpdateMinFeeB = fromIntegral <$> strictMaybeToMaybe (Shelley._minfeeB pmap)
          , DB.paramUpdateMaxBlockSize = fromIntegral <$> strictMaybeToMaybe (Shelley._maxBBSize pmap)
          , DB.paramUpdateMaxTxSize = fromIntegral <$> strictMaybeToMaybe (Shelley._maxTxSize pmap)
          , DB.paramUpdateMaxBhSize = fromIntegral <$> strictMaybeToMaybe (Shelley._maxBHSize pmap)
          , DB.paramUpdateKeyDeposit = fromIntegral . Shelley.unCoin <$> strictMaybeToMaybe (Shelley._keyDeposit pmap)
          , DB.paramUpdatePoolDeposit = fromIntegral . Shelley.unCoin <$> strictMaybeToMaybe (Shelley._poolDeposit pmap)
          , DB.paramUpdateMaxEpoch = unEpochNo <$> strictMaybeToMaybe (Shelley._eMax pmap)
          , DB.paramUpdateOptimalPoolCount = fromIntegral <$> strictMaybeToMaybe (Shelley._nOpt pmap)
          , DB.paramUpdateInfluence = fromRational <$> strictMaybeToMaybe (Shelley._a0 pmap)
          , DB.paramUpdateMonetaryExpandRate = Shelley.unitIntervalToDouble <$> strictMaybeToMaybe (Shelley._rho pmap)
          , DB.paramUpdateTreasuryGrowthRate = Shelley.unitIntervalToDouble <$> strictMaybeToMaybe (Shelley._tau pmap)
          , DB.paramUpdateDecentralisation = Shelley.unitIntervalToDouble <$> strictMaybeToMaybe (Shelley._d pmap)
          , DB.paramUpdateEntropy = Shelley.nonceToBytes <$> strictMaybeToMaybe (Shelley._extraEntropy pmap)
          , DB.paramUpdateProtocolVersion = textShow <$> strictMaybeToMaybe (Shelley._protocolVersion pmap)
          , DB.paramUpdateMinUTxOValue = fromIntegral . Shelley.unCoin <$> strictMaybeToMaybe (Shelley._minUTxOValue pmap)
          , DB.paramUpdateMinPoolCost = fromIntegral . Shelley.unCoin <$> strictMaybeToMaybe (Shelley._minPoolCost pmap)
          , DB.paramUpdateRegisteredTxId = txId
          }

insertTxMetadata
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Shelley.MetaData
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTxMetadata tracer txId (Shelley.MetaData mdmap) =
    mapM_ insert $ Map.toList mdmap
  where
    insert
        :: (MonadBaseControl IO m, MonadIO m)
        => (Word64, Shelley.MetaDatum)
        -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    insert (key, md) = do
      let jsonbs = LBS.toStrict $ Aeson.encode (jsonFromMetadataValue md)
      ejson <- liftIO $ safeDecodeUtf8 jsonbs
      case ejson of
        Left err ->
          liftIO . logWarning tracer $ mconcat
            [ "insertTxMetadata: Could not decode to UTF8: ", textShow err ]
        Right json -> do
          -- See https://github.com/input-output-hk/cardano-db-sync/issues/297
          if containsUnicodeNul json
            then liftIO $ logWarning tracer "insertTxMetadata: dropped due to a Unicode NUL character."
            else
              void . lift . DB.insertTxMetadata $
                DB.TxMetadata
                  { DB.txMetadataKey = DbWord64 key
                  , DB.txMetadataJson = json
                  , DB.txMetadataTxId = txId
                  }

safeDecodeUtf8 :: ByteString -> IO (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
    | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
    | otherwise = try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0

containsUnicodeNul :: Text -> Bool
containsUnicodeNul = Text.isInfixOf "\\u000"
