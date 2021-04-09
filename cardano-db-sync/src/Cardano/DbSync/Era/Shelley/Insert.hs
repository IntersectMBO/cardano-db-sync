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

import           Cardano.Api (SerialiseAsCBOR (..))
import           Cardano.Api.Shelley (TxMetadataValue (..), makeTransactionMetadata,
                   metadataValueToJsonNoSchema)

import           Cardano.BM.Trace (Trace, logDebug, logInfo, logWarning)

import           Cardano.Db (DbLovelace (..), DbWord64 (..))

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Era.Util (liftLookupFail)

import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..), Value (..))

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import           Control.Monad.Extra (whenJust)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Group (invert)
import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import           Database.Persist.Sql (SqlBackend, putMany)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

insertShelleyBlock
    :: Trace IO Text -> Shelley.Network -> Generic.Block -> LedgerStateSnapshot -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
insertShelleyBlock tracer network blk lStateSnap details = do
  runExceptT $ do
    pbid <- liftLookupFail (renderInsertName (Generic.blkEra blk)) $ DB.queryBlockId (Generic.blkPreviousHash blk)
    mPhid <- lift $ queryPoolHashId (Generic.blkCreatorPoolHash blk)

    slid <- lift . DB.insertSlotLeader $ Generic.mkSlotLeader (Generic.blkSlotLeader blk) mPhid
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Generic.blkHash blk
                    , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
                    , DB.blockSlotNo = Just $ unSlotNo (Generic.blkSlotNo blk)
                    , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
                    , DB.blockBlockNo = Just $ unBlockNo (Generic.blkBlockNo blk)
                    , DB.blockPreviousId  = Just pbid
                    , DB.blockSlotLeaderId = slid
                    , DB.blockSize = Generic.blkSize blk
                    , DB.blockTime = sdSlotTime details
                    , DB.blockTxCount = fromIntegral $ length (Generic.blkTxs blk)
                    , DB.blockProtoMajor = fromIntegral $ Shelley.pvMajor (Generic.blkProto blk)
                    , DB.blockProtoMinor = fromIntegral $ Shelley.pvMinor (Generic.blkProto blk)

                    -- Shelley specific
                    , DB.blockVrfKey = Just $ Generic.blkVrfKey blk
                    , DB.blockOpCert = Just $ Generic.blkOpCert blk
                    }

    zipWithM_ (insertTx tracer network blkId (sdEpochNo details) (Generic.blkSlotNo blk)) [0 .. ] (Generic.blkTxs blk)

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
          followingClosely = getSyncStatus details == SyncFollowing

      when (followingClosely && slotWithinEpoch /= 0 && unBlockNo (Generic.blkBlockNo blk) `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ renderInsertName (Generic.blkEra blk), ": continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch , "/"
            , textShow (unEpochSize $ sdEpochSize details), ")"
            ]
      logger followingClosely tracer $ mconcat
        [ renderInsertName (Generic.blkEra blk), ": epoch "
        , textShow (unEpochNo $ sdEpochNo details)
        , ", slot ", textShow (unSlotNo $ Generic.blkSlotNo blk)
        , ", block ", textShow (unBlockNo $ Generic.blkBlockNo blk)
        , ", hash ", renderByteArray (Generic.blkHash blk)
        ]

    whenJust (lssNewEpoch lStateSnap) $ \ newEpoch -> do
      insertOnNewEpoch tracer blkId (Generic.blkSlotNo blk) (sdEpochNo details) newEpoch

    when (getSyncStatus details == SyncFollowing) $
      -- Serializiing things during syncing can drastically slow down full sync
      -- times (ie 10x or more).
      lift DB.transactionCommit
  where
    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | followingClosely = logInfo
      | unBlockNo (Generic.blkBlockNo blk) `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

renderInsertName :: Generic.BlockEra -> Text
renderInsertName eraName =
  case eraName of
    Generic.Shelley -> "insertShelleyBlock"
    other -> mconcat [ "insertShelleyBlock(", textShow other, ")" ]

-- -----------------------------------------------------------------------------

insertOnNewEpoch
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> SlotNo -> EpochNo -> Generic.NewEpoch
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOnNewEpoch tracer blkId slotNo epochNo newEpoch = do
    whenJust (Generic.epochUpdate newEpoch) $ \esum -> do
      let stakes = Generic.euStakeDistribution esum

      whenJust (Generic.euRewards esum) $ \ grewards ->
        insertGenericRewards grewards stakes

      insertEpochParam tracer blkId epochNo (Generic.euProtoParams esum) (Generic.euNonce esum)
      insertEpochStake tracer epochNo stakes

    whenJust (Generic.adaPots newEpoch) $ \pots ->
      insertPots blkId slotNo epochNo pots
    liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo epochNo)
  where
    insertGenericRewards
        :: (MonadBaseControl IO m, MonadIO m)
        => Generic.Rewards -> Generic.StakeDist
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertGenericRewards grewards stakes = do
      liftIO . logInfo tracer $ mconcat
                [ "Finishing epoch ", textShow (unEpochNo epochNo - 1), ": "
                , textShow (length (Generic.rewards grewards)), " rewards, "
                , textShow (length (Generic.orphaned grewards)), " orphaned_rewards, "
                , textShow (length (Generic.unStakeDist stakes)), " stakes."
                ]

      -- Subtract 2 from the epoch to calculate when the epoch in which the reward was earned.
      insertRewards tracer (epochNo - 2) (Generic.rewards grewards)
      insertOrphanedRewards tracer (epochNo - 2) (Generic.orphaned grewards)

-- -----------------------------------------------------------------------------

insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> DB.BlockId -> EpochNo -> SlotNo -> Word64 -> Generic.Tx
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer network blkId epochNo slotNo blockIndex tx = do
    let fees = unCoin $ Generic.txFees tx
        outSum = unCoin $ Generic.txOutSum tx
        withdrawalSum = unCoin $ Generic.txWithdrawalSum tx
    inSum <- fromIntegral . unDbLovelace <$> lift (queryTxInputSum $ Generic.txInputs tx)
    -- Insert transaction and get txId from the DB.
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Generic.txHash tx
                , DB.txBlockId = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = DB.DbLovelace (fromIntegral outSum)
                , DB.txFee = DB.DbLovelace (fromIntegral . unCoin $ Generic.txFees tx)
                , DB.txDeposit = fromIntegral (inSum + withdrawalSum) - fromIntegral (outSum + fees)
                , DB.txSize = Generic.txSize tx
                , DB.txInvalidBefore = DbWord64 . unSlotNo <$> Generic.txInvalidBefore tx
                , DB.txInvalidHereafter = DbWord64 . unSlotNo <$> Generic.txInvalidHereafter tx
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    mapM_ (insertTxOut tracer txId) (Generic.txOutputs tx)

    -- Insert the transaction inputs.
    mapM_ (insertTxIn tracer txId) (Generic.txInputs tx)

    case Generic.txMetadata tx of
      Nothing -> pure ()
      Just md -> insertTxMetadata tracer txId md

    mapM_ (insertCertificate tracer network txId epochNo slotNo) $ Generic.txCertificates tx
    mapM_ (insertWithdrawals tracer txId) $ Generic.txWithdrawals tx

    mapM_ (insertParamProposal tracer txId) $ Generic.txParamProposal tx

    insertMaTxMint tracer txId $ Generic.txMint tx

insertTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Generic.TxOut
    -> ExceptT e (ReaderT SqlBackend m) ()
insertTxOut tracer txId (Generic.TxOut index addr value maMap) = do
  mSaId <- lift $ insertStakeAddressRefIfMissing txId addr
  txOutId <- lift . DB.insertTxOut $
                DB.TxOut
                  { DB.txOutTxId = txId
                  , DB.txOutIndex = index
                  , DB.txOutAddress = Generic.renderAddress addr
                  , DB.txOutAddressRaw = Shelley.serialiseAddr addr
                  , DB.txOutPaymentCred = Generic.maybePaymentCred addr
                  , DB.txOutStakeAddressId = mSaId
                  , DB.txOutValue = Generic.coinToDbLovelace value
                  }
  insertMaTxOut tracer txOutId maMap

insertTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Generic.TxIn
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId (Generic.TxIn txId index) = do
  txOutId <- liftLookupFail "insertTxIn" $ DB.queryTxId txId
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral index
              }

insertCertificate
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> DB.TxId -> EpochNo -> SlotNo -> Generic.TxCertificate
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCertificate tracer network txId epochNo slotNo (Generic.TxCertificate idx cert) =
  case cert of
    Shelley.DCertDeleg deleg -> insertDelegCert tracer network txId idx epochNo slotNo deleg
    Shelley.DCertPool pool -> insertPoolCert tracer network epochNo txId idx pool
    Shelley.DCertMir mir -> insertMirCert tracer network txId idx mir
    Shelley.DCertGenesis _gen -> do
        -- TODO : Low priority
        liftIO $ logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
        pure ()


insertPoolCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> EpochNo -> DB.TxId -> Word16 -> Shelley.PoolCert StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolCert tracer network epoch txId idx pCert =
  case pCert of
    Shelley.RegPool pParams -> insertPoolRegister tracer network epoch txId idx pParams
    Shelley.RetirePool keyHash epochNum -> insertPoolRetire txId epochNum idx keyHash

insertDelegCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> DB.TxId -> Word16 -> EpochNo -> SlotNo -> Shelley.DelegCert StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert tracer network txId idx epochNo slotNo dCert =
  case dCert of
    Shelley.RegKey cred -> insertStakeRegistration tracer txId idx $ Generic.annotateStakingCred network cred
    Shelley.DeRegKey cred -> insertStakeDeregistration tracer network txId idx cred
    Shelley.Delegate (Shelley.Delegation cred poolkh) -> insertDelegation tracer network txId idx epochNo slotNo cred poolkh

insertPoolRegister
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> EpochNo -> DB.TxId -> Word16 -> Shelley.PoolParams StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister tracer network (EpochNo epoch) txId idx params = do
  mdId <- case strictMaybeToMaybe $ Shelley._poolMD params of
            Just md -> Just <$> insertMetaData txId md
            Nothing -> pure Nothing

  when (fromIntegral (Shelley.unCoin $ Shelley._poolPledge params) > maxLovelace) $
    liftIO . logWarning tracer $
      mconcat
        [ "Bad pledge amount: ", textShow (Shelley.unCoin $ Shelley._poolPledge params)
        , " > maxLovelace."
        ]

  when (fromIntegral (Shelley.unCoin $ Shelley._poolCost params) > maxLovelace) $
    liftIO . logWarning tracer $
      mconcat
        [ "Bad fixed cost amount: ", textShow (Shelley.unCoin $ Shelley._poolCost params)
        , " > maxLovelace."
        ]

  poolHashId <- insertPoolHash (Shelley._poolId params)
  poolUpdateId <- lift . DB.insertPoolUpdate $
                    DB.PoolUpdate
                      { DB.poolUpdateHashId = poolHashId
                      , DB.poolUpdateCertIndex = idx
                      , DB.poolUpdateVrfKeyHash = Crypto.hashToBytes (Shelley._poolVrf params)
                      , DB.poolUpdatePledge = Generic.coinToDbLovelace (Shelley._poolPledge params)
                      , DB.poolUpdateRewardAddr = Generic.serialiseRewardAcntWithNetwork network (Shelley._poolRAcnt params)
                      , DB.poolUpdateActiveEpochNo = epoch + 2
                      , DB.poolUpdateMetaId = mdId
                      , DB.poolUpdateMargin = realToFrac $ Shelley.intervalValue (Shelley._poolMargin params)
                      , DB.poolUpdateFixedCost = Generic.coinToDbLovelace (Shelley._poolCost params)
                      , DB.poolUpdateRegisteredTxId = txId
                      }

  mapM_ (insertPoolOwner network poolHashId txId) $ toList (Shelley._poolOwners params)
  mapM_ (insertPoolRelay poolUpdateId) $ toList (Shelley._poolRelays params)

maxLovelace :: Word64
maxLovelace = 45000000000000000

insertPoolHash
    :: forall m . (MonadBaseControl IO m, MonadIO m)
    => Shelley.KeyHash 'Shelley.StakePool StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolHashId
insertPoolHash kh =
    lift . DB.insertPoolHash $
      DB.PoolHash
        { DB.poolHashHashRaw = Generic.unKeyHashRaw kh
        , DB.poolHashView = Generic.unKeyHashView kh
        }


insertPoolRetire
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> EpochNo -> Word16 -> Shelley.KeyHash 'Shelley.StakePool StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire txId epochNum idx keyHash = do
  poolId <- liftLookupFail "insertPoolRetire" $ queryStakePoolKeyHash keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetireHashId = poolId
      , DB.poolRetireCertIndex = idx
      , DB.poolRetireAnnouncedTxId = txId
      , DB.poolRetireRetiringEpoch = unEpochNo epochNum
      }


insertMetaData
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> Shelley.PoolMetadata
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolMetaDataId
insertMetaData txId md =
  lift . DB.insertPoolMetaData $
    DB.PoolMetaData
      { DB.poolMetaDataUrl = Shelley.urlToText (Shelley._poolMDUrl md)
      , DB.poolMetaDataHash = Shelley._poolMDHash md
      , DB.poolMetaDataRegisteredTxId = txId
      }

insertStakeAddress
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> Shelley.RewardAcnt StandardCrypto
    -> ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddress txId rewardAddr =
  -- If the address already esists in the table, it will not be inserted again (due to
  -- the uniqueness constraint) but the function will return the 'StakeAddressId'.
  DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHashRaw = Shelley.serialiseRewardAcnt rewardAddr
      , DB.stakeAddressView = Generic.renderRewardAcnt rewardAddr
      , DB.stakeAddressRegisteredTxId = txId
      }

insertStakeAddressRefIfMissing
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.TxId -> Shelley.Addr StandardCrypto
    -> ReaderT SqlBackend m (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing txId addr =
    maybe insertSAR (pure . Just) =<< queryStakeAddressRef addr
  where
    insertSAR :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m (Maybe DB.StakeAddressId)
    insertSAR =
      case addr of
        Shelley.AddrBootstrap {} -> pure Nothing
        Shelley.Addr nw _pcred sref ->
          case sref of
            Shelley.StakeRefBase cred ->
              Just <$> insertStakeAddress txId (Shelley.RewardAcnt nw cred)
            Shelley.StakeRefPtr {} ->
              -- This happens when users pay to payment addresses that refer to a stake addresses
              -- by pointer, but where the pointer does not refer to a registered stake address.
              pure Nothing
            Shelley.StakeRefNull -> pure Nothing

insertPoolOwner
    :: (MonadBaseControl IO m, MonadIO m)
    => Shelley.Network -> DB.PoolHashId -> DB.TxId -> Shelley.KeyHash 'Shelley.Staking StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner network poolHashId txId skh = do
  saId <- lift $ insertStakeAddress txId (Shelley.RewardAcnt network (Shelley.KeyHashObj skh))
  void . lift . DB.insertPoolOwner $
    DB.PoolOwner
      { DB.poolOwnerAddrId = saId
      , DB.poolOwnerPoolHashId = poolHashId
      , DB.poolOwnerRegisteredTxId = txId
      }

insertStakeRegistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Word16 -> Shelley.RewardAcnt StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration _tracer txId idx rewardAccount = do
  saId <- lift $ insertStakeAddress txId rewardAccount
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = saId
      , DB.stakeRegistrationCertIndex = idx
      , DB.stakeRegistrationTxId = txId
      }

insertStakeDeregistration
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> DB.TxId -> Word16 -> Shelley.StakeCredential StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration _tracer network txId idx cred = do
  scId <- liftLookupFail "insertStakeDeregistration" $ queryStakeAddress (Generic.stakingCredHash network cred)
  void . lift . DB.insertStakeDeregistration $
    DB.StakeDeregistration
      { DB.stakeDeregistrationAddrId = scId
      , DB.stakeDeregistrationCertIndex = idx
      , DB.stakeDeregistrationTxId = txId
      }

insertDelegation
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> DB.TxId -> Word16 -> EpochNo -> SlotNo
    -> Shelley.StakeCredential StandardCrypto -> Shelley.KeyHash 'Shelley.StakePool StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegation _tracer network txId idx (EpochNo epoch) slotNo cred poolkh = do
  addrId <- liftLookupFail "insertDelegation" $ queryStakeAddress (Generic.stakingCredHash network cred)
  poolHashId <-liftLookupFail "insertDelegation" $ queryStakePoolKeyHash poolkh
  void . lift . DB.insertDelegation $
    DB.Delegation
      { DB.delegationAddrId = addrId
      , DB.delegationCertIndex = idx
      , DB.delegationPoolHashId = poolHashId
      , DB.delegationActiveEpochNo = epoch + 2 -- The first epoch where this delegation is valid.
      , DB.delegationTxId = txId
      , DB.delegationSlotNo = unSlotNo slotNo
      }

insertMirCert
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Shelley.Network -> DB.TxId -> Word16 -> Shelley.MIRCert StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMirCert _tracer network txId idx mcert = do
    case Shelley.mirPot mcert of
      Shelley.ReservesMIR ->
        case Shelley.mirRewards mcert of
          Shelley.StakeAddressesMIR rwds -> mapM_ insertMirReserves $ Map.toList rwds
          Shelley.SendToOppositePotMIR xfrs -> insertPotTransfer (invert $ Shelley.toDeltaCoin xfrs)

      Shelley.TreasuryMIR -> do
        case Shelley.mirRewards mcert of
          Shelley.StakeAddressesMIR rwds -> mapM_ insertMirTreasury $ Map.toList rwds
          Shelley.SendToOppositePotMIR xfrs -> insertPotTransfer (Shelley.toDeltaCoin xfrs)

  where
    insertMirReserves
        :: (MonadBaseControl IO m, MonadIO m)
        => (Shelley.StakeCredential StandardCrypto, Shelley.DeltaCoin)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirReserves (cred, dcoin) = do
      addrId <- lift . insertStakeAddress txId $ Generic.annotateStakingCred network cred
      void . lift . DB.insertReserve $
        DB.Reserve
          { DB.reserveAddrId = addrId
          , DB.reserveCertIndex = idx
          , DB.reserveTxId = txId
          , DB.reserveAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertMirTreasury
        :: (MonadBaseControl IO m, MonadIO m)
        => (Shelley.StakeCredential StandardCrypto, Shelley.DeltaCoin)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirTreasury (cred, dcoin) = do
      addrId <- lift . insertStakeAddress txId $ Generic.annotateStakingCred network cred
      void . lift . DB.insertTreasury $
        DB.Treasury
          { DB.treasuryAddrId = addrId
          , DB.treasuryCertIndex = idx
          , DB.treasuryTxId = txId
          , DB.treasuryAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertPotTransfer
        :: (MonadBaseControl IO m, MonadIO m)
        => Shelley.DeltaCoin -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertPotTransfer dcoin =
      void . lift . DB.insertPotTransfer $
        DB.PotTransfer
          { DB.potTransferCertIndex = idx
          , DB.potTransferTreasury = DB.deltaCoinToDbInt65 dcoin
          , DB.potTransferReserves = DB.deltaCoinToDbInt65 (invert dcoin)
          , DB.potTransferTxId = txId
          }

insertWithdrawals
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Generic.TxWithdrawal
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertWithdrawals _tracer txId (Generic.TxWithdrawal account coin) = do
  addrId <- liftLookupFail "insertWithdrawals" $ queryStakeAddress (Shelley.serialiseRewardAcnt account)
  void . lift . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = Generic.coinToDbLovelace coin
      }

insertPoolRelay
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.PoolUpdateId -> Shelley.StakePoolRelay
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
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

insertParamProposal
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> ParamProposal
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertParamProposal _tracer txId pp =
  void . lift . DB.insertParamProposal $
    DB.ParamProposal
      { DB.paramProposalEpochNo = unEpochNo $ pppEpochNo pp
      , DB.paramProposalKey = pppKey pp
      , DB.paramProposalMinFeeA = fromIntegral <$> pppMinFeeA pp
      , DB.paramProposalMinFeeB = fromIntegral <$> pppMinFeeB pp
      , DB.paramProposalMaxBlockSize = fromIntegral <$> pppMaxBhSize pp
      , DB.paramProposalMaxTxSize = fromIntegral <$> pppMaxTxSize pp
      , DB.paramProposalMaxBhSize = fromIntegral <$> pppMaxBhSize pp
      , DB.paramProposalKeyDeposit = Generic.coinToDbLovelace <$> pppKeyDeposit pp
      , DB.paramProposalPoolDeposit = Generic.coinToDbLovelace <$> pppPoolDeposit pp
      , DB.paramProposalMaxEpoch = unEpochNo <$> pppMaxEpoch pp
      , DB.paramProposalOptimalPoolCount = fromIntegral <$> pppOptimalPoolCount pp
      , DB.paramProposalInfluence = fromRational <$> pppInfluence pp
      , DB.paramProposalMonetaryExpandRate = Generic.unitIntervalToDouble <$> pppMonetaryExpandRate pp
      , DB.paramProposalTreasuryGrowthRate = Generic.unitIntervalToDouble <$> pppTreasuryGrowthRate pp
      , DB.paramProposalDecentralisation = Generic.unitIntervalToDouble <$> pppDecentralisation pp
      , DB.paramProposalEntropy = Generic.nonceToBytes =<< pppEntropy pp
      , DB.paramProposalProtocolMajor = fromIntegral . Shelley.pvMajor <$> pppProtocolVersion pp
      , DB.paramProposalProtocolMinor = fromIntegral . Shelley.pvMinor <$> pppProtocolVersion pp
      , DB.paramProposalMinUtxoValue = Generic.coinToDbLovelace <$> pppMinUtxoValue pp
      , DB.paramProposalMinPoolCost = Generic.coinToDbLovelace <$> pppMinPoolCost pp
      , DB.paramProposalRegisteredTxId = txId
      }

insertTxMetadata
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Map Word64 TxMetadataValue
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxMetadata tracer txId metadata =
    mapM_ insert $ Map.toList metadata
  where
    insert
        :: (MonadBaseControl IO m, MonadIO m)
        => (Word64, TxMetadataValue)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insert (key, md) = do
      let jsonbs = LBS.toStrict $ Aeson.encode (metadataValueToJsonNoSchema md)
          singleKeyCBORMetadata = serialiseToCBOR $ makeTransactionMetadata $ Map.singleton key md
      ejson <- liftIO $ safeDecodeUtf8 jsonbs
      mjson <- case ejson of
                 Left err -> do
                   liftIO . logWarning tracer $ mconcat
                      [ "insertTxMetadata: Could not decode to UTF8: ", textShow err ]
                   return Nothing
                 Right json ->
                   -- See https://github.com/input-output-hk/cardano-db-sync/issues/297
                   if containsUnicodeNul json
                     then do
                       liftIO $ logWarning tracer "insertTxMetadata: dropped due to a Unicode NUL character."
                       return Nothing
                     else
                       pure $ Just json
      void . lift . DB.insertTxMetadata $
        DB.TxMetadata
          { DB.txMetadataKey = DbWord64 key
          , DB.txMetadataJson = mjson
          , DB.txMetadataBytes = singleKeyCBORMetadata
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

insertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards _tracer epoch rewards = do
    forM_ (chunksOf 1000 $ Map.toList rewards) $ \rewardsChunk -> do
      dbRewards <- concatMapM mkRewards rewardsChunk
      lift $ putMany dbRewards
  where
    mkRewards
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- liftLookupFail "insertReward StakeAddress" $ queryStakeAddress (Generic.unStakeCred saddr)
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- liftLookupFail "insertReward StakePool" $ queryStakePoolKeyHash (Shelley.rewardPool rwd)
        pure $ DB.Reward
                  { DB.rewardAddrId = saId
                  , DB.rewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.rewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.rewardEpochNo = unEpochNo epoch
                  , DB.rewardPoolId = poolId
                  }

insertOrphanedRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOrphanedRewards _tracer epoch orphanedRewards =
    -- There are probably not many of these for each epoch, but just in case there
    -- are, it does not hurt to chunk them.
    forM_ (chunksOf 1000 $ Map.toList orphanedRewards) $ \orphanedRewardsChunk -> do
      dbRewards <- concatMapM mkOrphanedReward orphanedRewardsChunk
      lift $ putMany dbRewards
  where
    mkOrphanedReward
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.OrphanedReward]
    mkOrphanedReward (saddr, rset) = do
      saId <- liftLookupFail "insertReward StakeAddress" $ queryStakeAddress (Generic.unStakeCred saddr)
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- liftLookupFail "insertReward StakePool" $ queryStakePoolKeyHash (Shelley.rewardPool rwd)
        pure $ DB.OrphanedReward
                  { DB.orphanedRewardAddrId = saId
                  , DB.orphanedRewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.orphanedRewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.orphanedRewardEpochNo = unEpochNo epoch
                  , DB.orphanedRewardPoolId = poolId
                  }

insertEpochParam
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> EpochNo -> Generic.ProtoParams -> Shelley.Nonce
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochParam _tracer blkId (EpochNo epoch) params nonce =
  void . lift . DB.insertEpochParam $
    DB.EpochParam
      { DB.epochParamEpochNo = epoch
      , DB.epochParamMinFeeA = fromIntegral (Generic.ppMinfeeA params)
      , DB.epochParamMinFeeB = fromIntegral (Generic.ppMinfeeB params)
      , DB.epochParamMaxBlockSize = fromIntegral (Generic.ppMaxBBSize params)
      , DB.epochParamMaxTxSize = fromIntegral (Generic.ppMaxTxSize params)
      , DB.epochParamMaxBhSize = fromIntegral (Generic.ppMaxBHSize params)
      , DB.epochParamKeyDeposit = Generic.coinToDbLovelace (Generic.ppKeyDeposit params)
      , DB.epochParamPoolDeposit = Generic.coinToDbLovelace (Generic.ppPoolDeposit params)
      , DB.epochParamMaxEpoch = unEpochNo (Generic.ppMaxEpoch params)
      , DB.epochParamOptimalPoolCount = fromIntegral (Generic.ppOptialPoolCount params)
      , DB.epochParamInfluence = fromRational (Generic.ppInfluence params)
      , DB.epochParamMonetaryExpandRate = Generic.unitIntervalToDouble (Generic.ppMonetaryExpandRate params)
      , DB.epochParamTreasuryGrowthRate = Generic.unitIntervalToDouble (Generic.ppTreasuryGrowthRate params)
      , DB.epochParamDecentralisation = Generic.unitIntervalToDouble (Generic.ppDecentralisation params)
      , DB.epochParamEntropy = Generic.nonceToBytes $ Generic.ppExtraEntropy params
      , DB.epochParamProtocolMajor = fromIntegral $ Shelley.pvMajor (Generic.ppProtocolVersion params)
      , DB.epochParamProtocolMinor = fromIntegral $ Shelley.pvMinor (Generic.ppProtocolVersion params)
      , DB.epochParamMinUtxoValue = Generic.coinToDbLovelace (Generic.ppMinUTxOValue params)
      , DB.epochParamMinPoolCost = Generic.coinToDbLovelace (Generic.ppMinPoolCost params)
      , DB.epochParamNonce = Generic.nonceToBytes nonce
      , DB.epochParamBlockId = blkId
      }

insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Generic.StakeDist
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake _tracer (EpochNo epoch) smap =
    forM_ (chunksOf 1000 $ Map.toList (Generic.unStakeDist smap)) $ \stakeChunk -> do
      dbStakes <- mapM mkStake stakeChunk
      lift $ putMany dbStakes
  where
    mkStake
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Shelley.Coin)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake (saddr, coin) = do
      (saId, poolId) <- liftLookupFail "insertEpochStake" $ queryStakeAddressAndPool epoch (Generic.unStakeCred saddr)
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = epoch -- The epoch where this delegation becomes valid.
          }

insertMaTxMint
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Value StandardCrypto
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMaTxMint _tracer txId (Value _adaShouldAlwaysBeZeroButWeDoNotCheck mintMap) =
    mapM_ insertOuter $ Map.toList mintMap
  where
    insertOuter
        :: (MonadBaseControl IO m, MonadIO m)
        => (PolicyID StandardCrypto, Map AssetName Integer)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertOuter (policy, aMap) =
      mapM_ (insertInner policy) $ Map.toList aMap

    insertInner
        :: (MonadBaseControl IO m, MonadIO m)
        => PolicyID StandardCrypto -> (AssetName, Integer)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertInner policy (aname, amount) =
      void . lift . DB.insertMaTxMint $
        DB.MaTxMint
          { DB.maTxMintPolicy = Generic.unScriptHash (policyID policy)
          , DB.maTxMintName = assetName aname
          , DB.maTxMintQuantity = DB.integerToDbInt65 amount
          , DB.maTxMintTxId = txId
          }

insertMaTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxOutId -> Map (PolicyID StandardCrypto) (Map AssetName Integer)
    -> ExceptT e (ReaderT SqlBackend m) ()
insertMaTxOut _tracer txOutId maMap =
    mapM_ insertOuter $ Map.toList maMap
  where
    insertOuter
        :: (MonadBaseControl IO m, MonadIO m)
        => (PolicyID StandardCrypto, Map AssetName Integer)
        -> ExceptT e (ReaderT SqlBackend m) ()
    insertOuter (policy, aMap) =
      mapM_ (insertInner policy) $ Map.toList aMap

    insertInner
        :: (MonadBaseControl IO m, MonadIO m)
        => PolicyID StandardCrypto -> (AssetName, Integer)
        -> ExceptT e (ReaderT SqlBackend m) ()
    insertInner policy (aname, amount) =
      void . lift . DB.insertMaTxOut $
        DB.MaTxOut
          { DB.maTxOutPolicy = Generic.unScriptHash (policyID policy)
          , DB.maTxOutName = assetName aname
          , DB.maTxOutQuantity = DbWord64 (fromIntegral amount)
          , DB.maTxOutTxOutId = txOutId
          }

insertPots
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.BlockId
    -> SlotNo -> EpochNo
    -> Generic.AdaPots
    -> ExceptT e (ReaderT SqlBackend m) ()
insertPots blockId slotNo epochNo pots =
    void . lift $ DB.insertAdaPots $
      DB.AdaPots
        { DB.adaPotsSlotNo = unSlotNo slotNo
        , DB.adaPotsEpochNo = unEpochNo epochNo
        , DB.adaPotsTreasury = Generic.coinToDbLovelace $ Generic.apTreasury pots
        , DB.adaPotsReserves = Generic.coinToDbLovelace $ Generic.apReserves pots
        , DB.adaPotsRewards = Generic.coinToDbLovelace $ Generic.apRewards pots
        , DB.adaPotsUtxo = Generic.coinToDbLovelace $ Generic.apUtxo pots
        , DB.adaPotsDeposits = Generic.coinToDbLovelace $ Generic.apDeposits pots
        , DB.adaPotsFees = Generic.coinToDbLovelace $ Generic.apFees pots
        , DB.adaPotsBlockId = blockId
        }
