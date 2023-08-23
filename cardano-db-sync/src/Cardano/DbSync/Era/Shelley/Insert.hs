{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Insert (
  insertShelleyBlock,
  -- These are exported for data in Shelley Genesis
  insertPoolRegister,
  insertStakeRegistration,
  insertDelegation,
  insertStakeAddressRefIfMissing,
  mkAdaPots,
) where

import Cardano.BM.Trace (Trace, logDebug, logInfo, logWarning)
import Cardano.Crypto.Hash (hashToBytes)
import qualified Cardano.Crypto.Hashing as Crypto
import Cardano.Db (DbLovelace (..), DbWord64 (..), PoolUrl (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (
  insertBlockAndCache,
  insertDatumAndCache,
  insertPoolKeyWithCache,
  queryDatum,
  queryMAWithCache,
  queryPoolKeyWithCache,
  queryPrevBlockWithCache,
  queryRewardAccountWithCache,
  queryRewardAccountWithCacheRetBs,
  queryStakeAddrWithCache,
 )
import Cardano.DbSync.Cache.Epoch (writeEpochBlockDiffToCache)
import Cardano.DbSync.Cache.Types (Cache (..), CacheNew (..), EpochBlockDiff (..))

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Metadata (
  TxMetadataValue (..),
  metadataValueToJsonNoSchema,
 )
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Shelley.Insert.Epoch
import Cardano.DbSync.Era.Shelley.Insert.Grouped
import Cardano.DbSync.Era.Shelley.Offline
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Era.Util (liftLookupFail, safeDecodeToJson)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (ApplyResult (..), DepositsMap, lookupDepositsMap)
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Cbor (serialiseTxMetadataToCbor)
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo.Language (Language)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Ledger.BaseTypes (getVersion, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Coin as Ledger
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Keys
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert
import Cardano.Prelude
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except.Extra (newExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either.Extra (eitherToMaybe)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardConway, StandardCrypto)

{- HLINT ignore "Reduce duplication" -}

type IsPoolMember = PoolKeyHash -> Bool

insertShelleyBlock ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Bool ->
  Bool ->
  Bool ->
  Generic.Block ->
  SlotDetails ->
  IsPoolMember ->
  ApplyResult ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertShelleyBlock syncEnv shouldLog withinTwoMins withinHalfHour blk details isMember applyResult = do
  runExceptT $ do
    pbid <- case Generic.blkPreviousHash blk of
      Nothing -> liftLookupFail (renderErrorMessage (Generic.blkEra blk)) DB.queryGenesis -- this is for networks that fork from Byron on epoch 0.
      Just pHash -> queryPrevBlockWithCache (renderErrorMessage (Generic.blkEra blk)) cache pHash
    mPhid <- lift $ queryPoolKeyWithCache cache CacheNew $ coerceKeyRole $ Generic.blkSlotLeader blk
    let epochNo = sdEpochNo details

    slid <- lift . DB.insertSlotLeader $ Generic.mkSlotLeader (Generic.unKeyHashRaw $ Generic.blkSlotLeader blk) (eitherToMaybe mPhid)
    blkId <-
      lift . insertBlockAndCache cache $
        DB.Block
          { DB.blockHash = Generic.blkHash blk
          , DB.blockEpochNo = Just $ unEpochNo epochNo
          , DB.blockSlotNo = Just $ unSlotNo (Generic.blkSlotNo blk)
          , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
          , DB.blockBlockNo = Just $ unBlockNo (Generic.blkBlockNo blk)
          , DB.blockPreviousId = Just pbid
          , DB.blockSlotLeaderId = slid
          , DB.blockSize = Generic.blkSize blk
          , DB.blockTime = sdSlotTime details
          , DB.blockTxCount = fromIntegral $ length (Generic.blkTxs blk)
          , DB.blockProtoMajor = getVersion $ Ledger.pvMajor (Generic.blkProto blk)
          , DB.blockProtoMinor = fromIntegral $ Ledger.pvMinor (Generic.blkProto blk)
          , -- Shelley specific
            DB.blockVrfKey = Just $ Generic.blkVrfKey blk
          , DB.blockOpCert = Just $ Generic.blkOpCert blk
          , DB.blockOpCertCounter = Just $ Generic.blkOpCertCounter blk
          }

    let zippedTx = zip [0 ..] (Generic.blkTxs blk)
    let txInserter = insertTx syncEnv isMember blkId (sdEpochNo details) (Generic.blkSlotNo blk) (apDepositsMap applyResult)
    blockGroupedData <- foldM (\gp (idx, tx) -> txInserter idx tx gp) mempty zippedTx
    minIds <- insertBlockGroupedData syncEnv blockGroupedData

    -- now that we've inserted the Block and all it's txs lets cache what we'll need
    -- when we later update the epoch values.
    -- if have --dissable-epoch && --dissable-cache then no need to cache data.
    when (soptEpochAndCacheEnabled $ envOptions syncEnv)
      . newExceptT
      $ writeEpochBlockDiffToCache
        cache
        EpochBlockDiff
          { ebdBlockId = blkId
          , ebdTime = sdSlotTime details
          , ebdFees = groupedTxFees blockGroupedData
          , ebdEpochNo = unEpochNo (sdEpochNo details)
          , ebdOutSum = fromIntegral $ groupedTxOutSum blockGroupedData
          , ebdTxCount = fromIntegral $ length (Generic.blkTxs blk)
          }

    when withinHalfHour $
      insertReverseIndex blkId minIds

    liftIO $ do
      let epoch = unEpochNo epochNo
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)

      when (withinTwoMins && slotWithinEpoch /= 0 && unBlockNo (Generic.blkBlockNo blk) `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ renderInsertName (Generic.blkEra blk)
            , ": continuing epoch "
            , textShow epoch
            , " (slot "
            , textShow slotWithinEpoch
            , "/"
            , textShow (unEpochSize $ sdEpochSize details)
            , ")"
            ]
      logger tracer $
        mconcat
          [ renderInsertName (Generic.blkEra blk)
          , ": epoch "
          , textShow (unEpochNo epochNo)
          , ", slot "
          , textShow (unSlotNo $ Generic.blkSlotNo blk)
          , ", block "
          , textShow (unBlockNo $ Generic.blkBlockNo blk)
          , ", hash "
          , renderByteArray (Generic.blkHash blk)
          ]

    whenStrictJust (apNewEpoch applyResult) $ \newEpoch -> do
      insertOnNewEpoch tracer blkId (Generic.blkSlotNo blk) epochNo newEpoch

    insertStakeSlice syncEnv $ apStakeSlice applyResult

    when (ioOfflineData iopts && unBlockNo (Generic.blkBlockNo blk) `mod` offlineModBase == 0)
      . lift
      $ do
        insertOfflineResults tracer (envOfflineResultQueue syncEnv)
        loadOfflineWorkQueue tracer (envOfflineWorkQueue syncEnv)
  where
    iopts = getInsertOptions syncEnv

    logger :: Trace IO a -> a -> IO ()
    logger
      | shouldLog = logInfo
      | withinTwoMins = logInfo
      | unBlockNo (Generic.blkBlockNo blk) `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

    renderInsertName :: Generic.BlockEra -> Text
    renderInsertName eraName =
      mconcat ["Insert ", textShow eraName, " Block"]

    renderErrorMessage :: Generic.BlockEra -> Text
    renderErrorMessage eraName =
      case eraName of
        Generic.Shelley -> "insertShelleyBlock"
        other -> mconcat ["insertShelleyBlock(", textShow other, ")"]

    offlineModBase :: Word64
    offlineModBase = if withinTwoMins then 10 else 2000

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    cache :: Cache
    cache = envCache syncEnv

-- -----------------------------------------------------------------------------

insertOnNewEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Generic.NewEpoch ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOnNewEpoch tracer blkId slotNo epochNo newEpoch = do
  whenStrictJust (Generic.euProtoParams epochUpdate) $ \params ->
    lift $ insertEpochParam tracer blkId epochNo params (Generic.euNonce epochUpdate)
  whenStrictJust (Generic.neAdaPots newEpoch) $ \pots ->
    insertPots blkId slotNo epochNo pots
  where
    epochUpdate :: Generic.EpochUpdate
    epochUpdate = Generic.neEpochUpdate newEpoch

-- -----------------------------------------------------------------------------

insertTx ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  IsPoolMember ->
  DB.BlockId ->
  EpochNo ->
  SlotNo ->
  DepositsMap ->
  Word64 ->
  Generic.Tx ->
  BlockGroupedData ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) BlockGroupedData
insertTx syncEnv isMember blkId epochNo slotNo depositsMap blockIndex tx grouped = do
  let !txHash = Generic.txHash tx
  let !mdeposits = if not (Generic.txValidContract tx) then Just (Coin 0) else lookupDepositsMap txHash depositsMap
  let !outSum = fromIntegral $ unCoin $ Generic.txOutSum tx
      !withdrawalSum = fromIntegral $ unCoin $ Generic.txWithdrawalSum tx
  hasConsumed <- liftIO $ getHasConsumedOrPruneTxOut syncEnv
  -- In some txs and with specific configuration we may be able to find necessary data within the tx body.
  -- In these cases we can avoid expensive queries.
  (resolvedInputs, fees', deposits) <- case (mdeposits, unCoin <$> Generic.txFees tx) of
    (Just deposits, Just fees) -> do
      (resolvedInputs, _) <- splitLast <$> mapM (resolveTxInputs hasConsumed False (fst <$> groupedTxOut grouped)) (Generic.txInputs tx)
      pure (resolvedInputs, fees, Just (unCoin deposits))
    (Nothing, Just fees) -> do
      (resolvedInputs, amounts) <- splitLast <$> mapM (resolveTxInputs hasConsumed False (fst <$> groupedTxOut grouped)) (Generic.txInputs tx)
      if any isNothing amounts
        then pure (resolvedInputs, fees, Nothing)
        else
          let !inSum = sum $ map unDbLovelace $ catMaybes amounts
           in pure (resolvedInputs, fees, Just $ fromIntegral (inSum + withdrawalSum) - fromIntegral outSum - fromIntegral fees)
    (_, Nothing) -> do
      -- Nothing in fees means a phase 2 failure
      (resolvedInsFull, amounts) <- splitLast <$> mapM (resolveTxInputs hasConsumed True (fst <$> groupedTxOut grouped)) (Generic.txInputs tx)
      let !inSum = sum $ map unDbLovelace $ catMaybes amounts
          !diffSum = if inSum >= outSum then inSum - outSum else 0
          !fees = maybe diffSum (fromIntegral . unCoin) (Generic.txFees tx)
      pure (resolvedInsFull, fromIntegral fees, Just 0)
  let fees = fromIntegral fees'
  -- Insert transaction and get txId from the DB.
  !txId <-
    lift . DB.insertTx $
      DB.Tx
        { DB.txHash = txHash
        , DB.txBlockId = blkId
        , DB.txBlockIndex = blockIndex
        , DB.txOutSum = DB.DbLovelace outSum
        , DB.txFee = DB.DbLovelace fees
        , DB.txDeposit = fromIntegral <$> deposits
        , DB.txSize = Generic.txSize tx
        , DB.txInvalidBefore = DbWord64 . unSlotNo <$> Generic.txInvalidBefore tx
        , DB.txInvalidHereafter = DbWord64 . unSlotNo <$> Generic.txInvalidHereafter tx
        , DB.txValidContract = Generic.txValidContract tx
        , DB.txScriptSize = sum $ Generic.txScriptSizes tx
        }

  if not (Generic.txValidContract tx)
    then do
      !txOutsGrouped <- mapM (prepareTxOut tracer cache iopts (txId, txHash)) (Generic.txOutputs tx)

      let !txIns = map (prepareTxIn txId Map.empty) resolvedInputs
      -- There is a custom semigroup instance for BlockGroupedData which uses addition for the values `fees` and `outSum`.
      -- Same happens bellow on last line of this function.
      pure (grouped <> BlockGroupedData txIns txOutsGrouped [] [] fees outSum)
    else do
      -- The following operations only happen if the script passes stage 2 validation (or the tx has
      -- no script).
      !txOutsGrouped <- mapM (prepareTxOut tracer cache iopts (txId, txHash)) (Generic.txOutputs tx)

      !redeemers <- Map.fromList <$> mapM (insertRedeemer tracer (fst <$> groupedTxOut grouped) txId) (Generic.txRedeemer tx)

      when (ioPlutusExtra iopts) $
        mapM_ (insertDatum tracer cache txId) (Generic.txData tx)

      mapM_ (insertCollateralTxIn tracer txId) (Generic.txCollateralInputs tx)

      mapM_ (insertReferenceTxIn tracer txId) (Generic.txReferenceInputs tx)

      mapM_ (insertCollateralTxOut tracer cache iopts (txId, txHash)) (Generic.txCollateralOutputs tx)

      txMetadata <-
        whenFalseMempty (ioMetadata iopts) $
          prepareTxMetadata tracer txId (Generic.txMetadata tx)

      mapM_ (insertCertificate tracer cache isMember network blkId txId epochNo slotNo redeemers) $ Generic.txCertificates tx
      mapM_ (insertWithdrawals tracer cache txId redeemers) $ Generic.txWithdrawals tx

      mapM_ (lift . insertParamProposal blkId txId) $ Generic.txParamProposal tx

      maTxMint <-
        whenFalseMempty (ioMetadata iopts) $
          prepareMaTxMint tracer cache txId $
            Generic.txMint tx

      when (ioPlutusExtra iopts) $
        mapM_ (insertScript tracer txId) $
          Generic.txScripts tx

      mapM_ (insertExtraKeyWitness tracer txId) $ Generic.txExtraKeyWitnesses tx
      mapM_ (lift . insertGovernanceAction cache network blkId txId) $ zip [0 ..] (Generic.txProposalProcedure tx)
      mapM_ (insertVotingProcedure cache txId) $ zip [0 ..] (Generic.txVotingProcedure tx)

      let !txIns = map (prepareTxIn txId redeemers) resolvedInputs
      pure (grouped <> BlockGroupedData txIns txOutsGrouped txMetadata maTxMint fees outSum)
  where
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    network = getNetwork syncEnv

prepareTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  InsertOptions ->
  (DB.TxId, ByteString) ->
  Generic.TxOut ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (ExtendedTxOut, [MissingMaTxOut])
prepareTxOut tracer cache iopts (txId, txHash) (Generic.TxOut index addr addrRaw value maMap mScript dt) = do
  mSaId <- lift $ insertStakeAddressRefIfMissing tracer cache txId addr
  mDatumId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      Generic.whenInlineDatum dt $
        insertDatum tracer cache txId
  mScriptId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      whenMaybe mScript $
        insertScript tracer txId
  let !txOut =
        DB.TxOut
          { DB.txOutTxId = txId
          , DB.txOutIndex = index
          , DB.txOutAddress = Generic.renderAddress addr
          , DB.txOutAddressRaw = addrRaw
          , DB.txOutAddressHasScript = hasScript
          , DB.txOutPaymentCred = Generic.maybePaymentCred addr
          , DB.txOutStakeAddressId = mSaId
          , DB.txOutValue = Generic.coinToDbLovelace value
          , DB.txOutDataHash = Generic.dataHashToBytes <$> Generic.getTxOutDatumHash dt
          , DB.txOutInlineDatumId = mDatumId
          , DB.txOutReferenceScriptId = mScriptId
          }
  let !eutxo = ExtendedTxOut txHash txOut
  !maTxOuts <- whenFalseMempty (ioMultiAssets iopts) $ prepareMaTxOuts tracer cache maMap
  pure (eutxo, maTxOuts)
  where
    hasScript :: Bool
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

insertCollateralTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  InsertOptions ->
  (DB.TxId, ByteString) ->
  Generic.TxOut ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxOut tracer cache iopts (txId, _txHash) (Generic.TxOut index addr addrRaw value maMap mScript dt) = do
  mSaId <- lift $ insertStakeAddressRefIfMissing tracer cache txId addr
  mDatumId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      Generic.whenInlineDatum dt $
        insertDatum tracer cache txId
  mScriptId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      whenMaybe mScript $
        insertScript tracer txId
  _ <-
    lift . DB.insertCollateralTxOut $
      DB.CollateralTxOut
        { DB.collateralTxOutTxId = txId
        , DB.collateralTxOutIndex = index
        , DB.collateralTxOutAddress = Generic.renderAddress addr
        , DB.collateralTxOutAddressRaw = addrRaw
        , DB.collateralTxOutAddressHasScript = hasScript
        , DB.collateralTxOutPaymentCred = Generic.maybePaymentCred addr
        , DB.collateralTxOutStakeAddressId = mSaId
        , DB.collateralTxOutValue = Generic.coinToDbLovelace value
        , DB.collateralTxOutDataHash = Generic.dataHashToBytes <$> Generic.getTxOutDatumHash dt
        , DB.collateralTxOutMultiAssetsDescr = textShow maMap
        , DB.collateralTxOutInlineDatumId = mDatumId
        , DB.collateralTxOutReferenceScriptId = mScriptId
        }
  pure ()
  where
    -- TODO: Is there any reason to add new tables for collateral multi-assets/multi-asset-outputs

    hasScript :: Bool
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

prepareTxIn ::
  DB.TxId ->
  Map Word64 DB.RedeemerId ->
  (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutId) ->
  ExtendedTxIn
prepareTxIn txInId redeemers (txIn, txOutId, mTxOutId) =
  ExtendedTxIn
    { etiTxIn = txInDB
    , etiTxOutId = mTxOutId
    }
  where
    txInDB =
      DB.TxIn
        { DB.txInTxInId = txInId
        , DB.txInTxOutId = txOutId
        , DB.txInTxOutIndex = fromIntegral $ Generic.txInIndex txIn
        , DB.txInRedeemerId = mlookup (Generic.txInRedeemerIndex txIn) redeemers
        }

insertCollateralTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxIn _tracer txInId (Generic.TxIn txId index _) = do
  txOutId <- liftLookupFail "insertCollateralTxIn" $ DB.queryTxId txId
  void . lift . DB.insertCollateralTxIn $
    DB.CollateralTxIn
      { DB.collateralTxInTxInId = txInId
      , DB.collateralTxInTxOutId = txOutId
      , DB.collateralTxInTxOutIndex = fromIntegral index
      }

insertReferenceTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertReferenceTxIn _tracer txInId (Generic.TxIn txId index _) = do
  txOutId <- liftLookupFail "insertReferenceTxIn" $ DB.queryTxId txId
  void . lift . DB.insertReferenceTxIn $
    DB.ReferenceTxIn
      { DB.referenceTxInTxInId = txInId
      , DB.referenceTxInTxOutId = txOutId
      , DB.referenceTxInTxOutIndex = fromIntegral index
      }

insertCertificate ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  IsPoolMember ->
  Ledger.Network ->
  DB.BlockId ->
  DB.TxId ->
  EpochNo ->
  SlotNo ->
  Map Word64 DB.RedeemerId ->
  Generic.TxCertificate ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCertificate tracer cache isMember network blkId txId epochNo slotNo redeemers (Generic.TxCertificate ridx idx cert) =
  case cert of
    Left (ShelleyTxCertDelegCert deleg) -> insertDelegCert tracer cache network txId idx mRedeemerId epochNo slotNo deleg
    Left (ShelleyTxCertPool pool) -> insertPoolCert tracer cache isMember network epochNo blkId txId idx pool
    Left (ShelleyTxCertMir mir) -> insertMirCert tracer cache network txId idx mir
    Left (ShelleyTxCertGenesisDeleg _gen) ->
      liftIO $ logWarning tracer "insertCertificate: Unhandled DCertGenesis certificate"
    Right (ConwayTxCertDeleg deleg) -> insertConwayDelegCert cache network txId idx mRedeemerId epochNo slotNo deleg
    Right (ConwayTxCertPool pool) -> insertPoolCert tracer cache isMember network epochNo blkId txId idx pool
    Right (ConwayTxCertCommittee c) -> case c of
      ConwayRegDRep cred coin ->
        lift $ insertDrepRegistration txId idx cred coin
      ConwayUnRegDRep cred coin ->
        lift $ insertDrepDeRegistration txId idx cred coin
      ConwayAuthCommitteeHotKey khCold khHot ->
        lift $ insertCommitteeRegistration txId idx khCold khHot
      ConwayResignCommitteeColdKey khCold ->
        lift $ insertCommitteeDeRegistration txId idx khCold
  where
    mRedeemerId = mlookup ridx redeemers

insertCommitteeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  KeyHash 'CommitteeColdKey c ->
  KeyHash 'CommitteeHotKey c ->
  ReaderT SqlBackend m ()
insertCommitteeRegistration txId idx khCold khHot = do
  void . DB.insertCommitteeRegistration $
    DB.CommitteeRegistration
      { DB.committeeRegistrationTxId = txId
      , DB.committeeRegistrationCertIndex = idx
      , DB.committeeRegistrationColdKey = Generic.unKeyHashRaw khCold
      , DB.committeeRegistrationHotKey = Generic.unKeyHashRaw khHot
      }

insertCommitteeDeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  KeyHash 'CommitteeColdKey c ->
  ReaderT SqlBackend m ()
insertCommitteeDeRegistration txId idx khCold = do
  void . DB.insertCommitteeDeRegistration $
    DB.CommitteeDeRegistration
      { DB.committeeDeRegistrationTxId = txId
      , DB.committeeDeRegistrationCertIndex = idx
      , DB.committeeDeRegistrationHotKey = Generic.unKeyHashRaw khCold
      }

insertDrepRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'Voting StandardCrypto ->
  Coin ->
  ReaderT SqlBackend m ()
insertDrepRegistration txId idx cred coin = do
  drepId <- insertCredDrepHash cred
  void . DB.insertDrepRegistration $
    DB.DrepRegistration
      { DB.drepRegistrationTxId = txId
      , DB.drepRegistrationCertIndex = idx
      , DB.drepRegistrationDeposit = Generic.coinToDbLovelace coin
      , DB.drepRegistrationDrepHashId = drepId
      }

insertDrepDeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Word16 ->
  Ledger.Credential 'Voting StandardCrypto ->
  Coin ->
  ReaderT SqlBackend m ()
insertDrepDeRegistration txId idx cred coin = do
  drepId <- insertCredDrepHash cred
  void . DB.insertDrepDeRegistration $
    DB.DrepDeRegistration
      { DB.drepDeRegistrationTxId = txId
      , DB.drepDeRegistrationCertIndex = idx
      , DB.drepDeRegistrationDeposit = Generic.coinToDbLovelace coin
      , DB.drepDeRegistrationDrepHashId = drepId
      }

insertPoolCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  IsPoolMember ->
  Ledger.Network ->
  EpochNo ->
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  Shelley.PoolCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolCert tracer cache isMember network epoch blkId txId idx pCert =
  case pCert of
    Shelley.RegPool pParams -> insertPoolRegister tracer cache isMember network epoch blkId txId idx pParams
    Shelley.RetirePool keyHash epochNum -> insertPoolRetire txId cache epochNum idx keyHash

insertDelegCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ShelleyDelegCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegCert _tracer cache network txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ShelleyRegCert cred -> insertStakeRegistration epochNo txId idx $ Generic.annotateStakingCred network cred
    ShelleyUnRegCert cred -> insertStakeDeregistration cache network epochNo txId idx mRedeemerId cred
    ShelleyDelegCert cred poolkh -> insertDelegation cache network epochNo slotNo txId idx mRedeemerId cred poolkh

insertConwayDelegCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  EpochNo ->
  SlotNo ->
  ConwayDelegCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertConwayDelegCert cache network txId idx mRedeemerId epochNo slotNo dCert =
  case dCert of
    ConwayRegCert cred _dep -> insertStakeRegistration epochNo txId idx $ Generic.annotateStakingCred network cred
    ConwayUnRegCert cred _dep -> insertStakeDeregistration cache network epochNo txId idx mRedeemerId cred
    ConwayDelegCert cred delegatee -> insertDeleg cred delegatee
    ConwayRegDelegCert cred delegatee _dep -> do
      insertStakeRegistration epochNo txId idx $ Generic.annotateStakingCred network cred
      insertDeleg cred delegatee
  where
    insertDeleg cred = \case
      DelegStake poolkh -> insertDelegation cache network epochNo slotNo txId idx mRedeemerId cred poolkh
      DelegVote drep -> insertDelegationVote cache network txId idx cred drep
      DelegStakeVote poolkh drep -> do
        insertDelegation cache network epochNo slotNo txId idx mRedeemerId cred poolkh
        insertDelegationVote cache network txId idx cred drep

insertPoolRegister ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  IsPoolMember ->
  Ledger.Network ->
  EpochNo ->
  DB.BlockId ->
  DB.TxId ->
  Word16 ->
  Shelley.PoolParams StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRegister _tracer cache isMember network (EpochNo epoch) blkId txId idx params = do
  poolHashId <- lift $ insertPoolKeyWithCache cache CacheNew (Shelley.ppId params)
  mdId <- case strictMaybeToMaybe $ Shelley.ppMetadata params of
    Just md -> Just <$> insertMetaDataRef poolHashId txId md
    Nothing -> pure Nothing

  epochActivationDelay <- mkEpochActivationDelay poolHashId

  saId <- lift $ insertStakeAddressWithCache cache CacheNew txId (adjustNetworkTag $ Shelley.ppRewardAcnt params)
  poolUpdateId <-
    lift . DB.insertPoolUpdate $
      DB.PoolUpdate
        { DB.poolUpdateHashId = poolHashId
        , DB.poolUpdateCertIndex = idx
        , DB.poolUpdateVrfKeyHash = hashToBytes (Shelley.ppVrf params)
        , DB.poolUpdatePledge = Generic.coinToDbLovelace (Shelley.ppPledge params)
        , DB.poolUpdateRewardAddrId = saId
        , DB.poolUpdateActiveEpochNo = epoch + epochActivationDelay
        , DB.poolUpdateMetaId = mdId
        , DB.poolUpdateMargin = realToFrac $ Ledger.unboundRational (Shelley.ppMargin params)
        , DB.poolUpdateFixedCost = Generic.coinToDbLovelace (Shelley.ppCost params)
        , DB.poolUpdateRegisteredTxId = txId
        }

  mapM_ (insertPoolOwner cache network poolUpdateId txId) $ toList (Shelley.ppOwners params)
  mapM_ (insertPoolRelay poolUpdateId) $ toList (Shelley.ppRelays params)
  where
    mkEpochActivationDelay :: MonadIO m => DB.PoolHashId -> ExceptT SyncNodeError (ReaderT SqlBackend m) Word64
    mkEpochActivationDelay poolHashId =
      if isMember (Shelley.ppId params)
        then pure 3
        else do
          -- if the pool is not registered at the end of the previous block, check for
          -- other registrations at the current block. If this is the first registration
          -- then it's +2, else it's +3.
          otherUpdates <- lift $ queryPoolUpdateByBlock blkId poolHashId
          pure $ if otherUpdates then 3 else 2

    -- Ignore the network in the `RewardAcnt` and use the provided one instead.
    -- This is a workaround for https://github.com/input-output-hk/cardano-db-sync/issues/546
    adjustNetworkTag :: Ledger.RewardAcnt StandardCrypto -> Ledger.RewardAcnt StandardCrypto
    adjustNetworkTag (Shelley.RewardAcnt _ cred) = Shelley.RewardAcnt network cred

insertPoolRetire ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Cache ->
  EpochNo ->
  Word16 ->
  Ledger.KeyHash 'Ledger.StakePool StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRetire txId cache epochNum idx keyHash = do
  poolId <- liftLookupFail "insertPoolRetire" $ queryPoolKeyWithCache cache CacheNew keyHash
  void . lift . DB.insertPoolRetire $
    DB.PoolRetire
      { DB.poolRetireHashId = poolId
      , DB.poolRetireCertIndex = idx
      , DB.poolRetireAnnouncedTxId = txId
      , DB.poolRetireRetiringEpoch = unEpochNo epochNum
      }

insertMetaDataRef ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.PoolHashId ->
  DB.TxId ->
  Shelley.PoolMetadata ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolMetadataRefId
insertMetaDataRef poolId txId md =
  lift . DB.insertPoolMetadataRef $
    DB.PoolMetadataRef
      { DB.poolMetadataRefPoolId = poolId
      , DB.poolMetadataRefUrl = PoolUrl $ Ledger.urlToText (Shelley.pmUrl md)
      , DB.poolMetadataRefHash = Shelley.pmHash md
      , DB.poolMetadataRefRegisteredTxId = txId
      }

insertStakeAddressWithCache ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  CacheNew ->
  DB.TxId ->
  Shelley.RewardAcnt StandardCrypto ->
  ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddressWithCache cache cacheNew txId rewardAddr = do
  eiAddrId <- queryRewardAccountWithCacheRetBs cache cacheNew rewardAddr
  case eiAddrId of
    Left (_err, bs) -> insertStakeAddress txId rewardAddr (Just bs)
    Right addrId -> pure addrId

-- If the address already esists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.TxId ->
  Shelley.RewardAcnt StandardCrypto ->
  Maybe ByteString ->
  ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddress _txId rewardAddr stakeCredBs =
  DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHashRaw = addrBs
      , DB.stakeAddressView = Generic.renderRewardAcnt rewardAddr
      , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.getRwdCred rewardAddr
      }
  where
    addrBs = fromMaybe (Ledger.serialiseRewardAcnt rewardAddr) stakeCredBs

-- | Insert a stake address if it is not already in the `stake_address` table. Regardless of
-- whether it is newly inserted or it is already there, we retrun the `StakeAddressId`.
insertStakeAddressRefIfMissing ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  DB.TxId ->
  Ledger.Addr StandardCrypto ->
  ReaderT SqlBackend m (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing _trce cache txId addr =
  case addr of
    Ledger.AddrBootstrap {} -> pure Nothing
    Ledger.Addr nw _pcred sref ->
      case sref of
        Ledger.StakeRefBase cred -> do
          Just <$> insertStakeAddressWithCache cache DontCacheNew txId (Shelley.RewardAcnt nw cred)
        Ledger.StakeRefPtr ptr -> do
          queryStakeRefPtr ptr
        Ledger.StakeRefNull -> pure Nothing

insertPoolOwner ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Ledger.Network ->
  DB.PoolUpdateId ->
  DB.TxId ->
  Ledger.KeyHash 'Ledger.Staking StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolOwner cache network poolUpdateId txId skh = do
  saId <- lift $ insertStakeAddressWithCache cache CacheNew txId (Shelley.RewardAcnt network (Ledger.KeyHashObj skh))
  void . lift . DB.insertPoolOwner $
    DB.PoolOwner
      { DB.poolOwnerAddrId = saId
      , DB.poolOwnerPoolUpdateId = poolUpdateId
      }

insertStakeRegistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Shelley.RewardAcnt StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeRegistration epochNo txId idx rewardAccount = do
  -- We by-pass the cache here It's likely it won't hit.
  -- We don't store to the cache yet, since there are many addrresses
  -- which are registered and never used.
  saId <- lift $ insertStakeAddress txId rewardAccount Nothing
  void . lift . DB.insertStakeRegistration $
    DB.StakeRegistration
      { DB.stakeRegistrationAddrId = saId
      , DB.stakeRegistrationCertIndex = idx
      , DB.stakeRegistrationEpochNo = unEpochNo epochNo
      , DB.stakeRegistrationTxId = txId
      }

insertStakeDeregistration ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Ledger.Network ->
  EpochNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeDeregistration cache network epochNo txId idx mRedeemerId cred = do
  scId <- liftLookupFail "insertStakeDeregistration" $ queryStakeAddrWithCache cache EvictAndReturn network cred
  void . lift . DB.insertStakeDeregistration $
    DB.StakeDeregistration
      { DB.stakeDeregistrationAddrId = scId
      , DB.stakeDeregistrationCertIndex = idx
      , DB.stakeDeregistrationEpochNo = unEpochNo epochNo
      , DB.stakeDeregistrationTxId = txId
      , DB.stakeDeregistrationRedeemerId = mRedeemerId
      }

insertDelegation ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Ledger.Network ->
  EpochNo ->
  SlotNo ->
  DB.TxId ->
  Word16 ->
  Maybe DB.RedeemerId ->
  StakeCred ->
  Ledger.KeyHash 'Ledger.StakePool StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegation cache network (EpochNo epoch) slotNo txId idx mRedeemerId cred poolkh = do
  addrId <- liftLookupFail "insertDelegation" $ queryStakeAddrWithCache cache CacheNew network cred
  poolHashId <- liftLookupFail "insertDelegation" $ queryPoolKeyWithCache cache CacheNew poolkh
  void . lift . DB.insertDelegation $
    DB.Delegation
      { DB.delegationAddrId = addrId
      , DB.delegationCertIndex = idx
      , DB.delegationPoolHashId = poolHashId
      , DB.delegationActiveEpochNo = epoch + 2 -- The first epoch where this delegation is valid.
      , DB.delegationTxId = txId
      , DB.delegationSlotNo = unSlotNo slotNo
      , DB.delegationRedeemerId = mRedeemerId
      , DB.delegationDeposit = Nothing
      }

insertDelegationVote ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  StakeCred ->
  DRep StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertDelegationVote cache network txId idx cred drep = do
  addrId <- liftLookupFail "insertDelegation" $ queryStakeAddrWithCache cache CacheNew network cred
  drepId <- lift $ insertDrep drep
  void . lift . DB.insertDelegationVote $
    DB.DelegationVote
      { DB.delegationVoteAddrId = addrId
      , DB.delegationVoteCertIndex = idx
      , DB.delegationVoteDrepHashId = drepId
      , DB.delegationVoteActiveEpochNo = 0 -- TODO: Conway
      , DB.delegationVoteTxId = txId
      , DB.delegationVoteRedeemerId = Nothing
      }

insertMirCert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Ledger.Network ->
  DB.TxId ->
  Word16 ->
  Shelley.MIRCert StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertMirCert _tracer cache network txId idx mcert = do
  case Shelley.mirPot mcert of
    Shelley.ReservesMIR ->
      case Shelley.mirRewards mcert of
        Shelley.StakeAddressesMIR rwds -> mapM_ insertMirReserves $ Map.toList rwds
        Shelley.SendToOppositePotMIR xfrs -> insertPotTransfer (Ledger.toDeltaCoin xfrs)
    Shelley.TreasuryMIR -> do
      case Shelley.mirRewards mcert of
        Shelley.StakeAddressesMIR rwds -> mapM_ insertMirTreasury $ Map.toList rwds
        Shelley.SendToOppositePotMIR xfrs -> insertPotTransfer (invert $ Ledger.toDeltaCoin xfrs)
  where
    insertMirReserves ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Ledger.DeltaCoin) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirReserves (cred, dcoin) = do
      addrId <- lift . insertStakeAddressWithCache cache CacheNew txId $ Generic.annotateStakingCred network cred
      void . lift . DB.insertReserve $
        DB.Reserve
          { DB.reserveAddrId = addrId
          , DB.reserveCertIndex = idx
          , DB.reserveTxId = txId
          , DB.reserveAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertMirTreasury ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Ledger.DeltaCoin) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertMirTreasury (cred, dcoin) = do
      addrId <- lift . insertStakeAddressWithCache cache CacheNew txId $ Generic.annotateStakingCred network cred
      void . lift . DB.insertTreasury $
        DB.Treasury
          { DB.treasuryAddrId = addrId
          , DB.treasuryCertIndex = idx
          , DB.treasuryTxId = txId
          , DB.treasuryAmount = DB.deltaCoinToDbInt65 dcoin
          }

    insertPotTransfer ::
      (MonadBaseControl IO m, MonadIO m) =>
      Ledger.DeltaCoin ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insertPotTransfer dcoinTreasury =
      void . lift . DB.insertPotTransfer $
        DB.PotTransfer
          { DB.potTransferCertIndex = idx
          , DB.potTransferTreasury = DB.deltaCoinToDbInt65 dcoinTreasury
          , DB.potTransferReserves = DB.deltaCoinToDbInt65 (invert dcoinTreasury)
          , DB.potTransferTxId = txId
          }

insertWithdrawals ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  DB.TxId ->
  Map Word64 DB.RedeemerId ->
  Generic.TxWithdrawal ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertWithdrawals _tracer cache txId redeemers txWdrl = do
  addrId <-
    liftLookupFail "insertWithdrawals" $
      queryRewardAccountWithCache cache CacheNew $
        Generic.txwRewardAccount txWdrl
  void . lift . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = Generic.coinToDbLovelace $ Generic.txwAmount txWdrl
      , DB.withdrawalRedeemerId = mlookup (Generic.txwRedeemerIndex txWdrl) redeemers
      }

insertPoolRelay ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.PoolUpdateId ->
  Shelley.StakePoolRelay ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolRelay updateId relay =
  void . lift . DB.insertPoolRelay $
    case relay of
      Shelley.SingleHostAddr mPort mIpv4 mIpv6 ->
        DB.PoolRelay -- An IPv4 and/or IPv6 address
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = textShow <$> strictMaybeToMaybe mIpv4
          , DB.poolRelayIpv6 = textShow <$> strictMaybeToMaybe mIpv6
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Ledger.portToWord16 <$> strictMaybeToMaybe mPort
          }
      Shelley.SingleHostName mPort name ->
        DB.PoolRelay -- An A or AAAA DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Just (Ledger.dnsToText name)
          , DB.poolRelayDnsSrvName = Nothing
          , DB.poolRelayPort = Ledger.portToWord16 <$> strictMaybeToMaybe mPort
          }
      Shelley.MultiHostName name ->
        DB.PoolRelay -- An SRV DNS record
          { DB.poolRelayUpdateId = updateId
          , DB.poolRelayIpv4 = Nothing
          , DB.poolRelayIpv6 = Nothing
          , DB.poolRelayDnsName = Nothing
          , DB.poolRelayDnsSrvName = Just (Ledger.dnsToText name)
          , DB.poolRelayPort = Nothing
          }

insertParamProposal ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  DB.TxId ->
  ParamProposal ->
  ReaderT SqlBackend m DB.ParamProposalId
insertParamProposal blkId txId pp = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (pppCostmdls pp)
  DB.insertParamProposal $
    DB.ParamProposal
      { DB.paramProposalRegisteredTxId = txId
      , DB.paramProposalEpochNo = unEpochNo <$> pppEpochNo pp
      , DB.paramProposalKey = pppKey pp
      , DB.paramProposalMinFeeA = fromIntegral <$> pppMinFeeA pp
      , DB.paramProposalMinFeeB = fromIntegral <$> pppMinFeeB pp
      , DB.paramProposalMaxBlockSize = fromIntegral <$> pppMaxBlockSize pp
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
      , DB.paramProposalProtocolMajor = getVersion . Ledger.pvMajor <$> pppProtocolVersion pp
      , DB.paramProposalProtocolMinor = fromIntegral . Ledger.pvMinor <$> pppProtocolVersion pp
      , DB.paramProposalMinUtxoValue = Generic.coinToDbLovelace <$> pppMinUtxoValue pp
      , DB.paramProposalMinPoolCost = Generic.coinToDbLovelace <$> pppMinPoolCost pp
      , -- New for Alonzo

        DB.paramProposalCoinsPerUtxoSize = Generic.coinToDbLovelace <$> pppCoinsPerUtxo pp
      , DB.paramProposalCostModelId = cmId
      , DB.paramProposalPriceMem = realToFrac <$> pppPriceMem pp
      , DB.paramProposalPriceStep = realToFrac <$> pppPriceStep pp
      , DB.paramProposalMaxTxExMem = DbWord64 <$> pppMaxTxExMem pp
      , DB.paramProposalMaxTxExSteps = DbWord64 <$> pppMaxTxExSteps pp
      , DB.paramProposalMaxBlockExMem = DbWord64 <$> pppMaxBlockExMem pp
      , DB.paramProposalMaxBlockExSteps = DbWord64 <$> pppMaxBlockExSteps pp
      , DB.paramProposalMaxValSize = DbWord64 . fromIntegral <$> pppMaxValSize pp
      , DB.paramProposalCollateralPercent = fromIntegral <$> pppCollateralPercentage pp
      , DB.paramProposalMaxCollateralInputs = fromIntegral <$> pppMaxCollateralInputs pp
      }

insertRedeemer ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  [ExtendedTxOut] ->
  DB.TxId ->
  (Word64, Generic.TxRedeemer) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Word64, DB.RedeemerId)
insertRedeemer tracer groupedOutputs txId (rix, redeemer) = do
  tdId <- insertRedeemerData tracer txId $ Generic.txRedeemerData redeemer
  scriptHash <- findScriptHash
  rid <-
    lift . DB.insertRedeemer $
      DB.Redeemer
        { DB.redeemerTxId = txId
        , DB.redeemerUnitMem = Generic.txRedeemerMem redeemer
        , DB.redeemerUnitSteps = Generic.txRedeemerSteps redeemer
        , DB.redeemerFee = DB.DbLovelace . fromIntegral . unCoin <$> Generic.txRedeemerFee redeemer
        , DB.redeemerPurpose = mkPurpose $ Generic.txRedeemerPurpose redeemer
        , DB.redeemerIndex = Generic.txRedeemerIndex redeemer
        , DB.redeemerScriptHash = scriptHash
        , DB.redeemerRedeemerDataId = tdId
        }
  pure (rix, rid)
  where
    mkPurpose :: Ledger.Tag -> DB.ScriptPurpose
    mkPurpose tag =
      case tag of
        Ledger.Spend -> DB.Spend
        Ledger.Mint -> DB.Mint
        Ledger.Cert -> DB.Cert
        Ledger.Rewrd -> DB.Rewrd

    findScriptHash ::
      (MonadBaseControl IO m, MonadIO m) =>
      ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
    findScriptHash =
      case Generic.txRedeemerScriptHash redeemer of
        Nothing -> pure Nothing
        Just (Right bs) -> pure $ Just bs
        Just (Left txIn) -> resolveScriptHash groupedOutputs txIn

insertDatum ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  DB.TxId ->
  Generic.PlutusData ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.DatumId
insertDatum tracer cache txId txd = do
  mDatumId <- lift $ queryDatum cache $ Generic.txDataHash txd
  case mDatumId of
    Just datumId -> pure datumId
    Nothing -> do
      value <- safeDecodeToJson tracer "insertDatum" $ Generic.txDataValue txd
      lift $
        insertDatumAndCache cache (Generic.txDataHash txd) $
          DB.Datum
            { DB.datumHash = Generic.dataHashToBytes $ Generic.txDataHash txd
            , DB.datumTxId = txId
            , DB.datumValue = value
            , DB.datumBytes = Generic.txDataBytes txd
            }

insertRedeemerData ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  Generic.PlutusData ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.RedeemerDataId
insertRedeemerData tracer txId txd = do
  mRedeemerDataId <- lift $ DB.queryRedeemerData $ Generic.dataHashToBytes $ Generic.txDataHash txd
  case mRedeemerDataId of
    Just redeemerDataId -> pure redeemerDataId
    Nothing -> do
      value <- safeDecodeToJson tracer "insertRedeemerData" $ Generic.txDataValue txd
      lift . DB.insertRedeemerData $
        DB.RedeemerData
          { DB.redeemerDataHash = Generic.dataHashToBytes $ Generic.txDataHash txd
          , DB.redeemerDataTxId = txId
          , DB.redeemerDataValue = value
          , DB.redeemerDataBytes = Generic.txDataBytes txd
          }

prepareTxMetadata ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  Maybe (Map Word64 TxMetadataValue) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.TxMetadata]
prepareTxMetadata tracer txId mmetadata =
  case mmetadata of
    Nothing -> pure []
    Just metadata -> mapM prepare $ Map.toList metadata
  where
    prepare ::
      (MonadBaseControl IO m, MonadIO m) =>
      (Word64, TxMetadataValue) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.TxMetadata
    prepare (key, md) = do
      let jsonbs = LBS.toStrict $ Aeson.encode (metadataValueToJsonNoSchema md)
          singleKeyCBORMetadata = serialiseTxMetadataToCbor $ Map.singleton key md
      mjson <- safeDecodeToJson tracer "prepareTxMetadata" jsonbs
      pure
        DB.TxMetadata
          { DB.txMetadataKey = DbWord64 key
          , DB.txMetadataJson = mjson
          , DB.txMetadataBytes = singleKeyCBORMetadata
          , DB.txMetadataTxId = txId
          }

insertCostModel ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  Map Language Ledger.CostModel ->
  ReaderT SqlBackend m DB.CostModelId
insertCostModel _blkId cms =
  DB.insertCostModel $
    DB.CostModel
      { DB.costModelHash = Crypto.abstractHashToBytes $ Crypto.serializeCborHash $ Ledger.CostModels cms mempty mempty
      , DB.costModelCosts = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode cms
      }

insertEpochParam ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.BlockId ->
  EpochNo ->
  Generic.ProtoParams ->
  Ledger.Nonce ->
  ReaderT SqlBackend m ()
insertEpochParam _tracer blkId (EpochNo epoch) params nonce = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (Generic.ppCostmdls params)
  void . DB.insertEpochParam $
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
      , DB.epochParamExtraEntropy = Generic.nonceToBytes $ Generic.ppExtraEntropy params
      , DB.epochParamProtocolMajor = getVersion $ Ledger.pvMajor (Generic.ppProtocolVersion params)
      , DB.epochParamProtocolMinor = fromIntegral $ Ledger.pvMinor (Generic.ppProtocolVersion params)
      , DB.epochParamMinUtxoValue = Generic.coinToDbLovelace (Generic.ppMinUTxOValue params)
      , DB.epochParamMinPoolCost = Generic.coinToDbLovelace (Generic.ppMinPoolCost params)
      , DB.epochParamNonce = Generic.nonceToBytes nonce
      , DB.epochParamCoinsPerUtxoSize = Generic.coinToDbLovelace <$> Generic.ppCoinsPerUtxo params
      , DB.epochParamCostModelId = cmId
      , DB.epochParamPriceMem = realToFrac <$> Generic.ppPriceMem params
      , DB.epochParamPriceStep = realToFrac <$> Generic.ppPriceStep params
      , DB.epochParamMaxTxExMem = DbWord64 <$> Generic.ppMaxTxExMem params
      , DB.epochParamMaxTxExSteps = DbWord64 <$> Generic.ppMaxTxExSteps params
      , DB.epochParamMaxBlockExMem = DbWord64 <$> Generic.ppMaxBlockExMem params
      , DB.epochParamMaxBlockExSteps = DbWord64 <$> Generic.ppMaxBlockExSteps params
      , DB.epochParamMaxValSize = DbWord64 . fromIntegral <$> Generic.ppMaxValSize params
      , DB.epochParamCollateralPercent = fromIntegral <$> Generic.ppCollateralPercentage params
      , DB.epochParamMaxCollateralInputs = fromIntegral <$> Generic.ppMaxCollateralInputs params
      , DB.epochParamBlockId = blkId
      }

prepareMaTxMint ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  DB.TxId ->
  MultiAsset StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.MaTxMint]
prepareMaTxMint _tracer cache txId (MultiAsset mintMap) =
  concatMapM (lift . prepareOuter) $ Map.toList mintMap
  where
    prepareOuter ::
      (MonadBaseControl IO m, MonadIO m) =>
      (PolicyID StandardCrypto, Map AssetName Integer) ->
      ReaderT SqlBackend m [DB.MaTxMint]
    prepareOuter (policy, aMap) =
      mapM (prepareInner policy) $ Map.toList aMap

    prepareInner ::
      (MonadBaseControl IO m, MonadIO m) =>
      PolicyID StandardCrypto ->
      (AssetName, Integer) ->
      ReaderT SqlBackend m DB.MaTxMint
    prepareInner policy (aname, amount) = do
      maId <- insertMultiAsset cache policy aname
      pure $
        DB.MaTxMint
          { DB.maTxMintIdent = maId
          , DB.maTxMintQuantity = DB.integerToDbInt65 amount
          , DB.maTxMintTxId = txId
          }

prepareMaTxOuts ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Map (PolicyID StandardCrypto) (Map AssetName Integer) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [MissingMaTxOut]
prepareMaTxOuts _tracer cache maMap =
  concatMapM (lift . prepareOuter) $ Map.toList maMap
  where
    prepareOuter ::
      (MonadBaseControl IO m, MonadIO m) =>
      (PolicyID StandardCrypto, Map AssetName Integer) ->
      ReaderT SqlBackend m [MissingMaTxOut]
    prepareOuter (policy, aMap) =
      mapM (prepareInner policy) $ Map.toList aMap

    prepareInner ::
      (MonadBaseControl IO m, MonadIO m) =>
      PolicyID StandardCrypto ->
      (AssetName, Integer) ->
      ReaderT SqlBackend m MissingMaTxOut
    prepareInner policy (aname, amount) = do
      maId <- insertMultiAsset cache policy aname
      pure $
        MissingMaTxOut
          { mmtoIdent = maId
          , mmtoQuantity = DbWord64 (fromIntegral amount)
          }

insertMultiAsset ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  PolicyID StandardCrypto ->
  AssetName ->
  ReaderT SqlBackend m DB.MultiAssetId
insertMultiAsset cache policy aName = do
  mId <- queryMAWithCache cache policy aName
  case mId of
    Right maId -> pure maId
    Left (policyBs, assetNameBs) ->
      DB.insertMultiAssetUnchecked $
        DB.MultiAsset
          { DB.multiAssetPolicy = policyBs
          , DB.multiAssetName = assetNameBs
          , DB.multiAssetFingerprint = DB.unAssetFingerprint (DB.mkAssetFingerprint policyBs assetNameBs)
          }

insertScript ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  Generic.TxScript ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.ScriptId
insertScript tracer txId script = do
  mScriptId <- lift $ DB.queryScript $ Generic.txScriptHash script
  case mScriptId of
    Just scriptId -> pure scriptId
    Nothing -> do
      json <- scriptConvert script
      lift . DB.insertScript $
        DB.Script
          { DB.scriptTxId = txId
          , DB.scriptHash = Generic.txScriptHash script
          , DB.scriptType = Generic.txScriptType script
          , DB.scriptSerialisedSize = Generic.txScriptPlutusSize script
          , DB.scriptJson = json
          , DB.scriptBytes = Generic.txScriptCBOR script
          }
  where
    scriptConvert :: MonadIO m => Generic.TxScript -> m (Maybe Text)
    scriptConvert s =
      maybe (pure Nothing) (safeDecodeToJson tracer "insertScript") (Generic.txScriptJson s)

insertExtraKeyWitness ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  ByteString ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertExtraKeyWitness _tracer txId keyHash = do
  void . lift . DB.insertExtraKeyWitness $
    DB.ExtraKeyWitness
      { DB.extraKeyWitnessHash = keyHash
      , DB.extraKeyWitnessTxId = txId
      }

insertPots ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Shelley.AdaPots ->
  ExceptT e (ReaderT SqlBackend m) ()
insertPots blockId slotNo epochNo pots =
  void . lift $
    DB.insertAdaPots $
      mkAdaPots blockId slotNo epochNo pots

mkAdaPots ::
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Shelley.AdaPots ->
  DB.AdaPots
mkAdaPots blockId slotNo epochNo pots =
  DB.AdaPots
    { DB.adaPotsSlotNo = unSlotNo slotNo
    , DB.adaPotsEpochNo = unEpochNo epochNo
    , DB.adaPotsTreasury = Generic.coinToDbLovelace $ Shelley.treasuryAdaPot pots
    , DB.adaPotsReserves = Generic.coinToDbLovelace $ Shelley.reservesAdaPot pots
    , DB.adaPotsRewards = Generic.coinToDbLovelace $ Shelley.rewardsAdaPot pots
    , DB.adaPotsUtxo = Generic.coinToDbLovelace $ Shelley.utxoAdaPot pots
    , DB.adaPotsDeposits = Generic.coinToDbLovelace $ Shelley.depositsAdaPot pots
    , DB.adaPotsFees = Generic.coinToDbLovelace $ Shelley.feesAdaPot pots
    , DB.adaPotsBlockId = blockId
    }

insertGovernanceAction ::
  (MonadIO m, MonadBaseControl IO m) =>
  Cache ->
  Ledger.Network ->
  DB.BlockId ->
  DB.TxId ->
  (Word64, ProposalProcedure StandardConway) ->
  ReaderT SqlBackend m ()
insertGovernanceAction cache network blkId txId (index, pp) = do
  addrId <-
    insertStakeAddressWithCache cache CacheNew txId $
      Generic.annotateStakingCred network (Ledger.KeyHashObj $ pProcReturnAddr pp)
  votingAnchorId <- whenMaybe (strictMaybeToMaybe $ pProcAnchor pp) $ insertAnchor txId
  mParamProposalId <-
    case pProcGovernanceAction pp of
      ParameterChange pparams ->
        Just <$> insertParamProposal blkId txId (convertConwayParamProposal pparams)
      _ -> pure Nothing
  governanceAction <-
    DB.insertGovernanceAction $
      DB.GovernanceAction
        { DB.governanceActionTxId = txId
        , DB.governanceActionIndex = index
        , DB.governanceActionDeposit = Generic.coinToDbLovelace $ pProcDeposit pp
        , DB.governanceActionReturnAddress = addrId
        , DB.governanceActionVotingAnchorId = votingAnchorId
        , DB.governanceActionType = Generic.toGovAction $ pProcGovernanceAction pp
        , DB.governanceActionDescription = textShow $ pProcGovernanceAction pp
        , DB.governanceActionParamProposal = mParamProposalId
        , DB.governanceActionRatifiedEpoch = Nothing
        , DB.governanceActionEnactedEpoch = Nothing
        , DB.governanceActionDroppedEpoch = Nothing
        , DB.governanceActionExpiredEpoch = Nothing
        }
  case pProcGovernanceAction pp of
    TreasuryWithdrawals mp -> mapM_ (insertTreasuryWithdrawal governanceAction) (Map.toList mp)
    NewCommittee st quorum -> insertNewCommittee governanceAction st quorum
    _ -> pure ()
  where
    insertTreasuryWithdrawal gaId (cred, coin) = do
      addrId <-
        insertStakeAddressWithCache cache CacheNew txId $
          Generic.annotateStakingCred network cred
      DB.insertTreasuryWithdrawal $
        DB.TreasuryWithdrawal
          { DB.treasuryWithdrawalGovernanceActionId = gaId
          , DB.treasuryWithdrawalStakeAddressId = addrId
          , DB.treasuryWithdrawalAmount = Generic.coinToDbLovelace coin
          }

    insertNewCommittee gaId st quorum = do
      void . DB.insertNewCommittee $
        DB.NewCommittee
          { DB.newCommitteeGovernanceActionId = gaId
          , DB.newCommitteeQuorum = realToFrac quorum
          , DB.newCommitteeMembers = textShow st
          }

insertAnchor :: (MonadIO m, MonadBaseControl IO m) => DB.TxId -> Anchor StandardCrypto -> ReaderT SqlBackend m DB.VotingAnchorId
insertAnchor txId anchor =
  DB.insertAnchor $
    DB.VotingAnchor
      { DB.votingAnchorTxId = txId
      , DB.votingAnchorUrl = DB.VoteUrl $ Ledger.urlToText $ anchorUrl anchor
      , DB.votingAnchorDataHash = Generic.achorHashToBytes $ anchorDataHash anchor
      }

insertVotingProcedure ::
  (MonadIO m, MonadBaseControl IO m) =>
  Cache ->
  DB.TxId ->
  (Word16, VotingProcedure StandardConway) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertVotingProcedure cache txId (index, vp) = do
  govActionId <- resolveGovernanceAction (vProcGovActionId vp)
  votingAnchorId <- whenMaybe (strictMaybeToMaybe $ vProcAnchor vp) $ lift . insertAnchor txId
  (mComitteeVoter, mDRepVoter, mStakePoolVoter) <- case vProcVoter vp of
    CommitteeVoter cred ->
      pure (Just $ Generic.unCredentialHash cred, Nothing, Nothing)
    DRepVoter cred -> do
      drep <- lift $ insertCredDrepHash cred
      pure (Nothing, Just drep, Nothing)
    StakePoolVoter poolkh -> do
      poolHashId <- liftLookupFail "insertVotingProcedure" $ queryPoolKeyWithCache cache CacheNew poolkh
      pure (Nothing, Nothing, Just poolHashId)
  void . lift . DB.insertVotingProcedure $
    DB.VotingProcedure
      { DB.votingProcedureTxId = txId
      , DB.votingProcedureIndex = index
      , DB.votingProcedureGovernanceActionId = govActionId
      , DB.votingProcedureComitteeVoter = mComitteeVoter
      , DB.votingProcedureDrepVoter = mDRepVoter
      , DB.votingProcedurePoolVoter = mStakePoolVoter
      , DB.votingProcedureVoterRole = Generic.toVoterRole $ vProcVoter vp
      , DB.votingProcedureVote = Generic.toVote $ vProcVote vp
      , DB.votingProcedureVotingAnchorId = votingAnchorId
      }

resolveGovernanceAction ::
  MonadIO m =>
  GovernanceActionId StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.GovernanceActionId
resolveGovernanceAction gaId = do
  gaTxId <-
    liftLookupFail "resolveGovernanceAction.queryTxId" $
      DB.queryTxId $
        Generic.unTxHash $
          gaidTxId gaId
  let (GovernanceActionIx index) = gaidGovActionIx gaId
  liftLookupFail "resolveGovernanceAction.queryGovernanceActionId" $
    DB.queryGovernanceActionId gaTxId index

insertDrep :: (MonadBaseControl IO m, MonadIO m) => DRep StandardCrypto -> ReaderT SqlBackend m DB.DrepHashId
insertDrep = \case
  DRepCredential cred -> insertCredDrepHash cred
  DRepAlwaysAbstain ->
    DB.insertDrepHash
      DB.DrepHash
        { DB.drepHashRaw = Nothing
        , DB.drepHashView = Just "AlwaysAbstain"
        , DB.drepHashHasScript = False
        }
  DRepAlwaysNoConfidence ->
    DB.insertDrepHash
      DB.DrepHash
        { DB.drepHashRaw = Nothing
        , DB.drepHashView = Just "AlwaysNoConfidence"
        , DB.drepHashHasScript = False
        }

insertCredDrepHash :: (MonadBaseControl IO m, MonadIO m) => Ledger.Credential 'Voting StandardCrypto -> ReaderT SqlBackend m DB.DrepHashId
insertCredDrepHash cred = do
  DB.insertDrepHash
    DB.DrepHash
      { DB.drepHashRaw = Just $ Generic.unCredentialHash cred
      , DB.drepHashView = Nothing
      , DB.drepHashHasScript = Generic.hasCredScript cred
      }
