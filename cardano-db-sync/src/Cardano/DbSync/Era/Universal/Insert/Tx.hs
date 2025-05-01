{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.Tx (
  prepareTxGrouped,
  insertTxRest,
  prepareTxOut,
) where

import Cardano.BM.Trace (Trace)
import Cardano.Db (DbWord64 (..))
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (insertAddressUsingCache, queryOrInsertSyncMultiAsset, queryTxIdWithCache, tryUpdateCacheTx)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Metadata (TxMetadataValue (..), metadataValueToJsonNoSchema)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types (TxInKey (..))
import Cardano.DbSync.Era.Universal.Insert.Certificate (insertCertificate)
import Cardano.DbSync.Era.Universal.Insert.GovAction (
  insertGovActionProposal,
  insertParamProposal,
  insertVotingProcedures,
 )
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Universal.Insert.Other (
  insertDatum,
  insertExtraKeyWitness,
  insertRedeemer,
  insertScript,
  insertWithdrawals,
  queryOrInsertStakeRef,
 )
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (ApplyResult (..), getGovExpiresAt)
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Cbor (serialiseTxMetadataToCbor)
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Prelude
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

--------------------------------------------------------------------------------------
-- INSERT TX
--------------------------------------------------------------------------------------

prepareTxGrouped ::
  (MonadIO m, MonadBaseControl IO m) =>
  SyncEnv ->
  [(TxIdLedger, DB.TxId)] ->
  DB.BlockId ->
  DB.TxId ->
  Generic.Tx ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ((DB.TxId, DB.Tx, Generic.Tx), BlockGroupedData)
prepareTxGrouped syncEnv txHashes blkId txId tx = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  txDb <- lift $ prepareTx syncEnv blkId tx
  txIns <- whenTrueMempty disInOut $ mapM (prepareTxIn syncEnv txHashes txId) (Generic.txInputs tx)
  txOuts <- whenTrueMempty disInOut $ mapM (prepareTxOut syncEnv iopts (txId, Generic.txHash tx)) (Generic.txOutputs tx)
  txMetadata <-
    whenFalseMempty (ioMetadata iopts && Generic.txValidContract tx) $
      prepareTxMetadata
        tracer
        txId
        iopts
        (Generic.txMetadata tx)
  maTxMint <-
    whenFalseMempty (ioMultiAssets iopts && Generic.txValidContract tx) $
      prepareMaTxMint syncEnv txId $
        Generic.txMint tx
  pure ((txId, txDb, tx), BlockGroupedData txIns txOuts txMetadata maTxMint 0 outSum)
  where
    tracer = getTrace syncEnv
    iopts = getInsertOptions syncEnv
    outSum = fromIntegral $ unCoin $ Generic.txOutSum tx

prepareTx ::
  MonadIO m =>
  SyncEnv ->
  DB.BlockId ->
  Generic.Tx ->
  m DB.Tx
prepareTx syncEnv blkId tx = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  let fees = case (unCoin <$> Generic.txFees tx, disInOut) of
        (_, True) -> 0
        (Nothing, _) -> 0
        (Just fees', _) -> fromIntegral fees'
  pure
    DB.Tx
      { DB.txHash = txHash
      , DB.txBlockId = blkId
      , DB.txBlockIndex = Generic.txBlockIndex tx
      , DB.txOutSum = DB.DbLovelace outSum
      , DB.txFee = DB.DbLovelace fees -- may be wrong if txValidContract is False or outsputs are disabled
      , DB.txDeposit = Nothing -- leaving this Nothing for now
      , DB.txSize = Generic.txSize tx
      , DB.txInvalidBefore = DbWord64 . unSlotNo <$> Generic.txInvalidBefore tx
      , DB.txInvalidHereafter = DbWord64 . unSlotNo <$> Generic.txInvalidHereafter tx
      , DB.txValidContract = Generic.txValidContract tx
      , DB.txScriptSize = sum $ Generic.txScriptSizes tx
      , DB.txTreasuryDonation = DB.DbLovelace (fromIntegral treasuryDonation)
      }
  where
    txHash = Generic.txHash tx
    outSum = fromIntegral $ unCoin $ Generic.txOutSum tx
    treasuryDonation = unCoin $ Generic.txTreasuryDonation tx

prepareTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  [(TxIdLedger, DB.TxId)] ->
  DB.TxId ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ExtendedTxIn
prepareTxIn syncEnv txHashes txId txIn = do
  txOutTxId <- prepareResolveTxInputs syncEnv txHashes txInKey
  pure $ ExtendedTxIn (txInDb txOutTxId) (Left txInKey)
  where
    txInKey = Generic.txInKey txIn
    txInDb txOutTxId =
      DB.TxIn
        { DB.txInTxInId = txId
        , DB.txInTxOutId = txOutTxId
        , DB.txInTxOutIndex = fromIntegral $ Generic.txInIndex (Generic.txInKey txIn)
        , DB.txInRedeemerId = Nothing -- Remove or fix later https://github.com/IntersectMBO/cardano-db-sync/issues/1746
        }

insertTxRest ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.BlockId ->
  EpochNo ->
  SlotNo ->
  ApplyResult ->
  DB.TxId ->
  DB.Tx ->
  Generic.Tx ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxRest syncEnv blkId epochNo slotNo applyResult txId _ tx = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  tryUpdateCacheTx cache (Generic.txLedgerTxId tx) txId
  when (ioTxCBOR iopts) $ do
    void
      . lift
      . DB.insertTxCBOR
      $ DB.TxCbor
        { DB.txCborTxId = txId
        , DB.txCborBytes = Generic.txCBOR tx
        }

  when (Generic.txValidContract tx) $ do
    -- The following operations only happen if the script passes stage 2 validation (or the tx has
    -- no script).
    !redeemers <-
      Map.fromList
        <$> whenFalseMempty
          (ioPlutusExtra iopts)
          (mapM (insertRedeemer syncEnv disInOut [] txId) (Generic.txRedeemer tx)) -- TODO leaving this empty for now
    when (ioPlutusExtra iopts) $ do
      mapM_ (insertDatum syncEnv txId) (Generic.txData tx)
      mapM_ (insertCollateralTxIn syncEnv txId) (Generic.txInKey <$> Generic.txCollateralInputs tx)
      mapM_ (insertReferenceTxIn syncEnv txId) (Generic.txInKey <$> Generic.txReferenceInputs tx)
      mapM_ (insertCollateralTxOut syncEnv iopts (txId, Generic.txHash tx)) (Generic.txCollateralOutputs tx)
    mapM_
      (insertCertificate syncEnv isMember mDeposits blkId txId epochNo slotNo redeemers)
      $ Generic.txCertificates tx
    when (ioShelley iopts) $
      mapM_ (insertWithdrawals syncEnv txId redeemers) $
        Generic.txWithdrawals tx
    when (ioShelley iopts) $
      mapM_ (lift . insertParamProposal blkId txId) $
        Generic.txParamProposal tx
    when (ioPlutusExtra iopts) $
      mapM_ (lift . insertScript syncEnv txId) $
        Generic.txScripts tx
    when (ioPlutusExtra iopts) $
      mapM_ (insertExtraKeyWitness tracer txId) $
        Generic.txExtraKeyWitnesses tx
    when (ioGov iopts) $ do
      mapM_ (insertGovActionProposal syncEnv blkId txId (getGovExpiresAt applyResult epochNo) (apGovActionState applyResult)) $ zip [0 ..] (Generic.txProposalProcedure tx)
      mapM_ (insertVotingProcedures syncEnv blkId txId) (Generic.txVotingProcedure tx)
  where
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    mDeposits = maybeFromStrict $ apDeposits applyResult
    isMember poolId = Set.member poolId (apPoolsRegistered applyResult)

--------------------------------------------------------------------------------------
-- INSERT TXOUT
--------------------------------------------------------------------------------------
prepareTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  InsertOptions ->
  (DB.TxId, ByteString) ->
  Generic.TxOut ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (ExtendedTxOut, [MissingMaTxOut])
prepareTxOut syncEnv iopts (txId, txHash) (Generic.TxOut index addr value maMap mScript dt) = do
  mSaId <- lift $ queryOrInsertStakeRef syncEnv addr
  mDatumId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      Generic.whenInlineDatum dt $
        insertDatum syncEnv txId
  mScriptId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      whenMaybe mScript $
        lift . insertScript syncEnv txId
  !txOut <-
    case ioTxOutTableType iopts of
      DB.TxOutCore ->
        pure $
          DB.CTxOutW $
            C.TxOut
              { C.txOutAddress = addrText
              , C.txOutAddressHasScript = hasScript
              , C.txOutConsumedByTxId = Nothing
              , C.txOutDataHash = Generic.dataHashToBytes <$> Generic.getTxOutDatumHash dt
              , C.txOutIndex = index
              , C.txOutInlineDatumId = mDatumId
              , C.txOutPaymentCred = Generic.maybePaymentCred addr
              , C.txOutReferenceScriptId = mScriptId
              , C.txOutStakeAddressId = mSaId
              , C.txOutTxId = txId
              , C.txOutValue = Generic.coinToDbLovelace value
              }
      DB.TxOutVariantAddress -> do
        let vAddress =
              V.Address
                { V.addressAddress = Generic.renderAddress addr
                , V.addressRaw = Ledger.serialiseAddr addr
                , V.addressHasScript = hasScript
                , V.addressPaymentCred = Generic.maybePaymentCred addr
                , V.addressStakeAddressId = mSaId
                }
        addrId <- lift $ insertAddressUsingCache cache UpdateCache (Ledger.serialiseAddr addr) vAddress
        pure $
          DB.VTxOutW
            (mkTxOutVariant mSaId addrId mDatumId mScriptId)
            (Just vAddress)
  -- TODO: Unsure about what we should return here for eutxo
  let !eutxo =
        case ioTxOutTableType iopts of
          DB.TxOutCore -> ExtendedTxOut txHash txOut
          DB.TxOutVariantAddress -> ExtendedTxOut txHash txOut
  !maTxOuts <- whenFalseMempty (ioMultiAssets iopts) $ prepareMaTxOuts syncEnv maMap
  pure (eutxo, maTxOuts)
  where
    hasScript :: Bool
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

    addrText :: Text
    addrText = Generic.renderAddress addr

    mkTxOutVariant :: Maybe DB.StakeAddressId -> V.AddressId -> Maybe DB.DatumId -> Maybe DB.ScriptId -> V.TxOut
    mkTxOutVariant mSaId addrId mDatumId mScriptId =
      V.TxOut
        { V.txOutAddressId = addrId
        , V.txOutConsumedByTxId = Nothing
        , V.txOutDataHash = Generic.dataHashToBytes <$> Generic.getTxOutDatumHash dt
        , V.txOutIndex = index
        , V.txOutInlineDatumId = mDatumId
        , V.txOutReferenceScriptId = mScriptId
        , V.txOutTxId = txId
        , V.txOutValue = Generic.coinToDbLovelace value
        , V.txOutStakeAddressId = mSaId
        }

    cache = envCache syncEnv

prepareTxMetadata ::
  MonadIO m =>
  Trace IO Text ->
  DB.TxId ->
  InsertOptions ->
  Maybe (Map Word64 TxMetadataValue) ->
  m [DB.TxMetadata]
prepareTxMetadata tracer txId inOpts mmetadata = do
  case mmetadata of
    Nothing -> pure []
    Just metadata -> mapMaybeM prepare $ Map.toList metadata
  where
    prepare ::
      MonadIO m =>
      (Word64, TxMetadataValue) ->
      m (Maybe DB.TxMetadata)
    prepare (key, md) = do
      case ioKeepMetadataNames inOpts of
        Strict.Just metadataNames -> do
          let isMatchingKey = key `elem` metadataNames
          if isMatchingKey
            then mkDbTxMetadata (key, md)
            else pure Nothing
        -- if we have TxMetadata and keepMetadataNames is Nothing then we want to keep all metadata
        Strict.Nothing -> mkDbTxMetadata (key, md)

    mkDbTxMetadata ::
      MonadIO m =>
      (Word64, TxMetadataValue) ->
      m (Maybe DB.TxMetadata)
    mkDbTxMetadata (key, md) = do
      let jsonbs = LBS.toStrict $ Aeson.encode (metadataValueToJsonNoSchema md)
          singleKeyCBORMetadata = serialiseTxMetadataToCbor $ Map.singleton key md
      mjson <- safeDecodeToJson tracer "prepareTxMetadata: Column 'json' in table 'metadata' " jsonbs
      pure $
        Just $
          DB.TxMetadata
            { DB.txMetadataKey = DbWord64 key
            , DB.txMetadataJson = mjson
            , DB.txMetadataBytes = singleKeyCBORMetadata
            , DB.txMetadataTxId = txId
            }

--------------------------------------------------------------------------------------
-- INSERT MULTI ASSET
--------------------------------------------------------------------------------------
prepareMaTxMint ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.TxId ->
  MultiAsset StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.MaTxMint]
prepareMaTxMint syncEnv txId (MultiAsset mintMap) =
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
      maId <- queryOrInsertSyncMultiAsset syncEnv policy aname
      pure $
        DB.MaTxMint
          { DB.maTxMintIdent = maId
          , DB.maTxMintQuantity = DB.integerToDbInt65 amount
          , DB.maTxMintTxId = txId
          }

prepareMaTxOuts ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Map (PolicyID StandardCrypto) (Map AssetName Integer) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [MissingMaTxOut]
prepareMaTxOuts syncEnv maMap =
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
      maId <- queryOrInsertSyncMultiAsset syncEnv policy aname
      pure $
        MissingMaTxOut
          { mmtoIdent = maId
          , mmtoQuantity = DbWord64 (fromIntegral amount)
          }

--------------------------------------------------------------------------------------
-- INSERT COLLATERAL
--------------------------------------------------------------------------------------
insertCollateralTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  InsertOptions ->
  (DB.TxId, ByteString) ->
  Generic.TxOut ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxOut syncEnv iopts (txId, _txHash) (Generic.TxOut index addr value maMap mScript dt) = do
  mSaId <- lift $ queryOrInsertStakeRef syncEnv addr
  mDatumId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      Generic.whenInlineDatum dt $
        insertDatum syncEnv txId
  mScriptId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      whenMaybe mScript $
        lift . insertScript syncEnv txId
  _ <-
    case ioTxOutTableType iopts of
      DB.TxOutCore -> do
        lift
          . DB.insertCollateralTxOut
          $ DB.CCollateralTxOutW
          $ C.CollateralTxOut
            { C.collateralTxOutTxId = txId
            , C.collateralTxOutIndex = index
            , C.collateralTxOutAddress = Generic.renderAddress addr
            , C.collateralTxOutAddressHasScript = hasScript
            , C.collateralTxOutPaymentCred = Generic.maybePaymentCred addr
            , C.collateralTxOutStakeAddressId = mSaId
            , C.collateralTxOutValue = Generic.coinToDbLovelace value
            , C.collateralTxOutDataHash = Generic.dataHashToBytes <$> Generic.getTxOutDatumHash dt
            , C.collateralTxOutMultiAssetsDescr = textShow maMap
            , C.collateralTxOutInlineDatumId = mDatumId
            , C.collateralTxOutReferenceScriptId = mScriptId
            }
      DB.TxOutVariantAddress -> do
        let vAddress =
              V.Address
                { V.addressAddress = Generic.renderAddress addr
                , V.addressRaw = Ledger.serialiseAddr addr
                , V.addressHasScript = hasScript
                , V.addressPaymentCred = Generic.maybePaymentCred addr
                , V.addressStakeAddressId = mSaId
                }
        addrId <- lift $ insertAddressUsingCache cache UpdateCache (Ledger.serialiseAddr addr) vAddress
        lift
          . DB.insertCollateralTxOut
          $ DB.VCollateralTxOutW
          $ V.CollateralTxOut
            { V.collateralTxOutTxId = txId
            , V.collateralTxOutIndex = index
            , V.collateralTxOutAddressId = addrId
            , V.collateralTxOutStakeAddressId = mSaId
            , V.collateralTxOutValue = Generic.coinToDbLovelace value
            , V.collateralTxOutDataHash = Generic.dataHashToBytes <$> Generic.getTxOutDatumHash dt
            , V.collateralTxOutMultiAssetsDescr = textShow maMap
            , V.collateralTxOutInlineDatumId = mDatumId
            , V.collateralTxOutReferenceScriptId = mScriptId
            }
  pure ()
  where
    -- TODO: Is there any reason to add new tables for collateral multi-assets/multi-asset-outputs
    hasScript :: Bool
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

    cache = envCache syncEnv

insertCollateralTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.TxId ->
  Generic.TxInKey ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxIn syncEnv txInId txIn = do
  let txId = txInTxId txIn
  txOutId <- liftLookupFail "insertCollateralTxIn" $ queryTxIdWithCache (envCache syncEnv) txId
  void
    . lift
    . DB.insertCollateralTxIn
    $ DB.CollateralTxIn
      { DB.collateralTxInTxInId = txInId
      , DB.collateralTxInTxOutId = txOutId
      , DB.collateralTxInTxOutIndex = fromIntegral (txInIndex txIn)
      }

insertReferenceTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.TxId ->
  Generic.TxInKey ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertReferenceTxIn syncEnv txInId txIn = do
  let txId = txInTxId txIn
  txOutId <- liftLookupFail "insertReferenceTxIn" $ queryTxIdWithCache (envCache syncEnv) txId
  void
    . lift
    . DB.insertReferenceTxIn
    $ DB.ReferenceTxIn
      { DB.referenceTxInTxInId = txInId
      , DB.referenceTxInTxOutId = txOutId
      , DB.referenceTxInTxOutIndex = fromIntegral (txInIndex txIn)
      }
