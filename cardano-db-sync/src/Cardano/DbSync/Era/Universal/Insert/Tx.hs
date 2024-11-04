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
  insertTx,
  insertTxOut,
) where

import Cardano.BM.Trace (Trace)
import Cardano.Db (DbLovelace (..), DbWord64 (..))
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (queryTxIdWithCache, tryUpdateCacheTx)
import Cardano.DbSync.Cache.Types (CacheStatus (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Metadata (TxMetadataValue (..), metadataValueToJsonNoSchema)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types (TxIn (..))
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
  insertMultiAsset,
  insertRedeemer,
  insertScript,
  insertStakeAddressRefIfMissing,
  insertWithdrawals,
 )
import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember)
import Cardano.DbSync.Era.Util (liftLookupFail, safeDecodeToJson)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (ApplyResult (..), getGovExpiresAt, lookupDepositsMap)
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
import qualified Data.Strict.Maybe as Strict
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

--------------------------------------------------------------------------------------
-- INSERT TX
--------------------------------------------------------------------------------------
insertTx ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  IsPoolMember ->
  DB.BlockId ->
  EpochNo ->
  SlotNo ->
  ApplyResult ->
  Word64 ->
  Generic.Tx ->
  BlockGroupedData ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) BlockGroupedData
insertTx syncEnv isMember blkId epochNo slotNo applyResult blockIndex tx grouped = do
  let !txHash = Generic.txHash tx
  let !mdeposits = if not (Generic.txValidContract tx) then Just (Coin 0) else lookupDepositsMap txHash (apDepositsMap applyResult)
  let !outSum = fromIntegral $ unCoin $ Generic.txOutSum tx
      !withdrawalSum = fromIntegral $ unCoin $ Generic.txWithdrawalSum tx
      !treasuryDonation = unCoin $ Generic.txTreasuryDonation tx
      hasConsumed = getHasConsumedOrPruneTxOut syncEnv
      txIn = Generic.txInputs tx
  disInOut <- liftIO $ getDisableInOutState syncEnv
  -- In some txs and with specific configuration we may be able to find necessary data within the tx body.
  -- In these cases we can avoid expensive queries.
  (resolvedInputs, fees', deposits) <- case (disInOut, mdeposits, unCoin <$> Generic.txFees tx) of
    (True, _, _) -> pure ([], 0, unCoin <$> mdeposits)
    (_, Just deposits, Just fees) -> do
      (resolvedInputs, _) <- splitLast <$> mapM (resolveTxInputs syncEnv hasConsumed False (fst <$> groupedTxOut grouped)) txIn
      pure (resolvedInputs, fees, Just (unCoin deposits))
    (_, Nothing, Just fees) -> do
      (resolvedInputs, amounts) <- splitLast <$> mapM (resolveTxInputs syncEnv hasConsumed False (fst <$> groupedTxOut grouped)) txIn
      if any isNothing amounts
        then pure (resolvedInputs, fees, Nothing)
        else
          let !inSum = sum $ map unDbLovelace $ catMaybes amounts
           in pure (resolvedInputs, fees, Just $ fromIntegral (inSum + withdrawalSum) - fromIntegral outSum - fees - treasuryDonation)
    (_, _, Nothing) -> do
      -- Nothing in fees means a phase 2 failure
      (resolvedInsFull, amounts) <- splitLast <$> mapM (resolveTxInputs syncEnv hasConsumed True (fst <$> groupedTxOut grouped)) txIn
      let !inSum = sum $ map unDbLovelace $ catMaybes amounts
          !diffSum = if inSum >= outSum then inSum - outSum else 0
          !fees = maybe diffSum (fromIntegral . unCoin) (Generic.txFees tx)
      pure (resolvedInsFull, fromIntegral fees, Just 0)
  let fees = fromIntegral fees'
  -- Insert transaction and get txId from the DB.
  !txId <-
    lift
      . DB.insertTx
      $ DB.Tx
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
        , DB.txTreasuryDonation = DB.DbLovelace (fromIntegral treasuryDonation)
        }

  tryUpdateCacheTx cache (Generic.txLedgerTxId tx) txId
  when (ioTxCBOR iopts) $ do
    void
      . lift
      . DB.insertTxCBOR
      $ DB.TxCbor
        { DB.txCborTxId = txId
        , DB.txCborBytes = Generic.txCBOR tx
        }

  if not (Generic.txValidContract tx)
    then do
      !txOutsGrouped <- mapM (insertTxOut tracer cache iopts (txId, txHash)) (Generic.txOutputs tx)

      let !txIns = map (prepareTxIn txId Map.empty) resolvedInputs
      -- There is a custom semigroup instance for BlockGroupedData which uses addition for the values `fees` and `outSum`.
      -- Same happens bellow on last line of this function.
      pure (grouped <> BlockGroupedData txIns txOutsGrouped [] [] fees outSum)
    else do
      -- The following operations only happen if the script passes stage 2 validation (or the tx has
      -- no script).
      !txOutsGrouped <- mapM (insertTxOut tracer cache iopts (txId, txHash)) (Generic.txOutputs tx)

      !redeemers <-
        Map.fromList
          <$> whenFalseMempty
            (ioPlutusExtra iopts)
            (mapM (insertRedeemer syncEnv disInOut (fst <$> groupedTxOut grouped) txId) (Generic.txRedeemer tx))

      when (ioPlutusExtra iopts) $ do
        mapM_ (insertDatum tracer cache txId) (Generic.txData tx)
        mapM_ (insertCollateralTxIn syncEnv tracer txId) (Generic.txCollateralInputs tx)
        mapM_ (insertReferenceTxIn syncEnv tracer txId) (Generic.txReferenceInputs tx)
        mapM_ (insertCollateralTxOut tracer cache iopts (txId, txHash)) (Generic.txCollateralOutputs tx)

      txMetadata <-
        whenFalseMempty (ioMetadata iopts) $
          insertTxMetadata
            tracer
            txId
            iopts
            (Generic.txMetadata tx)
      mapM_
        (insertCertificate syncEnv isMember mDeposits blkId txId epochNo slotNo redeemers)
        $ Generic.txCertificates tx
      when (ioShelley iopts) $
        mapM_ (insertWithdrawals tracer cache txId redeemers) $
          Generic.txWithdrawals tx
      when (ioShelley iopts) $
        mapM_ (lift . insertParamProposal blkId txId) $
          Generic.txParamProposal tx

      maTxMint <-
        whenFalseMempty (ioMultiAssets iopts) $
          insertMaTxMint tracer cache txId $
            Generic.txMint tx

      when (ioPlutusExtra iopts) $
        mapM_ (lift . insertScript tracer txId) $
          Generic.txScripts tx

      when (ioPlutusExtra iopts) $
        mapM_ (insertExtraKeyWitness tracer txId) $
          Generic.txExtraKeyWitnesses tx

      when (ioGov iopts) $ do
        mapM_ (insertGovActionProposal tracer cache blkId txId (getGovExpiresAt applyResult epochNo) (apGovActionState applyResult)) $ zip [0 ..] (Generic.txProposalProcedure tx)
        mapM_ (insertVotingProcedures tracer cache blkId txId) (Generic.txVotingProcedure tx)

      let !txIns = map (prepareTxIn txId redeemers) resolvedInputs
      pure (grouped <> BlockGroupedData txIns txOutsGrouped txMetadata maTxMint fees outSum)
  where
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv
    mDeposits = maybeFromStrict $ apDeposits applyResult

--------------------------------------------------------------------------------------
-- INSERT TXOUT
--------------------------------------------------------------------------------------
insertTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  InsertOptions ->
  (DB.TxId, ByteString) ->
  Generic.TxOut ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (ExtendedTxOut, [MissingMaTxOut])
insertTxOut tracer cache iopts (txId, txHash) (Generic.TxOut index addr value maMap mScript dt) = do
  mSaId <- lift $ insertStakeAddressRefIfMissing tracer cache addr
  mDatumId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      Generic.whenInlineDatum dt $
        insertDatum tracer cache txId
  mScriptId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      whenMaybe mScript $
        lift . insertScript tracer txId
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
        addrId <- lift $ insertAddress addr vAddress
        pure $
          DB.VTxOutW
            (mkTxOutVariant mSaId addrId mDatumId mScriptId)
            (Just vAddress)
  -- TODO: Unsure about what we should return here for eutxo
  let !eutxo =
        case ioTxOutTableType iopts of
          DB.TxOutCore -> ExtendedTxOut txHash txOut
          DB.TxOutVariantAddress -> ExtendedTxOut txHash txOut
  !maTxOuts <- whenFalseMempty (ioMultiAssets iopts) $ insertMaTxOuts tracer cache maMap
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

insertAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  Ledger.Addr StandardCrypto ->
  V.Address ->
  ReaderT SqlBackend m V.AddressId
insertAddress address vAddress = do
  mAddrId <- DB.queryAddressId addrRaw
  case mAddrId of
    Nothing -> DB.insertAddress vAddress
    Just addrId -> pure addrId
  where
    addrRaw = Ledger.serialiseAddr address

insertTxMetadata ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  InsertOptions ->
  Maybe (Map Word64 TxMetadataValue) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.TxMetadata]
insertTxMetadata tracer txId inOpts mmetadata = do
  case mmetadata of
    Nothing -> pure []
    Just metadata -> mapMaybeM prepare $ Map.toList metadata
  where
    prepare ::
      (MonadBaseControl IO m, MonadIO m) =>
      (Word64, TxMetadataValue) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.TxMetadata)
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
      (MonadBaseControl IO m, MonadIO m) =>
      (Word64, TxMetadataValue) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.TxMetadata)
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
insertMaTxMint ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  DB.TxId ->
  MultiAsset StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.MaTxMint]
insertMaTxMint _tracer cache txId (MultiAsset mintMap) =
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

insertMaTxOuts ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  Map (PolicyID StandardCrypto) (Map AssetName Integer) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [MissingMaTxOut]
insertMaTxOuts _tracer cache maMap =
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

--------------------------------------------------------------------------------------
-- INSERT COLLATERAL
--------------------------------------------------------------------------------------
insertCollateralTxOut ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  InsertOptions ->
  (DB.TxId, ByteString) ->
  Generic.TxOut ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxOut tracer cache iopts (txId, _txHash) (Generic.TxOut index addr value maMap mScript dt) = do
  mSaId <- lift $ insertStakeAddressRefIfMissing tracer cache addr
  mDatumId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      Generic.whenInlineDatum dt $
        insertDatum tracer cache txId
  mScriptId <-
    whenFalseEmpty (ioPlutusExtra iopts) Nothing $
      whenMaybe mScript $
        lift . insertScript tracer txId
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
        addrId <- lift $ insertAddress addr vAddress
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

insertCollateralTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  DB.TxId ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertCollateralTxIn syncEnv _tracer txInId txIn = do
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
  Trace IO Text ->
  DB.TxId ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertReferenceTxIn syncEnv _tracer txInId txIn = do
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

--------------------------------------------------------------------------------------
-- Prepare TX-IN
--------------------------------------------------------------------------------------
prepareTxIn ::
  DB.TxId ->
  Map Word64 DB.RedeemerId ->
  (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW) ->
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
