{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.Other (
  toDouble,
  insertRedeemer,
  insertDatum,
  insertWithdrawals,
  insertRedeemerData,
  insertStakeAddressRefIfMissing,
  insertMultiAsset,
  insertScript,
  insertExtraKeyWitness,
) where

import Cardano.BM.Trace (Trace)
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache (insertDatumAndCache, queryDatum, queryMAWithCache, queryOrInsertRewardAccount, queryOrInsertStakeAddress)
import Cardano.DbSync.Cache.Types (Cache (..), CacheNew (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Query (queryStakeRefPtr)
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Util (safeDecodeToJson)
import Cardano.DbSync.Error
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..))
import Cardano.Prelude
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

--------------------------------------------------------------------------------------------
-- Insert Redeemer
--------------------------------------------------------------------------------------------
insertRedeemer ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Bool ->
  [ExtendedTxOut] ->
  DB.TxId ->
  (Word64, Generic.TxRedeemer) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Word64, DB.RedeemerId)
insertRedeemer tracer disInOut groupedOutputs txId (rix, redeemer) = do
  tdId <- insertRedeemerData tracer txId $ Generic.txRedeemerData redeemer
  scriptHash <- findScriptHash
  rid <-
    lift
      . DB.insertRedeemer
      $ DB.Redeemer
        { DB.redeemerTxId = txId
        , DB.redeemerUnitMem = Generic.txRedeemerMem redeemer
        , DB.redeemerUnitSteps = Generic.txRedeemerSteps redeemer
        , DB.redeemerFee = DB.DbLovelace . fromIntegral . unCoin <$> Generic.txRedeemerFee redeemer
        , DB.redeemerPurpose = Generic.txRedeemerPurpose redeemer
        , DB.redeemerIndex = Generic.txRedeemerIndex redeemer
        , DB.redeemerScriptHash = scriptHash
        , DB.redeemerRedeemerDataId = tdId
        }
  pure (rix, rid)
  where
    findScriptHash ::
      (MonadBaseControl IO m, MonadIO m) =>
      ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
    findScriptHash =
      case (disInOut, Generic.txRedeemerScriptHash redeemer) of
        (True, _) -> pure Nothing
        (_, Nothing) -> pure Nothing
        (_, Just (Right bs)) -> pure $ Just bs
        (_, Just (Left txIn)) -> resolveScriptHash groupedOutputs txIn

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
      lift
        . DB.insertRedeemerData
        $ DB.RedeemerData
          { DB.redeemerDataHash = Generic.dataHashToBytes $ Generic.txDataHash txd
          , DB.redeemerDataTxId = txId
          , DB.redeemerDataValue = value
          , DB.redeemerDataBytes = Generic.txDataBytes txd
          }

--------------------------------------------------------------------------------------------
-- Insert Others
--------------------------------------------------------------------------------------------
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
    lift $ queryOrInsertRewardAccount cache CacheNew $ Generic.txwRewardAccount txWdrl
  void . lift . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = Generic.coinToDbLovelace $ Generic.txwAmount txWdrl
      , DB.withdrawalRedeemerId = mlookup (Generic.txwRedeemerIndex txWdrl) redeemers
      }

-- | Insert a stake address if it is not already in the `stake_address` table. Regardless of
-- whether it is newly inserted or it is already there, we retrun the `StakeAddressId`.
insertStakeAddressRefIfMissing ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Ledger.Addr StandardCrypto ->
  ReaderT SqlBackend m (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing _trce cache addr =
  case addr of
    Ledger.AddrBootstrap {} -> pure Nothing
    Ledger.Addr nw _pcred sref ->
      case sref of
        Ledger.StakeRefBase cred -> do
          Just <$> queryOrInsertStakeAddress cache DontCacheNew nw cred
        Ledger.StakeRefPtr ptr -> do
          queryStakeRefPtr ptr
        Ledger.StakeRefNull -> pure Nothing

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
  ReaderT SqlBackend m DB.ScriptId
insertScript tracer txId script = do
  mScriptId <- DB.queryScript $ Generic.txScriptHash script
  case mScriptId of
    Just scriptId -> pure scriptId
    Nothing -> do
      json <- scriptConvert script
      DB.insertScript $
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
  void
    . lift
    . DB.insertExtraKeyWitness
    $ DB.ExtraKeyWitness
      { DB.extraKeyWitnessHash = keyHash
      , DB.extraKeyWitnessTxId = txId
      }

--------------------------------------------------------------------------------------------
-- Insert Helpers
--------------------------------------------------------------------------------------------
toDouble :: Ledger.UnitInterval -> Double
toDouble = Generic.unitIntervalToDouble
