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
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Cache (insertDatumAndCache, queryDatum, queryMAWithCache, queryOrInsertRewardAccount, queryOrInsertStakeAddress)
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheStatus (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Util (safeDecodeToJson)
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..))
import Cardano.Prelude

--------------------------------------------------------------------------------------------
-- Insert Redeemer
--------------------------------------------------------------------------------------------
insertRedeemer ::
  MonadIO m =>
  SyncEnv ->
  Bool ->
  [ExtendedTxOut] ->
  DB.TxId ->
  (Word64, Generic.TxRedeemer) ->
  DB.DbAction m (Word64, DB.RedeemerId)
insertRedeemer syncEnv disInOut groupedOutputs txId (rix, redeemer) = do
  tdId <- insertRedeemerData tracer txId $ Generic.txRedeemerData redeemer
  scriptHash <- findScriptHash
  rid <-
    DB.insertRedeemer $
      DB.Redeemer
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
    tracer = getTrace syncEnv
    findScriptHash ::
      MonadIO m =>
      DB.DbAction m (Maybe ByteString)
    findScriptHash =
      case (disInOut, Generic.txRedeemerScriptHash redeemer) of
        (True, _) -> pure Nothing
        (_, Nothing) -> pure Nothing
        (_, Just (Right bs)) -> pure $ Just bs
        (_, Just (Left txIn)) -> resolveScriptHash syncEnv groupedOutputs txIn

insertRedeemerData ::
  MonadIO m =>
  Trace IO Text ->
  DB.TxId ->
  Generic.PlutusData ->
  DB.DbAction m DB.RedeemerDataId
insertRedeemerData tracer txId txd = do
  mRedeemerDataId <- DB.queryRedeemerData $ Generic.dataHashToBytes $ Generic.txDataHash txd
  case mRedeemerDataId of
    Just redeemerDataId -> pure redeemerDataId
    Nothing -> do
      value <- safeDecodeToJson tracer "insertDatum: Column 'value' in table 'datum' " $ Generic.txDataValue txd
      DB.insertRedeemerData $
        DB.RedeemerData
          { DB.redeemerDataHash = Generic.dataHashToBytes $ Generic.txDataHash txd
          , DB.redeemerDataTxId = txId
          , DB.redeemerDataValue = value
          , DB.redeemerDataBytes = Generic.txDataBytes txd
          }

--------------------------------------------------------------------------------------------
-- Insert Others
--------------------------------------------------------------------------------------------
insertDatum ::
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  DB.TxId ->
  Generic.PlutusData ->
  DB.DbAction m DB.DatumId
insertDatum tracer cache txId txd = do
  mDatumId <- queryDatum cache $ Generic.txDataHash txd
  case mDatumId of
    Just datumId -> pure datumId
    Nothing -> do
      value <- safeDecodeToJson tracer "insertRedeemerData: Column 'value' in table 'redeemer' " $ Generic.txDataValue txd
      insertDatumAndCache cache (Generic.txDataHash txd) $
        DB.Datum
          { DB.datumHash = Generic.dataHashToBytes $ Generic.txDataHash txd
          , DB.datumTxId = txId
          , DB.datumValue = value
          , DB.datumBytes = Generic.txDataBytes txd
          }

insertWithdrawals ::
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  DB.TxId ->
  Map Word64 DB.RedeemerId ->
  Generic.TxWithdrawal ->
  DB.DbAction m ()
insertWithdrawals tracer cache txId redeemers txWdrl = do
  addrId <-
    queryOrInsertRewardAccount tracer cache UpdateCache $ Generic.txwRewardAccount txWdrl
  void . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = Generic.coinToDbLovelace $ Generic.txwAmount txWdrl
      , DB.withdrawalRedeemerId = mlookup (Generic.txwRedeemerIndex txWdrl) redeemers
      }

-- | Insert a stake address if it is not already in the `stake_address` table. Regardless of
-- whether it is newly inserted or it is already there, we retrun the `StakeAddressId`.
insertStakeAddressRefIfMissing ::
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  Ledger.Addr ->
  DB.DbAction m (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing trce cache addr =
  case addr of
    Ledger.AddrBootstrap {} -> pure Nothing
    Ledger.Addr nw _pcred sref ->
      case sref of
        Ledger.StakeRefBase cred -> do
          Just <$> queryOrInsertStakeAddress trce cache UpdateCache nw cred
        Ledger.StakeRefPtr ptr -> do
          DB.queryStakeRefPtr ptr
        Ledger.StakeRefNull -> pure Nothing

insertMultiAsset ::
  MonadIO m =>
  CacheStatus ->
  PolicyID ->
  AssetName ->
  DB.DbAction m DB.MultiAssetId
insertMultiAsset cache policy aName = do
  mId <- queryMAWithCache cache policy aName
  case mId of
    Right maId -> pure maId
    Left (policyBs, assetNameBs) ->
      DB.insertMultiAsset $
        DB.MultiAsset
          { DB.multiAssetPolicy = policyBs
          , DB.multiAssetName = assetNameBs
          , DB.multiAssetFingerprint = DB.unAssetFingerprint (DB.mkAssetFingerprint policyBs assetNameBs)
          }

insertScript ::
  MonadIO m =>
  Trace IO Text ->
  DB.TxId ->
  Generic.TxScript ->
  DB.DbAction m DB.ScriptId
insertScript tracer txId script = do
  mScriptId <- DB.queryScriptWithId $ Generic.txScriptHash script
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
      maybe (pure Nothing) (safeDecodeToJson tracer "insertScript: Column 'json' in table 'script' ") (Generic.txScriptJson s)

insertExtraKeyWitness ::
  MonadIO m =>
  Trace IO Text ->
  DB.TxId ->
  ByteString ->
  DB.DbAction m ()
insertExtraKeyWitness _tracer txId keyHash = do
  void
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
