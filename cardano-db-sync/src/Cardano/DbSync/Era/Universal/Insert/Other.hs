{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

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

import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, MonadAppDB (..), SyncEnv (..))
import Cardano.DbSync.Cache (insertDatumAndCache, queryDatum, queryMAWithCache, queryOrInsertRewardAccount, queryOrInsertStakeAddress)
import Cardano.DbSync.Cache.Types (UpdateCache (..), CacheStatus)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Query (queryStakeRefPtr)
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Util (safeDecodeToJson)
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..))
import Cardano.Prelude
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

--------------------------------------------------------------------------------------------
-- Insert Redeemer
--------------------------------------------------------------------------------------------
insertRedeemer ::
  Bool ->
  [ExtendedTxOut] ->
  DB.TxId ->
  (Word64, Generic.TxRedeemer) ->
  App (Word64, DB.RedeemerId)
insertRedeemer disInOut groupedOutputs txId (rix, redeemer) = do
  tdId <- insertRedeemerData txId $ Generic.txRedeemerData redeemer
  scriptHash <- findScriptHash
  rid <-
    dbQueryToApp
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
    findScriptHash :: App (Maybe ByteString)
    findScriptHash =
      case (disInOut, Generic.txRedeemerScriptHash redeemer) of
        (True, _) -> pure Nothing
        (_, Nothing) -> pure Nothing
        (_, Just (Right bs)) -> pure $ Just bs
        (_, Just (Left txIn)) -> resolveScriptHash groupedOutputs txIn

insertRedeemerData ::
  DB.TxId ->
  Generic.PlutusData ->
  App DB.RedeemerDataId
insertRedeemerData txId txd = do
  mRedeemerDataId <- dbQueryToApp $ DB.queryRedeemerData $ Generic.dataHashToBytes $ Generic.txDataHash txd
  case mRedeemerDataId of
    Just redeemerDataId -> pure redeemerDataId
    Nothing -> do
      value <- safeDecodeToJson "insertDatum: Column 'value' in table 'datum' " $ Generic.txDataValue txd
      dbQueryToApp
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
  DB.TxId ->
  Generic.PlutusData ->
  App DB.DatumId
insertDatum txId txd = do
  mDatumId <- queryDatum $ Generic.txDataHash txd
  case mDatumId of
    Just datumId -> pure datumId
    Nothing -> do
      value <- safeDecodeToJson "insertRedeemerData: Column 'value' in table 'redeemer' " $ Generic.txDataValue txd
      insertDatumAndCache (Generic.txDataHash txd) $
        DB.Datum
          { DB.datumHash = Generic.dataHashToBytes $ Generic.txDataHash txd
          , DB.datumTxId = txId
          , DB.datumValue = value
          , DB.datumBytes = Generic.txDataBytes txd
          }

insertWithdrawals ::
  DB.TxId ->
  Map Word64 DB.RedeemerId ->
  Generic.TxWithdrawal ->
  App ()
insertWithdrawals txId redeemers txWdrl = do
  cache <- asks envCache
  addrId <- queryOrInsertRewardAccount cache UpdateCache $ Generic.txwRewardAccount txWdrl
  void . dbQueryToApp . DB.insertWithdrawal $
    DB.Withdrawal
      { DB.withdrawalAddrId = addrId
      , DB.withdrawalTxId = txId
      , DB.withdrawalAmount = Generic.coinToDbLovelace $ Generic.txwAmount txWdrl
      , DB.withdrawalRedeemerId = mlookup (Generic.txwRedeemerIndex txWdrl) redeemers
      }

-- | Insert a stake address if it is not already in the `stake_address` table. Regardless of
-- whether it is newly inserted or it is already there, we retrun the `StakeAddressId`.
insertStakeAddressRefIfMissing ::
  CacheStatus ->
  Ledger.Addr StandardCrypto ->
  App (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing cache addr =
  case addr of
    Ledger.AddrBootstrap {} -> pure Nothing
    Ledger.Addr nw _pcred sref ->
      case sref of
        Ledger.StakeRefBase cred -> do
          Just <$> queryOrInsertStakeAddress cacheStatus DoNotUpdateCache nw cred
        Ledger.StakeRefPtr ptr -> do
          queryStakeRefPtr ptr
        Ledger.StakeRefNull -> pure Nothing

insertMultiAsset ::
  PolicyID StandardCrypto ->
  AssetName ->
  App DB.MultiAssetId
insertMultiAsset policy aName = do
  mId <- queryMAWithCache policy aName
  case mId of
    Right maId -> pure maId
    Left (policyBs, assetNameBs) ->
      dbQueryToApp $
        DB.insertMultiAssetUnchecked $
          DB.MultiAsset
            { DB.multiAssetPolicy = policyBs
            , DB.multiAssetName = assetNameBs
            , DB.multiAssetFingerprint = DB.unAssetFingerprint (DB.mkAssetFingerprint policyBs assetNameBs)
            }

insertScript ::
  DB.TxId ->
  Generic.TxScript ->
  App DB.ScriptId
insertScript txId script = do
  mScriptId <- dbQueryToApp $ DB.queryScript $ Generic.txScriptHash script
  case mScriptId of
    Just scriptId -> pure scriptId
    Nothing -> do
      json <- scriptConvert script
      dbQueryToApp $
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
    scriptConvert :: Generic.TxScript -> App (Maybe Text)
    scriptConvert s =
      maybe (pure Nothing) (safeDecodeToJson "insertScript: Column 'json' in table 'script' ") (Generic.txScriptJson s)

insertExtraKeyWitness ::
  DB.TxId ->
  ByteString ->
  App ()
insertExtraKeyWitness txId keyHash = do
  void
    . dbQueryToApp
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
