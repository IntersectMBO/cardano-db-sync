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
import Cardano.DbSync.Api.Types (SyncEnv (..), UnicodeNullSource (..))
import Cardano.DbSync.Cache (insertDatumAndCache, queryDatum, queryMAWithCache, queryOrInsertRewardAccount, queryOrInsertStakeAddress)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Util (safeDecodeToJson)
import Cardano.DbSync.Error (SyncNodeError)
import Cardano.DbSync.Types (BlockEra (..))
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
  SyncEnv ->
  Bool ->
  [ExtendedTxOut] ->
  DB.TxId ->
  (Word64, Generic.TxRedeemer) ->
  ExceptT SyncNodeError DB.DbM (Word64, DB.RedeemerId)
insertRedeemer syncEnv disInOut groupedOutputs txId (rix, redeemer) = do
  tdId <- insertRedeemerData syncEnv txId $ Generic.txRedeemerData redeemer
  scriptHash <- findScriptHash
  rid <-
    lift $
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
    findScriptHash ::
      ExceptT SyncNodeError DB.DbM (Maybe ByteString)
    findScriptHash =
      case (disInOut, Generic.txRedeemerScriptHash redeemer) of
        (True, _) -> pure Nothing
        (_, Nothing) -> pure Nothing
        (_, Just (Right bs)) -> pure $ Just bs
        (_, Just (Left txIn)) -> resolveScriptHash syncEnv groupedOutputs txIn

insertRedeemerData ::
  SyncEnv ->
  DB.TxId ->
  Generic.PlutusData ->
  ExceptT SyncNodeError DB.DbM DB.RedeemerDataId
insertRedeemerData syncEnv txId txd = do
  mRedeemerDataId <- lift $ DB.queryRedeemerData $ Generic.dataHashToBytes $ Generic.txDataHash txd
  case mRedeemerDataId of
    Just redeemerDataId -> pure redeemerDataId
    Nothing -> do
      value <- safeDecodeToJson syncEnv InsertDatum txId (Generic.txDataValue txd)
      lift $
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
  SyncEnv ->
  DB.TxId ->
  Generic.PlutusData ->
  ExceptT SyncNodeError DB.DbM DB.DatumId
insertDatum syncEnv txId txd = do
  mDatumId <- queryDatum syncEnv $ Generic.txDataHash txd
  case mDatumId of
    Just datumId -> pure datumId
    Nothing -> do
      value <- safeDecodeToJson syncEnv InsertRedeemerData txId (Generic.txDataValue txd)
      insertDatumAndCache (envCache syncEnv) (Generic.txDataHash txd) $
        DB.Datum
          { DB.datumHash = Generic.dataHashToBytes $ Generic.txDataHash txd
          , DB.datumTxId = txId
          , DB.datumValue = value
          , DB.datumBytes = Generic.txDataBytes txd
          }

insertWithdrawals ::
  SyncEnv ->
  DB.TxId ->
  Map Word64 DB.RedeemerId ->
  Generic.TxWithdrawal ->
  ExceptT SyncNodeError DB.DbM ()
insertWithdrawals syncEnv txId redeemers txWdrl = do
  addrId <-
    queryOrInsertRewardAccount syncEnv UpdateCache $ Generic.txwRewardAccount txWdrl
  void . lift $
    DB.insertWithdrawal $
      DB.Withdrawal
        { DB.withdrawalAddrId = addrId
        , DB.withdrawalTxId = txId
        , DB.withdrawalAmount = Generic.coinToDbLovelace $ Generic.txwAmount txWdrl
        , DB.withdrawalRedeemerId = mlookup (Generic.txwRedeemerIndex txWdrl) redeemers
        }

-- | Insert a stake address if it is not already in the `stake_address` table. Regardless of
-- whether it is newly inserted or it is already there, we retrun the `StakeAddressId`.
insertStakeAddressRefIfMissing ::
  SyncEnv ->
  Ledger.Addr ->
  BlockEra ->
  ExceptT SyncNodeError DB.DbM (Maybe DB.StakeAddressId)
insertStakeAddressRefIfMissing syncEnv addr era =
  case addr of
    Ledger.AddrBootstrap {} -> pure Nothing
    Ledger.Addr nw _pcred sref ->
      case sref of
        Ledger.StakeRefBase cred -> do
          Just <$> queryOrInsertStakeAddress syncEnv UpdateCache nw cred
        Ledger.StakeRefPtr ptr ->
          -- In Conway era onwards, Pointer addresses are treated as Enterprise addresses
          case era of
            Conway -> pure Nothing
            Dijkstra -> pure Nothing
            _ -> lift $ DB.queryStakeRefPtr ptr
        Ledger.StakeRefNull -> pure Nothing

insertMultiAsset ::
  SyncEnv ->
  PolicyID ->
  AssetName ->
  ExceptT SyncNodeError DB.DbM DB.MultiAssetId
insertMultiAsset syncEnv policy aName = do
  mId <- queryMAWithCache syncEnv policy aName
  case mId of
    Right maId -> pure maId
    Left (policyBs, assetNameBs) ->
      lift $
        DB.insertMultiAsset $
          DB.MultiAsset
            { DB.multiAssetPolicy = policyBs
            , DB.multiAssetName = assetNameBs
            , DB.multiAssetFingerprint = DB.unAssetFingerprint (DB.mkAssetFingerprint policyBs assetNameBs)
            }

insertScript ::
  SyncEnv ->
  DB.TxId ->
  Generic.TxScript ->
  ExceptT SyncNodeError DB.DbM DB.ScriptId
insertScript syncEnv txId script = do
  mScriptId <- lift $ DB.queryScriptWithId $ Generic.txScriptHash script
  case mScriptId of
    Just scriptId -> pure scriptId
    Nothing -> do
      json <- scriptConvert script
      lift $
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
      maybe (pure Nothing) (safeDecodeToJson syncEnv InsertScript txId) (Generic.txScriptJson s)

insertExtraKeyWitness ::
  Trace IO Text ->
  DB.TxId ->
  ByteString ->
  ExceptT SyncNodeError DB.DbM ()
insertExtraKeyWitness _tracer txId keyHash = do
  void
    . lift
    $ DB.insertExtraKeyWitness
    $ DB.ExtraKeyWitness
      { DB.extraKeyWitnessHash = keyHash
      , DB.extraKeyWitnessTxId = txId
      }

--------------------------------------------------------------------------------------------
-- Insert Helpers
--------------------------------------------------------------------------------------------
toDouble :: Ledger.UnitInterval -> Double
toDouble = Generic.unitIntervalToDouble
