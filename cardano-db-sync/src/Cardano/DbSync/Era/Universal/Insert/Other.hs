{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.Other (
  toDouble,
  insertRedeemer,
  insertDatum,
  insertWithdrawals,
  insertDirectDeposits,
  insertTxGuard,
  insertTxRequiredGuard,
  insertTxAccountBalanceInterval,
  insertRedeemerData,
  insertStakeAddressRefIfMissing,
  insertMultiAsset,
  insertScript,
  insertExtraKeyWitness,
) where

import qualified Cardano.Db as DB
import Cardano.Db.Log (LogMessage)
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
import Cardano.Logging (Trace)
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
      value <- safeDecodeToJson syncEnv InsertRedeemerData txId (Generic.txDataValue txd)
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
      value <- safeDecodeToJson syncEnv InsertDatum txId (Generic.txDataValue txd)
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

-- | Insert a Dijkstra direct deposit: lovelace paid straight into an account, with no
-- output of its own. The mirror of 'insertWithdrawals', and with no redeemer.
insertDirectDeposits ::
  SyncEnv ->
  DB.TxId ->
  Generic.TxDirectDeposit ->
  ExceptT SyncNodeError DB.DbM ()
insertDirectDeposits syncEnv txId txDeposit = do
  addrId <-
    queryOrInsertRewardAccount syncEnv UpdateCache $ Generic.txddAccount txDeposit
  void . lift $
    DB.insertDirectDeposit $
      DB.DirectDeposit
        { DB.directDepositAddrId = addrId
        , DB.directDepositAmount = Generic.coinToDbLovelace $ Generic.txddAmount txDeposit
        , DB.directDepositTxId = txId
        }

-- | Insert a guard credential a Dijkstra transaction body supplies. The index is the
-- credential's position in the body's ordered guard set, which is also the index of the
-- matching redeemer with purpose 'guard'.
insertTxGuard ::
  DB.TxId ->
  Generic.TxGuard ->
  ExceptT SyncNodeError DB.DbM ()
insertTxGuard txId txGuard =
  void . lift $
    DB.insertTxGuard $
      DB.TxGuard
        { DB.txGuardTxId = txId
        , DB.txGuardIndex = Generic.txgIndex txGuard
        , DB.txGuardCredential = Generic.txgCredential txGuard
        , DB.txGuardHasScript = Generic.txgHasScript txGuard
        }

-- | Insert a guard credential a Dijkstra transaction body requires of its enclosing
-- top-level transaction. The datum is only recorded when plutus extra is enabled, since
-- that is what populates the `datum` table it points at.
insertTxRequiredGuard ::
  SyncEnv ->
  Bool ->
  DB.TxId ->
  Generic.TxRequiredGuard ->
  ExceptT SyncNodeError DB.DbM ()
insertTxRequiredGuard syncEnv insertDatums txId reqGuard = do
  mDatumId <-
    if insertDatums
      then mapM (insertDatum syncEnv txId) (Generic.txrgDatum reqGuard)
      else pure Nothing
  void . lift $
    DB.insertTxRequiredGuard $
      DB.TxRequiredGuard
        { DB.txRequiredGuardTxId = txId
        , DB.txRequiredGuardCredential = Generic.txrgCredential reqGuard
        , DB.txRequiredGuardHasScript = Generic.txrgHasScript reqGuard
        , DB.txRequiredGuardDataId = mDatumId
        }

-- | Insert a balance assertion a Dijkstra transaction body makes about an account.
insertTxAccountBalanceInterval ::
  SyncEnv ->
  DB.TxId ->
  Generic.TxAccountBalanceInterval ->
  ExceptT SyncNodeError DB.DbM ()
insertTxAccountBalanceInterval syncEnv txId interval = do
  addrId <-
    queryOrInsertRewardAccount syncEnv UpdateCache $ Generic.txabiAccount interval
  void . lift $
    DB.insertTxAccountBalanceInterval $
      DB.TxAccountBalanceInterval
        { DB.txAccountBalanceIntervalTxId = txId
        , DB.txAccountBalanceIntervalAddrId = addrId
        , DB.txAccountBalanceIntervalStarting = Generic.txabiStarting interval
        , DB.txAccountBalanceIntervalLowerBound =
            Generic.coinToDbLovelace <$> Generic.txabiLowerBound interval
        , DB.txAccountBalanceIntervalUpperBound =
            Generic.coinToDbLovelace <$> Generic.txabiUpperBound interval
        , DB.txAccountBalanceIntervalExactValue =
            Generic.coinToDbLovelace <$> Generic.txabiExactValue interval
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
  Trace IO LogMessage ->
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
