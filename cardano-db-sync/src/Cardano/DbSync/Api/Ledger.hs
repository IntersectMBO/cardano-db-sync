{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.DbSync.Api.Ledger where

import Cardano.BM.Trace (logError, logInfo, logWarning)
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.TxIn
import Cardano.Prelude (ExceptT, lift, textShow)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTVarIO, writeTVar)
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lens.Micro
import Numeric
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache (queryTxIdWithCache)
import Cardano.DbSync.DbEvent (liftDbLookupEither)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage (fromTxOut)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types (DBPlutusScript)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Universal.Insert.Tx (insertTxOut)
import Cardano.DbSync.Error (SyncNodeError, mkSyncNodeCallStack)
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Types

bootStrapMaybe ::
  SyncEnv ->
  ExceptT SyncNodeError DB.DbM ()
bootStrapMaybe syncEnv = do
  bts <- liftIO $ readTVarIO (envBootstrap syncEnv)
  when bts $ migrateBootstrapUTxO syncEnv

migrateBootstrapUTxO ::
  SyncEnv ->
  ExceptT SyncNodeError DB.DbM ()
migrateBootstrapUTxO syncEnv = do
  case envLedgerEnv syncEnv of
    HasLedger lenv -> do
      liftIO $ logInfo trce "Starting UTxO bootstrap migration"
      cls <- liftIO $ readCurrentStateUnsafe lenv
      count <- lift $ DB.deleteTxOut (getTxOutVariantType syncEnv)
      when (count > 0) $
        liftIO $
          logWarning trce $
            "Found and deleted " <> textShow count <> " tx_out."
      storeUTxOFromLedger syncEnv cls
      lift $ DB.insertExtraMigration DB.BootstrapFinished
      liftIO $ logInfo trce "UTxO bootstrap migration done"
      liftIO $ atomically $ writeTVar (envBootstrap syncEnv) False
    NoLedger _ ->
      liftIO $ logWarning trce "Tried to bootstrap, but ledger state is not enabled. Please stop db-sync and restart without --disable-ledger-state"
  where
    trce = getTrace syncEnv

storeUTxOFromLedger ::
  SyncEnv ->
  ExtLedgerState CardanoBlock mk ->
  ExceptT SyncNodeError DB.DbM ()
storeUTxOFromLedger env st = case ledgerState st of
  LedgerStateBabbage bts -> storeUTxO env (getUTxO bts)
  LedgerStateConway stc -> storeUTxO env (getUTxO stc)
  LedgerStateDijkstra stc -> storeUTxO env (getUTxO stc)
  _otherwise -> liftIO $ logError trce "storeUTxOFromLedger is only supported after Babbage"
  where
    trce = getTrace env
    getUTxO st' =
      unUTxO $ Consensus.shelleyLedgerState st' ^. (nesEsL . esLStateL . lsUTxOStateL . utxoL)

storeUTxO ::
  ( Cardano.Ledger.Core.Value era ~ MaryValue
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , BabbageEraTxOut era
  , DBPlutusScript era
  , NativeScript era ~ Timelock era
  ) =>
  SyncEnv ->
  Map TxIn (BabbageTxOut era) ->
  ExceptT SyncNodeError DB.DbM ()
storeUTxO env mp = do
  liftIO $
    logInfo trce $
      mconcat
        [ "Inserting "
        , textShow size
        , " tx_out as pages of "
        , textShow bulkSize
        ]
  mapM_ (storePage env pagePerc) . zip [0 ..] . chunksOf bulkSize . Map.toList $ mp
  where
    trce = getTrace env
    bulkSize = DB.getTxOutBulkSize (getTxOutVariantType env)
    npages = size `div` bulkSize
    pagePerc :: Float = if npages == 0 then 100.0 else 100.0 / fromIntegral npages
    size = Map.size mp

storePage ::
  ( Cardano.Ledger.Core.Value era ~ MaryValue
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , DBPlutusScript era
  , BabbageEraTxOut era
  , NativeScript era ~ Timelock era
  ) =>
  SyncEnv ->
  Float ->
  (Int, [(TxIn, BabbageTxOut era)]) ->
  ExceptT SyncNodeError DB.DbM ()
storePage syncEnv percQuantum (n, ls) = do
  when (n `mod` 10 == 0) $ liftIO $ logInfo trce $ "Bootstrap in progress " <> prc <> "%"
  txOuts <- mapM (prepareTxOut syncEnv) ls
  txOutIds <- lift $ DB.insertBulkTxOut False $ etoTxOut . fst <$> txOuts
  let maTxOuts = concatMap (mkmaTxOuts txOutVariantType) $ zip txOutIds (snd <$> txOuts)
  void . lift $ DB.insertBulkMaTxOutPiped [maTxOuts]
  where
    txOutVariantType = getTxOutVariantType syncEnv
    trce = getTrace syncEnv
    prc = Text.pack $ showGFloat (Just 1) (max 0 $ min 100.0 (fromIntegral n * percQuantum)) ""

prepareTxOut ::
  ( Cardano.Ledger.Core.Value era ~ MaryValue
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , BabbageEraTxOut era
  , DBPlutusScript era
  , NativeScript era ~ Timelock era
  ) =>
  SyncEnv ->
  (TxIn, BabbageTxOut era) ->
  ExceptT SyncNodeError DB.DbM (ExtendedTxOut, [MissingMaTxOut])
prepareTxOut syncEnv (TxIn txIntxId (TxIx index), txOut) = do
  let txHashByteString = Generic.safeHashToByteString $ unTxId txIntxId
  let genTxOut = fromTxOut (fromIntegral index) txOut
  txId <- liftDbLookupEither mkSyncNodeCallStack $ queryTxIdWithCache syncEnv txIntxId
  insertTxOut syncEnv iopts (txId, txHashByteString) genTxOut
  where
    iopts = soptInsertOptions $ envOptions syncEnv
