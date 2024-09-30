{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.DbSync.Api.Ledger where

import Cardano.BM.Trace (logError, logInfo, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache (queryTxIdWithCache)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage (fromTxOut)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types (DBPlutusScript)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Universal.Insert.Tx (insertTxOut)
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Types
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.TxIn
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Prelude (lift, textShow)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTVarIO, writeTVar)
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Database.Persist.Sql (SqlBackend)
import Lens.Micro
import Numeric
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

bootStrapMaybe ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
bootStrapMaybe syncEnv = do
  bts <- liftIO $ readTVarIO (envBootstrap syncEnv)
  when bts $ migrateBootstrapUTxO syncEnv

migrateBootstrapUTxO ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
migrateBootstrapUTxO syncEnv = do
  case envLedgerEnv syncEnv of
    HasLedger lenv -> do
      liftIO $ logInfo trce "Starting UTxO bootstrap migration"
      cls <- liftIO $ readCurrentStateUnsafe lenv
      count <- lift $ DB.deleteTxOut (getTxOutTableType syncEnv)
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

storeUTxOFromLedger :: (MonadBaseControl IO m, MonadIO m) => SyncEnv -> ExtLedgerState CardanoBlock -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
storeUTxOFromLedger env st = case ledgerState st of
  LedgerStateBabbage bts -> storeUTxO env (getUTxO bts)
  LedgerStateConway stc -> storeUTxO env (getUTxO stc)
  _otherwise -> liftIO $ logError trce "storeUTxOFromLedger is only supported after Babbage"
  where
    trce = getTrace env
    getUTxO st' =
      unUTxO $ Consensus.shelleyLedgerState st' ^. (nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL)

pageSize :: Int
pageSize = 100000

storeUTxO ::
  ( EraCrypto era ~ StandardCrypto
  , Cardano.Ledger.Core.Value era ~ MaryValue StandardCrypto
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , BabbageEraTxOut era
  , MonadIO m
  , MonadBaseControl IO m
  , DBPlutusScript era
  , NativeScript era ~ Timelock era
  ) =>
  SyncEnv ->
  Map (TxIn StandardCrypto) (BabbageTxOut era) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
storeUTxO env mp = do
  liftIO $
    logInfo trce $
      mconcat
        [ "Inserting "
        , textShow size
        , " tx_out as pages of "
        , textShow pageSize
        ]
  mapM_ (storePage env pagePerc) . zip [0 ..] . chunksOf pageSize . Map.toList $ mp
  where
    trce = getTrace env
    npages = size `div` pageSize
    pagePerc :: Float = if npages == 0 then 100.0 else 100.0 / fromIntegral npages
    size = Map.size mp

storePage ::
  ( EraCrypto era ~ StandardCrypto
  , Cardano.Ledger.Core.Value era ~ MaryValue StandardCrypto
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , DBPlutusScript era
  , BabbageEraTxOut era
  , NativeScript era ~ Timelock era
  , MonadIO m
  , MonadBaseControl IO m
  ) =>
  SyncEnv ->
  Float ->
  (Int, [(TxIn StandardCrypto, BabbageTxOut era)]) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
storePage syncEnv percQuantum (n, ls) = do
  when (n `mod` 10 == 0) $ liftIO $ logInfo trce $ "Bootstrap in progress " <> prc <> "%"
  txOuts <- mapM (prepareTxOut syncEnv) ls
  txOutIds <-
    lift . DB.insertManyTxOut False $ etoTxOut . fst <$> txOuts
  let maTxOuts = concatMap (mkmaTxOuts txOutTableType) $ zip txOutIds (snd <$> txOuts)
  void . lift $ DB.insertManyMaTxOut maTxOuts
  where
    txOutTableType = getTxOutTableType syncEnv
    trce = getTrace syncEnv
    prc = Text.pack $ showGFloat (Just 1) (max 0 $ min 100.0 (fromIntegral n * percQuantum)) ""

prepareTxOut ::
  ( EraCrypto era ~ StandardCrypto
  , Cardano.Ledger.Core.Value era ~ MaryValue StandardCrypto
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , BabbageEraTxOut era
  , MonadIO m
  , MonadBaseControl IO m
  , DBPlutusScript era
  , NativeScript era ~ Timelock era
  ) =>
  SyncEnv ->
  (TxIn StandardCrypto, BabbageTxOut era) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (ExtendedTxOut, [MissingMaTxOut])
prepareTxOut syncEnv (TxIn txIntxId (TxIx index), txOut) = do
  let txHashByteString = Generic.safeHashToByteString $ unTxId txIntxId
  let genTxOut = fromTxOut index txOut
  txId <- liftLookupFail "prepareTxOut" $ queryTxIdWithCache cache txIntxId
  insertTxOut trce cache iopts (txId, txHashByteString) genTxOut
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv
    iopts = soptInsertOptions $ envOptions syncEnv
