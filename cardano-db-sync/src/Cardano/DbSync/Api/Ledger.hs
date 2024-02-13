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
import Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage (fromTxOut)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types (DBPlutusScript)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import qualified Cardano.DbSync.Era.Shelley.Insert as Insert
import Cardano.DbSync.Era.Shelley.Insert.Grouped
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Types
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.TxIn
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Prelude (lift)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTVarIO, writeTVar)
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
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
  when bts $ do
    migrateBootstrapUTxO syncEnv emptyTxCache -- TODO: hardcoded to empty

newtype TxCache = TxCache {txIdCache :: Map ByteString DB.TxId}

emptyTxCache :: TxCache
emptyTxCache = TxCache mempty

migrateBootstrapUTxO ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  TxCache ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
migrateBootstrapUTxO syncEnv txCache = do
  case envLedgerEnv syncEnv of
    HasLedger lenv -> do
      liftIO $ logInfo trce "Starting UTxO bootstrap migration"
      cls <- liftIO $ readCurrentStateUnsafe lenv
      count <- lift DB.deleteTxOut
      when (count > 0) $
        liftIO $
          logWarning trce $
            "Found and deleted " <> DB.textShow count <> " tx_out."
      storeUTxOFromLedger syncEnv txCache cls
      lift $ DB.insertExtraMigration DB.BootstrapFinished
      liftIO $ logInfo trce "UTxO bootstrap migration done"
      liftIO $ atomically $ writeTVar (envBootstrap syncEnv) False
    NoLedger _ ->
      liftIO $ logWarning trce "Tried to bootstrap, but ledger state is not enabled. Please stop db-sync and restart without --disable-ledger-state"
  where
    trce = getTrace syncEnv

storeUTxOFromLedger :: (MonadBaseControl IO m, MonadIO m) => SyncEnv -> TxCache -> ExtLedgerState CardanoBlock -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
storeUTxOFromLedger env txCache st = case ledgerState st of
  LedgerStateBabbage bts -> storeUTxO env txCache (getUTxO bts)
  LedgerStateConway stc -> storeUTxO env txCache (getUTxO stc)
  _ -> liftIO $ logError trce "storeUTxOFromLedger is only supported after Babbage"
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
  ) =>
  SyncEnv ->
  TxCache ->
  Map (TxIn StandardCrypto) (BabbageTxOut era) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
storeUTxO env txCache mp = do
  liftIO $
    logInfo trce $
      mconcat
        [ "Inserting "
        , DB.textShow size
        , " tx_out as pages of "
        , DB.textShow pageSize
        ]
  mapM_ (storePage env txCache pagePerc) . zip [0 ..] . chunksOf pageSize . Map.toList $ mp
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
  , MonadIO m
  , MonadBaseControl IO m
  ) =>
  SyncEnv ->
  TxCache ->
  Float ->
  (Int, [(TxIn StandardCrypto, BabbageTxOut era)]) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
storePage syncEnv cache percQuantum (n, ls) = do
  when (n `mod` 10 == 0) $ liftIO $ logInfo trce $ "Bootstrap in progress " <> prc <> "%"
  txOuts <- mapM (prepareTxOut syncEnv cache) ls
  txOutIds <- lift . DB.insertManyTxOutPlex True False $ etoTxOut . fst <$> txOuts
  let maTxOuts = concatMap mkmaTxOuts $ zip txOutIds (snd <$> txOuts)
  void . lift $ DB.insertManyMaTxOut maTxOuts
  where
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
  ) =>
  SyncEnv ->
  TxCache ->
  (TxIn StandardCrypto, BabbageTxOut era) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (ExtendedTxOut, [MissingMaTxOut])
prepareTxOut syncEnv txCache (TxIn txHash (TxIx index), txOut) = do
  let txHashByteString = Generic.safeHashToByteString $ unTxId txHash
  let genTxOut = fromTxOut index txOut
  txId <- queryTxIdWithCache txCache txHashByteString
  Insert.prepareTxOut trce cache iopts (txId, txHashByteString) genTxOut
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv
    iopts = soptInsertOptions $ envOptions syncEnv

queryTxIdWithCache :: MonadIO m => TxCache -> ByteString -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.TxId
queryTxIdWithCache (TxCache mp) hsh = do
  case Map.lookup hsh mp of
    Just txId -> pure txId
    Nothing -> liftLookupFail "queryTxIdWithCache" $ DB.queryTxId hsh
