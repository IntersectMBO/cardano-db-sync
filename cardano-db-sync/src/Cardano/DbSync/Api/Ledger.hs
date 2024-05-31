{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.DbSync.Api.Ledger where

import Cardano.BM.Trace (logError, logInfo, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, LedgerEnv (..), MonadAppDB (..), SyncEnv (..), askTrace)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage (fromTxOut)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types (DBPlutusScript)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Universal.Insert.Tx (insertTxOut)
import Cardano.DbSync.Era.Util
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
import Cardano.Prelude (MonadReader (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTVarIO, writeTVar)
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Lens.Micro
import Numeric
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

bootStrapMaybe :: App ()
bootStrapMaybe = do
  syncEnv <- ask
  bts <- liftIO $ readTVarIO (envBootstrap syncEnv)
  when bts $ do
    migrateBootstrapUTxO emptyTxCache -- TODO: hardcoded to empty

newtype TxCache = TxCache {txIdCache :: Map ByteString DB.TxId}

emptyTxCache :: TxCache
emptyTxCache = TxCache mempty

migrateBootstrapUTxO ::
  TxCache ->
  App ()
migrateBootstrapUTxO txCache = do
  syncEnv <- ask
  trce <- askTrace
  case envLedgerEnv syncEnv of
    HasLedger lenv -> do
      liftIO $ logInfo trce "Starting UTxO bootstrap migration"
      cls <- readCurrentStateUnsafe lenv
      count <- dbQueryToApp DB.deleteTxOut
      when (count > 0) $
        liftIO $
          logWarning trce $
            "Found and deleted " <> DB.textShow count <> " tx_out."
      storeUTxOFromLedger txCache cls
      dbQueryToApp $ DB.insertExtraMigration DB.BootstrapFinished
      liftIO $ logInfo trce "UTxO bootstrap migration done"
      liftIO $ atomically $ writeTVar (envBootstrap syncEnv) False
    NoLedger _ ->
      liftIO $ logWarning trce "Tried to bootstrap, but ledger state is not enabled. Please stop db-sync and restart without --disable-ledger-state"

storeUTxOFromLedger :: TxCache -> ExtLedgerState CardanoBlock -> App ()
storeUTxOFromLedger txCache st = case ledgerState st of
  LedgerStateBabbage bts -> storeUTxO txCache (getUTxO bts)
  LedgerStateConway stc -> storeUTxO txCache (getUTxO stc)
  _other -> do
    trce <- askTrace
    liftIO $ logError trce "storeUTxOFromLedger is only supported after Babbage"
  where
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
  , DBPlutusScript era
  ) =>
  TxCache ->
  Map (TxIn StandardCrypto) (BabbageTxOut era) ->
  App ()
storeUTxO txCache mp = do
  trce <- askTrace
  liftIO $
    logInfo trce $
      mconcat
        [ "Inserting "
        , DB.textShow size
        , " tx_out as pages of "
        , DB.textShow pageSize
        ]
  mapM_ (storePage txCache pagePerc) . zip [0 ..] . chunksOf pageSize . Map.toList $ mp
  where
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
  ) =>
  TxCache ->
  Float ->
  (Int, [(TxIn StandardCrypto, BabbageTxOut era)]) ->
  App ()
storePage cache percQuantum (n, ls) = do
  trce <- askTrace
  when (n `mod` 10 == 0) $ liftIO $ logInfo trce $ "Bootstrap in progress " <> prc <> "%"
  txOuts <- mapM (prepareTxOut cache) ls
  txOutIds <- dbQueryToApp . DB.insertManyTxOutPlex True False $ etoTxOut . fst <$> txOuts
  let maTxOuts = concatMap mkmaTxOuts $ zip txOutIds (snd <$> txOuts)
  void . dbQueryToApp $ DB.insertManyMaTxOut maTxOuts
  where
    prc = Text.pack $ showGFloat (Just 1) (max 0 $ min 100.0 (fromIntegral n * percQuantum)) ""

prepareTxOut ::
  ( EraCrypto era ~ StandardCrypto
  , Cardano.Ledger.Core.Value era ~ MaryValue StandardCrypto
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , BabbageEraTxOut era
  , DBPlutusScript era
  ) =>
  TxCache ->
  (TxIn StandardCrypto, BabbageTxOut era) ->
  App (ExtendedTxOut, [MissingMaTxOut])
prepareTxOut txCache (TxIn txHash (TxIx index), txOut) = do
  let txHashByteString = Generic.safeHashToByteString $ unTxId txHash
  let genTxOut = fromTxOut index txOut
  txId <- queryTxIdWithCache txCache txHashByteString
  insertTxOut (txId, txHashByteString) genTxOut

queryTxIdWithCache :: TxCache -> ByteString -> App DB.TxId
queryTxIdWithCache (TxCache mp) hsh = do
  case Map.lookup hsh mp of
    Just txId -> pure txId
    Nothing -> liftLookupFail "queryTxIdWithCache" $ dbQueryToApp $ DB.queryTxId hsh
