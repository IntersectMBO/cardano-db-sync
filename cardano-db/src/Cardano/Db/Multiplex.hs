{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Multiplex (
  insertTxOutPlex,
  insertManyTxOutPlex,
  updateListTxOutConsumedByTxInId,
  setNullTxOut,
  runExtraMigrations,
  ExtraCons.deleteConsumedTxOut,
) where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Db.Error (LookupFail (..), logAndThrowIO)
import Cardano.Db.Insert
import qualified Cardano.Db.Migration.Extra.CosnumedTxOut.Queries as ExtraCons
import qualified Cardano.Db.Migration.Extra.CosnumedTxOut.Schema as ExtraCons
import Cardano.Db.Query (queryAllExtraMigrations)
import Cardano.Db.Schema
import Cardano.Db.Types (ExtraMigration (..), PruneConsumeMigration (..), wasPruneTxOutPreviouslySet)
import Control.Exception (throw)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Word (Word64)
import Database.Persist.Sql (SqlBackend, ToBackendKey (..))

insertTxOutPlex ::
  (MonadBaseControl IO m, MonadIO m) =>
  Bool ->
  TxOut ->
  ReaderT SqlBackend m TxOutId
insertTxOutPlex hasConsMigration txOut = do
  if hasConsMigration
    then changeKey <$> ExtraCons.insertTxOutExtra (toExtraTxOut txOut)
    else insertTxOut txOut

insertManyTxOutPlex :: (MonadBaseControl IO m, MonadIO m) => Bool -> [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOutPlex hasConsMigration txOuts =
  if hasConsMigration
    then fmap changeKey <$> ExtraCons.insertManyTxOutExtra (toExtraTxOut <$> txOuts)
    else insertManyTxOut txOuts

changeKey ::
  ( ToBackendKey SqlBackend record1
  , ToBackendKey SqlBackend record2
  ) =>
  Key record1 ->
  Key record2
changeKey = fromBackendKey . toBackendKey

toExtraTxOut :: TxOut -> ExtraCons.TxOut
toExtraTxOut txOut =
  ExtraCons.TxOut
    { ExtraCons.txOutTxId = changeKey $ txOutTxId txOut
    , ExtraCons.txOutIndex = txOutIndex txOut
    , ExtraCons.txOutAddress = txOutAddress txOut
    , ExtraCons.txOutAddressRaw = txOutAddressRaw txOut
    , ExtraCons.txOutAddressHasScript = txOutAddressHasScript txOut
    , ExtraCons.txOutPaymentCred = txOutPaymentCred txOut
    , ExtraCons.txOutStakeAddressId = changeKey <$> txOutStakeAddressId txOut
    , ExtraCons.txOutValue = txOutValue txOut
    , ExtraCons.txOutDataHash = txOutDataHash txOut
    , ExtraCons.txOutInlineDatumId = changeKey <$> txOutInlineDatumId txOut
    , ExtraCons.txOutReferenceScriptId = changeKey <$> txOutReferenceScriptId txOut
    , ExtraCons.txOutConsumedByTxInId = Nothing
    }

updateListTxOutConsumedByTxInId :: MonadIO m => [(TxOutId, TxInId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxInId ls = do
  ExtraCons.queryUpdateListTxOutConsumedByTxInId (f <$> ls)
  where
    f (txOutId, txInId) = (changeKey txOutId, changeKey txInId)

setNullTxOut :: MonadIO m => Trace IO Text -> Maybe TxInId -> Word64 -> ReaderT SqlBackend m ()
setNullTxOut trce mMinTxInId =
  ExtraCons.querySetNullTxOut trce (changeKey <$> mMinTxInId)

runExtraMigrations :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Word64 -> PruneConsumeMigration -> ReaderT SqlBackend m ()
runExtraMigrations trce blockNoDiff PruneConsumeMigration {..} = do
  hasConsumedField <- ExtraCons.queryTxConsumedColumnExists
  ems <- queryAllExtraMigrations
  let wPruneTxOutPreviouslySet = wasPruneTxOutPreviouslySet ems
  -- first check if pruneTxOut flag is missing and it has previously been used
  case (pcmPruneTxOut, wPruneTxOutPreviouslySet) of
    (False, True) ->
      throw $
        DBExtraMigration
          ( "If --prune-tx-out flag is enabled and then db-sync is stopped all future executions of db-sync "
              <> "should still have this flag activated. Otherwise, it is considered bad usage and can cause crashes."
          )
    _ -> do
      case (hasConsumedField, pcmConsumeOrPruneTxOut, pcmPruneTxOut) of
        (False, False, False) -> do
          liftIO $ logInfo trce "No extra migration specified"
        (True, True, False) -> do
          liftIO $ logInfo trce "Extra migration consumed_tx_out already executed"
        (True, False, False) -> liftIO $ logAndThrowIO trce migratedButNotSet
        (False, True, False) -> do
          liftIO $ logInfo trce "Running extra migration consumed_tx_out"
          ExtraCons.migrateTxOut (Just trce)
        (False, _, True) -> do
          shouldInsertToMigrationTable
          liftIO $ logInfo trce "Running extra migrations consumed_tx_out and prune tx_out"
          ExtraCons.migrateTxOut (Just trce)
          liftIO $ logInfo trce "Now Running extra migration prune tx_out"
          ExtraCons.deleteConsumedTxOut trce blockNoDiff
        (True, _, True) -> do
          shouldInsertToMigrationTable
          liftIO $ logInfo trce "Running extra migration prune tx_out"
          ExtraCons.deleteConsumedTxOut trce blockNoDiff
      where
        migratedButNotSet = "consumed-tx-out or prune-tx-out is not set, but consumed migration is found."
        -- if PruneTxOutFlagPreviouslySet isn't already set then set it.
        shouldInsertToMigrationTable :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m ()
        shouldInsertToMigrationTable = do
          unless wPruneTxOutPreviouslySet $ insertExtraMigration PruneTxOutFlagPreviouslySet
