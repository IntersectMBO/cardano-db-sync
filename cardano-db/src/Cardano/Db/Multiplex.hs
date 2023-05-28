{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Db.Multiplex (
  insertTxOutPlex,
  insertManyTxOutPlex,
  updateListTxOutConsumedByTxInId,
) where

import Cardano.Db.Insert
import Cardano.Db.Schema
import qualified Cardano.Db.Migration.Extra.CosnumedTxOut.Queries as ExtraCons
import qualified Cardano.Db.Migration.Extra.CosnumedTxOut.Schema as ExtraCons
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend, ToBackendKey (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

insertTxOutPlex ::
  (MonadBaseControl IO m, MonadIO m) =>
  Bool ->
  TxOut ->
  ReaderT SqlBackend m TxOutId
insertTxOutPlex hasConsMigration txOut = do
    if hasConsMigration then
      changeKey <$> ExtraCons.insertTxOutExtra (toExtraTxOut txOut)
    else
      insertTxOut txOut

insertManyTxOutPlex :: (MonadBaseControl IO m, MonadIO m) => Bool -> [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOutPlex hasConsMigration txOuts =
  if hasConsMigration then
    fmap changeKey <$> ExtraCons.insertManyTxOutExtra (toExtraTxOut <$> txOuts)
  else
    insertManyTxOut txOuts

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
updateListTxOutConsumedByTxInId ls =
    updateListTxOutConsumedByTxInId (f <$> ls)
  where
    f (txOutId, txInId) = (changeKey txOutId, changeKey txInId)
