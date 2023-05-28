{-# LANGUAGE FlexibleContexts #-}

module Cardano.Db.Migration.Extra.CosnumedTxOut.Queries where

import Cardano.Db.Insert (insertMany', insertUnchecked)
import Cardano.Db.Migration.Extra.CosnumedTxOut.Schema
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist ((=.))
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Class (update)

insertTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOutExtra = insertUnchecked "TxOutExtra"

insertManyTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOutExtra = insertMany' "TxOut"

updateListTxOutConsumedByTxInId :: MonadIO m => [(TxOutId, TxInId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxInId = mapM_ (uncurry updateTxOutConsumedByTxInId)

updateTxOutConsumedByTxInId :: MonadIO m => TxOutId -> TxInId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxInId txOutId txInId =
    update txOutId [TxOutConsumedByTxInId =. Just txInId]
