{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Update (
  setNullTxOut,
  updateTxOutConsumedByTxInId,
  updateListTxOutConsumedByTxInId
) where

import Cardano.BM.Trace (Trace, logError)
import Cardano.Db.MinId
import Cardano.Db.Schema
import Cardano.Db.Text
import Cardano.Db.Query
import Control.Monad.Extra (when, whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Database.Persist ((=.))
import Database.Persist.Class (update)
import Database.Persist.Sql (SqlBackend)

updateListTxOutConsumedByTxInId :: MonadIO m => [(TxOutId, TxInId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxInId = mapM_ (uncurry updateTxOutConsumedByTxInId)

updateTxOutConsumedByTxInId :: MonadIO m => TxOutId -> TxInId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxInId txOutId txInId =
    update txOutId [TxOutConsumedByTxInId =. Just txInId]

setNullTxOut :: MonadIO m => Trace IO Text -> MinIds -> Word64 -> ReaderT SqlBackend m ()
setNullTxOut trce minIds txInDeleted = do
  whenJust (minTxInId minIds) $ \txInId -> do
    txOutIds <- getTxOutConsumedAfter txInId
    mapM_ setNullTxOutConsumedAfterTxInId txOutIds
    let updatedEntries = fromIntegral (length txOutIds)
    when (updatedEntries /= txInDeleted) $
      liftIO $ logError trce $
        Text.concat
          [ "Deleted "
          , textShow txInDeleted
          , " inputs, but set to null only "
          , textShow updatedEntries
          , "consumed outputs. Please file an issue at https://github.com/input-output-hk/cardano-db-sync/issues"
          ]

-- | This requires an index at TxOutConsumedByTxInId.
setNullTxOutConsumedAfterTxInId :: MonadIO m => TxOutId -> ReaderT SqlBackend m ()
setNullTxOutConsumedAfterTxInId txOutId = do
    update txOutId [TxOutConsumedByTxInId =. Nothing]
