{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Migration.Extra.CosnumedTxOut.Queries where

import Cardano.BM.Trace (Trace, logError)
import Cardano.Db.Text
import Control.Monad.Extra (when, whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Cardano.Db.Insert (insertMany', insertUnchecked)
import Cardano.Db.Migration.Extra.CosnumedTxOut.Schema
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Database.Persist ((=.))
import Database.Persist.Class (update)
import Database.Esqueleto.Experimental hiding (update, (=.))

insertTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOutExtra = insertUnchecked "TxOutExtra"

insertManyTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOutExtra = insertMany' "TxOut"

updateListTxOutConsumedByTxInId :: MonadIO m => [(TxOutId, TxInId)] -> ReaderT SqlBackend m ()
updateListTxOutConsumedByTxInId = mapM_ (uncurry updateTxOutConsumedByTxInId)

updateTxOutConsumedByTxInId :: MonadIO m => TxOutId -> TxInId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxInId txOutId txInId =
    update txOutId [TxOutConsumedByTxInId =. Just txInId]

setNullTxOut :: MonadIO m => Trace IO Text -> Maybe TxInId -> Word64 -> ReaderT SqlBackend m ()
setNullTxOut trce mMinTxInId txInDeleted = do
  whenJust mMinTxInId $ \txInId -> do
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
getTxOutConsumedAfter :: MonadIO m => TxInId -> ReaderT SqlBackend m [TxOutId]
getTxOutConsumedAfter txInId = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (txOut ^. TxOutConsumedByTxInId >=. just (val txInId))
    pure $ txOut ^. persistIdField
  pure $ unValue <$> res

-- | This requires an index at TxOutConsumedByTxInId.
setNullTxOutConsumedAfterTxInId :: MonadIO m => TxOutId -> ReaderT SqlBackend m ()
setNullTxOutConsumedAfterTxInId txOutId = do
    update txOutId [TxOutConsumedByTxInId =. Nothing]
