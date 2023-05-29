{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Migration.Extra.CosnumedTxOut.Queries where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
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
import Database.Persist ((=.), (==.))
import Database.Persist.Class (update)
import Database.Esqueleto.Experimental hiding (update, (=.), (==.))
import Cardano.Db.Query (isJust, listToMaybe)

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

migrateTxOut :: MonadIO m => ReaderT SqlBackend m ()
migrateTxOut = migrateNextPage 0
  where
  migrateNextPage :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
  migrateNextPage offst = do
    liftIO $ print offst -- TODO delete
    page <- getInputPage offst pageSize
    mapM_ migratePair page
    when (fromIntegral (length page) == pageSize) $
      migrateNextPage $! offst + pageSize

migratePair :: MonadIO m => (TxInId, TxId, Word64) -> ReaderT SqlBackend m ()
migratePair (txInId, txId, index) =
    updateTxOutConsumedByTxInIdUnique txId index txInId

pageSize :: Word64
pageSize = 100_000

isMigrated :: MonadIO m => ReaderT SqlBackend m Bool
isMigrated = do
  columntExists <- rawExecuteCount
            ( mconcat
                [ "SELECT column_name FROM information_schema.columns"
                , "WHERE table_name='tx_out' and column_name='consumed_by_tx_in_id'"
                ]
            )
            []
  pure (columntExists >= 1)

_validateMigration :: MonadIO m => Trace IO Text -> ReaderT SqlBackend m Bool
_validateMigration trce = do
  _migrated <- isMigrated
--  unless migrated $ runMigration
  txInCount <- countTxIn
  consumedTxOut <- countConsumed
  if txInCount > consumedTxOut
  then do
    liftIO $ logWarning trce $ mconcat
      ["Found incomplete TxOut migration. There are"
      , textShow txInCount, " TxIn, but only"
      , textShow consumedTxOut, " consumed TxOut"
      ]
    pure False
  else if txInCount == consumedTxOut
  then do
    liftIO $ logInfo trce "Found complete TxOut migration"
    pure True
  else do
    liftIO $ logError trce $ mconcat
      [ "The impossible happened! There are"
      , textShow txInCount, " TxIn, but "
      , textShow consumedTxOut, " consumed TxOut"
      ]
    pure False

updateTxOutConsumedByTxInIdUnique :: MonadIO m => TxId -> Word64 -> TxInId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxInIdUnique txOutId index txInId =
    updateWhere [TxOutTxId ==. txOutId, TxOutIndex ==. index] [TxOutConsumedByTxInId =. Just txInId]

getInputPage :: MonadIO m => Word64 -> Word64 -> ReaderT SqlBackend m [(TxInId, TxId, Word64)]
getInputPage offs pgSize = do
    res <- select $ do
      txIn <- from $ table @TxIn
      limit (fromIntegral pgSize)
      offset (fromIntegral offs)
      orderBy [asc (txIn ^. TxInId)]
      pure txIn
    pure $ convert <$> res
  where
    convert txIn =
      (entityKey txIn, txInTxOutId (entityVal txIn), txInTxOutIndex (entityVal txIn))

countTxIn :: MonadIO m => ReaderT SqlBackend m Word64
countTxIn = do
  res <- select $ do
    _ <- from $ table @TxIn
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

countConsumed :: MonadIO m => ReaderT SqlBackend m Word64
countConsumed = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (isJust $ txOut ^. TxOutConsumedByTxInId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)
