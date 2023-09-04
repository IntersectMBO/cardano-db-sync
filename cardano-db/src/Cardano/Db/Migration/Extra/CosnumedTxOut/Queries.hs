{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Migration.Extra.CosnumedTxOut.Queries where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import Cardano.Db.Insert (insertMany', insertUnchecked)
import Cardano.Db.Migration.Extra.CosnumedTxOut.Schema
import Cardano.Db.Query (isJust, listToMaybe, queryBlockHeight, queryMaxRefId)
import Cardano.Db.Text
import Control.Monad.Extra (when, whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Database.Esqueleto.Experimental hiding (update, (<=.), (=.), (==.))
import qualified Database.Esqueleto.Experimental as Experimental
import Database.Persist ((<=.), (=.), (==.))
import Database.Persist.Class (update)
import Database.Persist.Sql (deleteWhereCount)

insertTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOutExtra = insertUnchecked "TxOutExtra"

insertManyTxOutExtra :: (MonadBaseControl IO m, MonadIO m) => [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOutExtra = insertMany' "TxOut"

queryUpdateListTxOutConsumedByTxInId :: MonadIO m => [(TxOutId, TxInId)] -> ReaderT SqlBackend m ()
queryUpdateListTxOutConsumedByTxInId ls = do
  mapM_ (uncurry updateTxOutConsumedByTxInId) ls

updateTxOutConsumedByTxInId :: MonadIO m => TxOutId -> TxInId -> ReaderT SqlBackend m ()
updateTxOutConsumedByTxInId txOutId txInId =
  update txOutId [TxOutConsumedByTxInId =. Just txInId]

querySetNullTxOut :: MonadIO m => Trace IO Text -> Maybe TxInId -> Word64 -> ReaderT SqlBackend m ()
querySetNullTxOut trce mMinTxInId txInDeleted = do
  whenJust mMinTxInId $ \txInId -> do
    txOutIds <- getTxOutConsumedAfter txInId
    mapM_ setNullTxOutConsumedAfterTxInId txOutIds
    let updatedEntries = fromIntegral (length txOutIds)
    when (updatedEntries /= txInDeleted) $
      liftIO $
        logError trce $
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

migrateTxOut :: MonadIO m => Maybe (Trace IO Text) -> ReaderT SqlBackend m ()
migrateTxOut mTrace = do
  createConsumedTxOut
  migrateNextPage 0
  where
    migrateNextPage :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    migrateNextPage offst = do
      whenJust mTrace $ \trce ->
        liftIO $ logInfo trce $ "Handling input offset " <> textShow offst
      page <- getInputPage offst pageSize
      mapM_ migratePair page
      when (fromIntegral (length page) == pageSize) $
        migrateNextPage $!
          offst
            + pageSize

migratePair :: MonadIO m => (TxInId, TxId, Word64) -> ReaderT SqlBackend m ()
migratePair (txInId, txId, index) =
  updateTxOutConsumedByTxInIdUnique txId index txInId

pageSize :: Word64
pageSize = 100_000

queryTxConsumedColumnExists :: MonadIO m => ReaderT SqlBackend m Bool
queryTxConsumedColumnExists = do
  columntExists :: [Text] <-
    fmap unSingle
      <$> rawSql
        ( mconcat
            [ "SELECT column_name FROM information_schema.columns "
            , "WHERE table_name='tx_out' and column_name='consumed_by_tx_in_id'"
            ]
        )
        []
  pure (not $ null columntExists)

-- | This is a count of the null consumed_by_tx_in_id
queryTxOutConsumedNullCount :: MonadIO m => ReaderT SqlBackend m Word64
queryTxOutConsumedNullCount = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (isNothing $ txOut ^. TxOutConsumedByTxInId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

queryTxOutConsumedCount :: MonadIO m => ReaderT SqlBackend m Word64
queryTxOutConsumedCount = do
  res <- select $ do
    txOut <- from $ table @TxOut
    where_ (not_ $ isNothing $ txOut ^. TxOutConsumedByTxInId)
    pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

createConsumedTxOut :: MonadIO m => ReaderT SqlBackend m ()
createConsumedTxOut = do
  rawExecute
    "ALTER TABLE tx_out ADD COLUMN consumed_by_tx_in_id INT8 NULL"
    []
  rawExecute
    "CREATE INDEX IF NOT EXISTS idx_tx_out_consumed_by_tx_in_id ON tx_out (consumed_by_tx_in_id)"
    []
  rawExecute
    "ALTER TABLE ma_tx_out ADD CONSTRAINT ma_tx_out_tx_out_id_fkey FOREIGN KEY(tx_out_id) REFERENCES tx_out(id) ON DELETE CASCADE ON UPDATE RESTRICT"
    []

_validateMigration :: MonadIO m => Trace IO Text -> ReaderT SqlBackend m Bool
_validateMigration trce = do
  _migrated <- queryTxConsumedColumnExists
  --  unless migrated $ runMigration
  txInCount <- countTxIn
  consumedTxOut <- countConsumed
  if txInCount > consumedTxOut
    then do
      liftIO $
        logWarning trce $
          mconcat
            [ "Found incomplete TxOut migration. There are"
            , textShow txInCount
            , " TxIn, but only"
            , textShow consumedTxOut
            , " consumed TxOut"
            ]
      pure False
    else
      if txInCount == consumedTxOut
        then do
          liftIO $ logInfo trce "Found complete TxOut migration"
          pure True
        else do
          liftIO $
            logError trce $
              mconcat
                [ "The impossible happened! There are"
                , textShow txInCount
                , " TxIn, but "
                , textShow consumedTxOut
                , " consumed TxOut"
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

deleteConsumedTxOut :: forall m. MonadIO m => Trace IO Text -> Word64 -> ReaderT SqlBackend m ()
deleteConsumedTxOut trce blockNoDiff = do
  mBlockHeight <- queryBlockHeight
  maybe (logNoDelete "No blocks found") deleteConsumed mBlockHeight
  where
    logNoDelete txt = liftIO $ logInfo trce $ "No tx_out was deleted: " <> txt

    deleteConsumed :: Word64 -> ReaderT SqlBackend m ()
    deleteConsumed tipBlockNo = do
      if tipBlockNo <= blockNoDiff
        then logNoDelete $ "Tip blockNo is " <> textShow tipBlockNo
        else do
          mBlockId <- queryBlockNo $ tipBlockNo - blockNoDiff
          maybe
            (liftIO $ logError trce $ "BlockNo hole found at " <> textShow (tipBlockNo - blockNoDiff))
            deleteConsumedBeforeBlock
            mBlockId

    deleteConsumedBeforeBlock :: BlockId -> ReaderT SqlBackend m ()
    deleteConsumedBeforeBlock blockId = do
      mTxId <- queryMaxRefId TxBlockId blockId False
      case mTxId of
        Nothing -> logNoDelete $ "No txs found before " <> textShow blockId
        Just txId -> do
          mTxInId <- queryMaxRefId TxInTxInId txId True
          maybe
            (logNoDelete $ "No tx_in found before or at " <> textShow txId)
            deleteConsumedBeforeTxIn
            mTxInId

    deleteConsumedBeforeTxIn :: TxInId -> ReaderT SqlBackend m ()
    deleteConsumedBeforeTxIn txInId = do
      countDeleted <- deleteWhereCount [TxOutConsumedByTxInId <=. Just txInId]
      liftIO $ logInfo trce $ "Deleted " <> textShow countDeleted <> " tx_out"

queryBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockNo blkNo = do
  res <- select $ do
    blk <- from $ table @Block
    where_ (blk ^. BlockBlockNo Experimental.==. just (val blkNo))
    pure (blk ^. BlockId)
  pure $ fmap unValue (listToMaybe res)
