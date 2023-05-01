{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Migration.TxOut (
  migrateTxOut,
  deleteConsumedTxOut,
  isMigrated,
) where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import Cardano.Db.Query
import Cardano.Db.Schema
import Cardano.Db.Text
import Cardano.Db.Update
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Word (Word64)
import Database.Persist ((<=.))
import Database.Persist.Sql (SqlBackend, rawExecuteCount)
import Database.Persist.Sql (deleteWhereCount)

migrateTxOut :: MonadIO m => ReaderT SqlBackend m ()
migrateTxOut = migrateNextPage 0
  where
  migrateNextPage :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
  migrateNextPage offset = do
    liftIO $ print offset
    page <- getInputPage offset pageSize
    mapM_ migratePair page
    when (fromIntegral (length page) == pageSize) $
      migrateNextPage $! offset + pageSize

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

-- runMigration :: MonadIO m => ReaderT SqlBackend m ()
-- runMigration = do
--   

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
      count <- deleteWhereCount [TxOutConsumedByTxInId <=. Just txInId]
      liftIO $ logInfo trce $ "Deleted " <> textShow count <> " tx_in"
