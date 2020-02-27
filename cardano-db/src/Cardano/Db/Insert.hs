{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Insert
  ( insertBlock
  , insertEpoch
  , insertMeta
  , insertSlotLeader
  , insertTx
  , insertTxIn
  , insertTxOut

  -- Export mainly for testing.
  , insertByReturnKey
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Class (AtLeastOneUniqueKey, Key, PersistEntityBackend,
                    getByValue, insert)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (entityKey)

import           Cardano.Db.Schema


insertBlock :: MonadIO m => Block -> ReaderT SqlBackend m BlockId
insertBlock = insertByReturnKey

insertEpoch :: MonadIO m => Epoch -> ReaderT SqlBackend m EpochId
insertEpoch = insertByReturnKey

insertMeta :: MonadIO m => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertByReturnKey

insertSlotLeader :: MonadIO m => SlotLeader -> ReaderT SqlBackend m SlotLeaderId
insertSlotLeader = insertByReturnKey

insertTx :: MonadIO m => Tx -> ReaderT SqlBackend m TxId
insertTx = insertByReturnKey

insertTxIn :: MonadIO m => TxIn -> ReaderT SqlBackend m TxInId
insertTxIn = insertByReturnKey

insertTxOut :: MonadIO m => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOut = insertByReturnKey

-- -----------------------------------------------------------------------------

-- | Insert a record (with a Unique constraint), and return 'Right key' if the
-- record is inserted and 'Left key' if the record already exists in the DB.
insertByReturnKey
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => record -> ReaderT SqlBackend m (Key record)
insertByReturnKey value = do
  res <- getByValue value
  case res of
    Nothing -> insert value
    Just r -> pure $ entityKey r
