{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.DB.Insert
  ( insertBlock
  , insertTx
  , insertTxIn
  , insertTxOut

  -- Export mainly for testing.
  , insertByReturnKeyE
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Class (AtLeastOneUniqueKey, Key, PersistEntityBackend,
                    getByValue, insert)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (entityKey)

import           Explorer.DB.Schema


insertBlock :: MonadIO m => Block -> ReaderT SqlBackend m (Either BlockId BlockId)
insertBlock = insertByReturnKeyE

insertTx :: MonadIO m => Tx -> ReaderT SqlBackend m (Either TxId TxId)
insertTx = insertByReturnKeyE

insertTxIn :: MonadIO m => TxIn -> ReaderT SqlBackend m (Either TxInId TxInId)
insertTxIn = insertByReturnKeyE

insertTxOut :: MonadIO m => TxOut -> ReaderT SqlBackend m (Either TxOutId TxOutId)
insertTxOut = insertByReturnKeyE

-- -----------------------------------------------------------------------------

-- | Insert a record (with a Unique constraint), and return 'Right key' if the
-- record is inserted and 'Left key' if the record already exists in the DB.
insertByReturnKeyE
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => record -> ReaderT SqlBackend m (Either (Key record) (Key record))
insertByReturnKeyE value = do
  res <- getByValue value
  case res of
    Nothing -> Right <$> insert value
    Just r -> pure $ Left (entityKey r)
