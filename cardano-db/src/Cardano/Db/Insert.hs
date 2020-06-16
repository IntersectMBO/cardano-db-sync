{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Insert
  ( insertBlock
  , insertEpoch
  , insertMeta
  , insertPool
  , insertPoolMetaData
  , insertPoolOwner
  , insertPoolRetire
  , insertSlotLeader
  , insertStakeAddress
  , insertTx
  , insertTxIn
  , insertTxOut

  -- Export mainly for testing.
  , insertByReturnKey
  ) where


import           Control.Exception.Lifted (Exception, handle, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Class (AtLeastOneUniqueKey, Key, PersistEntityBackend,
                    getByValue, insert)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (entityKey)
import           Database.PostgreSQL.Simple (SqlError)

import           Cardano.Db.Schema


insertBlock :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlock = insertByReturnKey "Block"

insertEpoch :: (MonadBaseControl IO m, MonadIO m) => Epoch -> ReaderT SqlBackend m EpochId
insertEpoch = insertByReturnKey "Epoch"

insertMeta :: (MonadBaseControl IO m, MonadIO m) => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertByReturnKey "Meta"

insertPool :: (MonadBaseControl IO m, MonadIO m) => Pool -> ReaderT SqlBackend m PoolId
insertPool = insertByReturnKey "Pool"

insertPoolMetaData :: (MonadBaseControl IO m, MonadIO m) => PoolMetaData -> ReaderT SqlBackend m PoolMetaDataId
insertPoolMetaData = insertByReturnKey "PoolMetaData"

insertPoolOwner :: (MonadBaseControl IO m, MonadIO m) => PoolOwner -> ReaderT SqlBackend m PoolOwnerId
insertPoolOwner = insertByReturnKey "PoolOwner"

insertPoolRetire :: (MonadBaseControl IO m, MonadIO m) => PoolRetire -> ReaderT SqlBackend m PoolRetireId
insertPoolRetire = insertByReturnKey "PoolRetire"

insertSlotLeader :: (MonadBaseControl IO m, MonadIO m) => SlotLeader -> ReaderT SqlBackend m SlotLeaderId
insertSlotLeader = insertByReturnKey "SlotLeader"

insertStakeAddress :: (MonadBaseControl IO m, MonadIO m) => StakeAddress -> ReaderT SqlBackend m StakeAddressId
insertStakeAddress = insertByReturnKey "StakeAddress"

insertTx :: (MonadBaseControl IO m, MonadIO m) => Tx -> ReaderT SqlBackend m TxId
insertTx = insertByReturnKey "Tx"

insertTxIn :: (MonadBaseControl IO m, MonadIO m) => TxIn -> ReaderT SqlBackend m TxInId
insertTxIn = insertByReturnKey "TxIn"

insertTxOut :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOut = insertByReturnKey "TxOut"

-- -----------------------------------------------------------------------------

data DbInsertException
  = DbInsertException String SqlError
  deriving Show

instance Exception DbInsertException

-- | Insert a record (with a Unique constraint), and return 'Right key' if the
-- record is inserted and 'Left key' if the record already exists in the DB.
insertByReturnKey
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , MonadBaseControl IO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertByReturnKey vtype value = do
    res <- getByValue value
    case res of
      Nothing -> handle exceptHandler $ insert value
      Just r -> pure $ entityKey r
  where
    exceptHandler :: MonadIO m => SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)
