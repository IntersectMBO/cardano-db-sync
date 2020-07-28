{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Insert
  ( insertBlock
  , insertDelegation
  , insertEpoch
  , insertMeta
  , insertParamUpdate
  , insertPoolHash
  , insertPoolMetaData
  , insertPoolOwner
  , insertPoolRelay
  , insertPoolRetire
  , insertPoolUpdate
  , insertReserve
  , insertSlotLeader
  , insertStakeAddress
  , insertStakeRegistration
  , insertTreasury
  , insertTx
  , insertTxIn
  , insertTxOut
  , insertWithdrawal

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

insertDelegation :: (MonadBaseControl IO m, MonadIO m) => Delegation -> ReaderT SqlBackend m DelegationId
insertDelegation = insertByReturnKey "Delegation"

insertEpoch :: (MonadBaseControl IO m, MonadIO m) => Epoch -> ReaderT SqlBackend m EpochId
insertEpoch = insertByReturnKey "Epoch"

insertMeta :: (MonadBaseControl IO m, MonadIO m) => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertByReturnKey "Meta"

insertParamUpdate :: (MonadBaseControl IO m, MonadIO m) => ParamUpdate -> ReaderT SqlBackend m ParamUpdateId
insertParamUpdate = insertByReturnKey "ParamUpdate"

insertPoolHash :: (MonadBaseControl IO m, MonadIO m) => PoolHash -> ReaderT SqlBackend m PoolHashId
insertPoolHash = insertByReturnKey "PoolHash"

insertPoolMetaData :: (MonadBaseControl IO m, MonadIO m) => PoolMetaData -> ReaderT SqlBackend m PoolMetaDataId
insertPoolMetaData = insertByReturnKey "PoolMetaData"

insertPoolOwner :: (MonadBaseControl IO m, MonadIO m) => PoolOwner -> ReaderT SqlBackend m PoolOwnerId
insertPoolOwner = insertByReturnKey "PoolOwner"

insertPoolRelay :: (MonadBaseControl IO m, MonadIO m) => PoolRelay -> ReaderT SqlBackend m PoolRelayId
insertPoolRelay = insertByReturnKey "PoolRelay"

insertPoolRetire :: (MonadBaseControl IO m, MonadIO m) => PoolRetire -> ReaderT SqlBackend m PoolRetireId
insertPoolRetire = insertByReturnKey "PoolRetire"

insertPoolUpdate :: (MonadBaseControl IO m, MonadIO m) => PoolUpdate -> ReaderT SqlBackend m PoolUpdateId
insertPoolUpdate = insertByReturnKey "PoolUpdate"

insertReserve :: (MonadBaseControl IO m, MonadIO m) => Reserve -> ReaderT SqlBackend m ReserveId
insertReserve = insertByReturnKey "Reserve"

insertSlotLeader :: (MonadBaseControl IO m, MonadIO m) => SlotLeader -> ReaderT SqlBackend m SlotLeaderId
insertSlotLeader = insertByReturnKey "SlotLeader"

insertStakeAddress :: (MonadBaseControl IO m, MonadIO m) => StakeAddress -> ReaderT SqlBackend m StakeAddressId
insertStakeAddress = insertByReturnKey "StakeAddress"

insertStakeRegistration :: (MonadBaseControl IO m, MonadIO m) => StakeRegistration -> ReaderT SqlBackend m StakeRegistrationId
insertStakeRegistration = insertByReturnKey "StakeRegistration"

insertTreasury :: (MonadBaseControl IO m, MonadIO m) => Treasury -> ReaderT SqlBackend m TreasuryId
insertTreasury = insertByReturnKey "Treasury"

insertTx :: (MonadBaseControl IO m, MonadIO m) => Tx -> ReaderT SqlBackend m TxId
insertTx = insertByReturnKey "Tx"

insertTxIn :: (MonadBaseControl IO m, MonadIO m) => TxIn -> ReaderT SqlBackend m TxInId
insertTxIn = insertByReturnKey "TxIn"

insertTxOut :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOut = insertByReturnKey "TxOut"

insertWithdrawal :: (MonadBaseControl IO m, MonadIO m) => Withdrawal  -> ReaderT SqlBackend m WithdrawalId
insertWithdrawal = insertByReturnKey "Withdrawal"

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
