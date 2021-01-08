{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Insert
  ( insertBlock
  , insertDelegation
  , insertEpoch
  , insertEpochParam
  , insertEpochStake
  , insertMaTxMint
  , insertMaTxOut
  , insertMeta
  , insertOrphanedReward
  , insertParamProposal
  , insertPoolHash
  , insertPoolMetaData
  , insertPoolOwner
  , insertPoolRelay
  , insertPoolRetire
  , insertPoolUpdate
  , insertReserve
  , insertReward
  , insertSlotLeader
  , insertStakeAddress
  , insertStakeDeregistration
  , insertStakeRegistration
  , insertTreasury
  , insertTx
  , insertTxIn
  , insertTxMetadata
  , insertTxOut
  , insertWithdrawal

  -- Export mainly for testing.
  , insertByReturnKey
  ) where


import           Control.Exception.Lifted (Exception, handle, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Class (AtLeastOneUniqueKey, PersistEntityBackend, getByValue,
                   insert)
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

insertEpochParam :: (MonadBaseControl IO m, MonadIO m) => EpochParam -> ReaderT SqlBackend m EpochParamId
insertEpochParam = insertByReturnKey "EpochParam"

insertEpochStake :: (MonadBaseControl IO m, MonadIO m) => EpochStake -> ReaderT SqlBackend m EpochStakeId
insertEpochStake = insertByReturnKey "EpochStake"

insertMaTxMint :: (MonadBaseControl IO m, MonadIO m) => MaTxMint -> ReaderT SqlBackend m MaTxMintId
insertMaTxMint = insertByReturnKey "insertMaTxMint"

insertMaTxOut :: (MonadBaseControl IO m, MonadIO m) => MaTxOut -> ReaderT SqlBackend m MaTxOutId
insertMaTxOut = insertByReturnKey "insertMaTxOut"

insertMeta :: (MonadBaseControl IO m, MonadIO m) => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertByReturnKey "Meta"

insertOrphanedReward :: (MonadBaseControl IO m, MonadIO m) => OrphanedReward -> ReaderT SqlBackend m OrphanedRewardId
insertOrphanedReward = insertByReturnKey "OrphanedReward"

insertParamProposal :: (MonadBaseControl IO m, MonadIO m) => ParamProposal -> ReaderT SqlBackend m ParamProposalId
insertParamProposal = insertByReturnKey "ParamProposal"

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

insertReward :: (MonadBaseControl IO m, MonadIO m) => Reward -> ReaderT SqlBackend m RewardId
insertReward = insertByReturnKey "Reward"

insertSlotLeader :: (MonadBaseControl IO m, MonadIO m) => SlotLeader -> ReaderT SqlBackend m SlotLeaderId
insertSlotLeader = insertByReturnKey "SlotLeader"

insertStakeAddress :: (MonadBaseControl IO m, MonadIO m) => StakeAddress -> ReaderT SqlBackend m StakeAddressId
insertStakeAddress = insertByReturnKey "StakeAddress"

insertStakeDeregistration :: (MonadBaseControl IO m, MonadIO m) => StakeDeregistration -> ReaderT SqlBackend m StakeDeregistrationId
insertStakeDeregistration = insertByReturnKey "StakeDeregistration"

insertStakeRegistration :: (MonadBaseControl IO m, MonadIO m) => StakeRegistration -> ReaderT SqlBackend m StakeRegistrationId
insertStakeRegistration = insertByReturnKey "StakeRegistration"

insertTreasury :: (MonadBaseControl IO m, MonadIO m) => Treasury -> ReaderT SqlBackend m TreasuryId
insertTreasury = insertByReturnKey "Treasury"

insertTx :: (MonadBaseControl IO m, MonadIO m) => Tx -> ReaderT SqlBackend m TxId
insertTx = insertByReturnKey "Tx"

insertTxIn :: (MonadBaseControl IO m, MonadIO m) => TxIn -> ReaderT SqlBackend m TxInId
insertTxIn = insertByReturnKey "TxIn"

insertTxMetadata :: (MonadBaseControl IO m, MonadIO m) => TxMetadata -> ReaderT SqlBackend m TxMetadataId
insertTxMetadata = insertByReturnKey "TxMetadata"

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
