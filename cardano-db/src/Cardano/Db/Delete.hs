{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Delete
  ( deleteBlocksSlotNo
  , deleteDelistedPool
  , deleteBlocksBlockId
  , deleteBlock
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.Extra (whenJust)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Esqueleto.Experimental (PersistEntity, PersistField, persistIdField)
import           Database.Persist.Sql (PersistEntityBackend, SqlBackend, delete, selectKeysList, (==.), (>=.))
import           Database.Persist.Class.PersistQuery (deleteWhere)
import           Data.ByteString (ByteString)

import           Cardano.Db.MinId
import           Cardano.Db.Query
import           Cardano.Db.Schema
import           Cardano.Db.Text

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlocksSlotNo :: MonadIO m => SlotNo -> ReaderT SqlBackend m Bool
deleteBlocksSlotNo (SlotNo slotNo) = do
  mBlockId <- queryBlockSlotNo slotNo
  case mBlockId of
    Nothing -> pure False
    Just blockId -> do
      mDel <- deleteBlocksBlockId blockId
      pure $ either (const False) (const True) mDel

deleteBlocksBlockId :: MonadIO m => BlockId -> (ReaderT SqlBackend m) (Either LookupFail ())
deleteBlocksBlockId blockId = do
    mRollBackId <- queryReverseIndexBlockId blockId
    case mRollBackId of
      Nothing -> pure $ Left $ DbLookupMessage $ "Failed to find RollbackIndex for " <> textShow blockId
      Just rlbId -> do
        infos <- fmap textToMinId <$> queryMinIdsAfterReverseIndex rlbId
        let minIds = mconcat infos
        deleteTablesAfterBlockId blockId (minTxId minIds) (minTxInId minIds) (minTxOutId minIds) (minMaTxOutId minIds)
        pure $ Right ()

deleteTablesAfterBlockId :: MonadIO m => BlockId -> Maybe TxId -> Maybe TxInId -> Maybe TxOutId -> Maybe MaTxOutId -> ReaderT SqlBackend m ()
deleteTablesAfterBlockId blkId mtxId mtxInId mtxOutId mmaTxOutId = do
    deleteWhere [AdaPotsBlockId >=. blkId]
    deleteWhere [ReverseIndexBlockId >=. blkId]
    deleteWhere [EpochParamBlockId >=. blkId]
    deleteTablesAfterTxId mtxId mtxInId mtxOutId mmaTxOutId
    deleteWhere [BlockId >=. blkId]

deleteTablesAfterTxId :: MonadIO m => Maybe TxId -> Maybe TxInId -> Maybe TxOutId -> Maybe MaTxOutId -> ReaderT SqlBackend m ()
deleteTablesAfterTxId mtxId mtxInId mtxOutId mmaTxOutId = do
    whenJust mtxInId $ \txInId -> deleteWhere [TxInId >=. txInId]
    whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [MaTxOutId >=. maTxOutId]
    whenJust mtxOutId $ \txOutId -> deleteWhere [TxOutId >=. txOutId]

    whenJust mtxId $ \txId -> do
      queryFirstAndDeleteAfter CollateralTxOutTxId txId
      queryFirstAndDeleteAfter CollateralTxInTxInId txId
      queryFirstAndDeleteAfter ReferenceTxInTxInId txId
      queryFirstAndDeleteAfter PoolMetadataRefRegisteredTxId txId
      queryFirstAndDeleteAfter PoolRetireAnnouncedTxId txId
      queryFirstAndDeleteAfter StakeRegistrationTxId txId
      queryFirstAndDeleteAfter StakeDeregistrationTxId txId
      queryFirstAndDeleteAfter DelegationTxId txId
      queryFirstAndDeleteAfter TxMetadataTxId txId
      queryFirstAndDeleteAfter WithdrawalTxId txId
      queryFirstAndDeleteAfter TreasuryTxId txId
      queryFirstAndDeleteAfter ReserveTxId txId
      queryFirstAndDeleteAfter PotTransferTxId txId
      queryFirstAndDeleteAfter MaTxMintTxId txId
      queryFirstAndDeleteAfter RedeemerTxId txId
      queryFirstAndDeleteAfter ScriptTxId txId
      queryFirstAndDeleteAfter DatumTxId txId
      queryFirstAndDeleteAfter RedeemerDataTxId txId
      queryFirstAndDeleteAfter ExtraKeyWitnessTxId txId
      queryFirstAndDeleteAfter ParamProposalRegisteredTxId txId
      minPoolUpdate <- queryMinRefId PoolUpdateRegisteredTxId txId
      whenJust minPoolUpdate $ \puid -> do
        queryFirstAndDeleteAfter PoolOwnerPoolUpdateId puid
        queryFirstAndDeleteAfter PoolRelayUpdateId puid
        deleteWhere [PoolUpdateId >=. puid]
      deleteWhere [TxId >=. txId]

queryFirstAndDeleteAfter
  :: forall m record field. (MonadIO m, PersistEntity record, PersistField field, PersistEntityBackend record ~ SqlBackend)
  => EntityField record field -> field -> ReaderT SqlBackend m ()
queryFirstAndDeleteAfter txIdField txId = do
    mRecordId <- queryMinRefId txIdField txId
    whenJust mRecordId $ \recordId ->
      deleteWhere [persistIdField @record >=. recordId]

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
deleteDelistedPool poolHash = do
  keys <- selectKeysList [ DelistedPoolHashRaw ==. poolHash ] []
  mapM_ delete keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteBlock block = do
  mBlockId <- listToMaybe <$> selectKeysList [ BlockHash ==. blockHash block ] []
  case mBlockId of
    Nothing -> pure False
    Just blockId -> do
      mDel <- deleteBlocksBlockId blockId
      pure $ either (const False) (const True) mDel
