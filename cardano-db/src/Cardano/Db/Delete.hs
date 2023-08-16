{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Db.Delete (
  deleteBlocksSlotNo,
  deleteBlocksSlotNoNoTrace,
  deleteDelistedPool,
  deleteBlocksBlockId,
  deleteBlocksBlockIdNotrace,
  deleteBlock,
  deleteEpochRows,
  deleteAdaPots,
  -- for testing
  queryFirstAndDeleteAfter,
) where

import Cardano.BM.Trace (Trace, logWarning, nullTracer)
import Cardano.Db.MinId
import Cardano.Db.Query hiding (isJust)
import Cardano.Db.Schema
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (void)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (PersistEntity, PersistField, persistIdField)
import Database.Persist.Class.PersistQuery (deleteWhere)
import Database.Persist.Sql (
  PersistEntityBackend,
  SqlBackend,
  delete,
  deleteWhereCount,
  selectKeysList,
  (==.),
  (>=.),
 )

deleteBlocksSlotNoNoTrace :: MonadIO m => SlotNo -> ReaderT SqlBackend m Bool
deleteBlocksSlotNoNoTrace = deleteBlocksSlotNo nullTracer

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlocksSlotNo :: MonadIO m => Trace IO Text -> SlotNo -> ReaderT SqlBackend m Bool
deleteBlocksSlotNo trce (SlotNo slotNo) = do
  mBlockId <- queryBlockSlotNo slotNo
  case mBlockId of
    Nothing -> pure False
    Just blockId -> do
      void $ deleteBlocksBlockId trce blockId
      pure True

deleteBlocksBlockIdNotrace :: MonadIO m => BlockId -> ReaderT SqlBackend m ()
deleteBlocksBlockIdNotrace = void . deleteBlocksBlockId nullTracer

-- | Delete starting from a 'BlockId'.
deleteBlocksBlockId :: MonadIO m => Trace IO Text -> BlockId -> ReaderT SqlBackend m (MinIds, Word64)
deleteBlocksBlockId trce blockId = do
  mMinIds <- fmap (textToMinId =<<) <$> queryReverseIndexBlockId blockId
  (cminIds, completed) <- findMinIdsRec mMinIds mempty
  mTxId <- queryMinRefId TxBlockId blockId
  minIds <- if completed then pure cminIds else completeMinId mTxId cminIds
  txInDeleted <- deleteTablesAfterBlockId blockId mTxId minIds
  pure (minIds, txInDeleted)
  where
    findMinIdsRec :: MonadIO m => [Maybe MinIds] -> MinIds -> ReaderT SqlBackend m (MinIds, Bool)
    findMinIdsRec [] minIds = pure (minIds, True)
    findMinIdsRec (mMinIds : rest) minIds =
      case mMinIds of
        Nothing -> do
          liftIO $
            logWarning
              trce
              "Failed to find ReverseInex. Deletion may take longer."
          pure (minIds, False)
        Just minIdDB -> do
          let minIds' = minIds <> minIdDB
          if isComplete minIds'
            then pure (minIds', True)
            else findMinIdsRec rest minIds'

    isComplete (MinIds m1 m2 m3) = isJust m1 && isJust m2 && isJust m3

completeMinId :: MonadIO m => Maybe TxId -> MinIds -> ReaderT SqlBackend m MinIds
completeMinId mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <- whenNothingQueryMinRefId (minTxInId minIds) TxInTxInId txId
      mTxOutId <- whenNothingQueryMinRefId (minTxOutId minIds) TxOutTxId txId
      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId -> whenNothingQueryMinRefId (minMaTxOutId minIds) MaTxOutTxOutId txOutId
      pure $ MinIds mTxInId mTxOutId mMaTxOutId

deleteTablesAfterBlockId :: MonadIO m => BlockId -> Maybe TxId -> MinIds -> ReaderT SqlBackend m Word64
deleteTablesAfterBlockId blkId mtxId minIds = do
  deleteWhere [AdaPotsBlockId >=. blkId]
  deleteWhere [ReverseIndexBlockId >=. blkId]
  deleteWhere [EpochParamBlockId >=. blkId]
  txInDeleted <- deleteTablesAfterTxId mtxId (minTxInId minIds) (minTxOutId minIds) (minMaTxOutId minIds)
  deleteWhere [BlockId >=. blkId]
  pure txInDeleted

deleteTablesAfterTxId :: MonadIO m => Maybe TxId -> Maybe TxInId -> Maybe TxOutId -> Maybe MaTxOutId -> ReaderT SqlBackend m Word64
deleteTablesAfterTxId mtxId mtxInId mtxOutId mmaTxOutId = do
  txInDeleted <- fromIntegral <$> maybe (pure 0) (\txInId -> deleteWhereCount [TxInId >=. txInId]) mtxInId
  whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [MaTxOutId >=. maTxOutId]
  whenJust mtxOutId $ \txOutId -> deleteWhere [TxOutId >=. txOutId]

  whenJust mtxId $ \txId -> do
    queryFirstAndDeleteAfter CollateralTxOutTxId txId
    queryFirstAndDeleteAfter CollateralTxInTxInId txId
    queryFirstAndDeleteAfter ReferenceTxInTxInId txId
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
    queryFirstAndDeleteAfter VotingAnchorTxId txId
    queryFirstAndDeleteAfter DelegationVoteTxId txId
    queryFirstAndDeleteAfter CommitteeRegistrationTxId txId
    queryFirstAndDeleteAfter CommitteeDeRegistrationTxId txId
    queryFirstAndDeleteAfter DrepRegistrationTxId txId
    queryFirstAndDeleteAfter DrepDeRegistrationTxId txId
    queryFirstAndDeleteAfter VotingProcedureTxId txId
    mgaId <- queryMinRefId GovernanceActionTxId txId
    whenJust mgaId $ \gaId -> do
      queryFirstAndDeleteAfter TreasuryWithdrawalGovernanceActionId gaId
      queryFirstAndDeleteAfter NewCommitteeGovernanceActionId gaId
      deleteWhere [GovernanceActionId >=. gaId]
    mvaId <- queryMinRefId VotingAnchorTxId txId
    whenJust mvaId $ \vaId -> do
      queryFirstAndDeleteAfter AnchorOfflineDataVotingAnchorId vaId
      queryFirstAndDeleteAfter AnchorOfflineFetchErrorVotingAnchorId vaId
      deleteWhere [VotingAnchorId >=. vaId]
    minPmr <- queryMinRefId PoolMetadataRefRegisteredTxId txId
    whenJust minPmr $ \pmrId -> do
      queryFirstAndDeleteAfter PoolOfflineDataPmrId pmrId
      queryFirstAndDeleteAfter PoolOfflineFetchErrorPmrId pmrId
      deleteWhere [PoolMetadataRefId >=. pmrId]
    minPoolUpdate <- queryMinRefId PoolUpdateRegisteredTxId txId
    whenJust minPoolUpdate $ \puid -> do
      queryFirstAndDeleteAfter PoolOwnerPoolUpdateId puid
      queryFirstAndDeleteAfter PoolRelayUpdateId puid
      deleteWhere [PoolUpdateId >=. puid]
    deleteWhere [TxId >=. txId]
  pure txInDeleted

queryFirstAndDeleteAfter ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field, PersistEntityBackend record ~ SqlBackend) =>
  EntityField record field ->
  field ->
  ReaderT SqlBackend m ()
queryFirstAndDeleteAfter txIdField txId = do
  mRecordId <- queryMinRefId txIdField txId
  whenJust mRecordId $ \recordId ->
    deleteWhere [persistIdField @record >=. recordId]

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
deleteDelistedPool poolHash = do
  keys <- selectKeysList [DelistedPoolHashRaw ==. poolHash] []
  mapM_ delete keys
  pure $ not (null keys)

whenNothingQueryMinRefId ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field) =>
  Maybe (Key record) ->
  EntityField record field ->
  field ->
  ReaderT SqlBackend m (Maybe (Key record))
whenNothingQueryMinRefId mKey efield field = do
  case mKey of
    Just k -> pure $ Just k
    Nothing -> queryMinRefId efield field

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteBlock block = do
  mBlockId <- listToMaybe <$> selectKeysList [BlockHash ==. blockHash block] []
  case mBlockId of
    Nothing -> pure False
    Just blockId -> do
      void $ deleteBlocksBlockId nullTracer blockId
      pure True

deleteEpochRows :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
deleteEpochRows epochNum =
  deleteWhere [EpochNo >=. epochNum]

deleteAdaPots :: MonadIO m => BlockId -> ReaderT SqlBackend m ()
deleteAdaPots blkId = do
  deleteWhere [AdaPotsBlockId ==. blkId]
