{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Db.Operations.Core.Delete (
  deleteBlocksSlotNo,
  deleteBlocksSlotNoNoTrace,
  deleteDelistedPool,
  deleteBlocksBlockId,
  deleteBlocksBlockIdNotrace,
  deleteBlock,
  deleteEpochRows,
  deleteDrepDistr,
  deleteRewardRest,
  deletePoolStat,
  deleteAdaPots,
  -- for testing
  queryFirstAndDeleteAfter,
) where

import Cardano.BM.Trace (Trace, logWarning, nullTracer)
import Cardano.Db.Operations.Core.MinId (MinIds (..), MinIdsWrapper (..), completeMinId, textToMinIds)
import Cardano.Db.Operations.Core.Query
import Cardano.Db.Operations.Types (TxOutTableType (..))
import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Prelude (Int64)
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
  (!=.),
  (==.),
  (>.),
  (>=.),
 )

deleteBlocksSlotNoNoTrace :: MonadIO m => TxOutTableType -> SlotNo -> ReaderT SqlBackend m Bool
deleteBlocksSlotNoNoTrace = deleteBlocksSlotNo nullTracer

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlocksSlotNo :: MonadIO m => Trace IO Text -> TxOutTableType -> SlotNo -> ReaderT SqlBackend m Bool
deleteBlocksSlotNo trce txOutTableType (SlotNo slotNo) = do
  mBlockId <- queryBlockSlotNo slotNo
  case mBlockId of
    Nothing -> pure False
    Just blockId -> do
      void $ deleteBlocksBlockId trce txOutTableType blockId
      pure True

deleteBlocksBlockIdNotrace :: MonadIO m => TxOutTableType -> BlockId -> ReaderT SqlBackend m ()
deleteBlocksBlockIdNotrace txOutTableType = void . deleteBlocksBlockId nullTracer txOutTableType

-- | Delete starting from a 'BlockId'.
deleteBlocksBlockId :: MonadIO m => Trace IO Text -> TxOutTableType -> BlockId -> ReaderT SqlBackend m (Maybe TxId, Int64)
deleteBlocksBlockId trce txOutTableType blockId = do
  mMinIds <- fmap (textToMinIds txOutTableType =<<) <$> queryReverseIndexBlockId blockId
  (cminIds, completed) <- findMinIdsRec mMinIds mempty
  mTxId <- queryMinRefId TxBlockId blockId
  minIds <- if completed then pure cminIds else completeMinId mTxId cminIds
  blockCountInt <- deleteTablesAfterBlockId blockId mTxId minIds
  pure (mTxId, blockCountInt)
  where
    findMinIdsRec :: MonadIO m => [Maybe MinIdsWrapper] -> MinIdsWrapper -> ReaderT SqlBackend m (MinIdsWrapper, Bool)
    findMinIdsRec [] minIds = pure (minIds, True)
    findMinIdsRec (mMinIds : rest) minIds =
      case mMinIds of
        Nothing -> do
          liftIO $
            logWarning
              trce
              "Failed to find ReverseIndex. Deletion may take longer."
          pure (minIds, False)
        Just minIdDB -> do
          let minIds' = minIds <> minIdDB
          if isComplete minIds'
            then pure (minIds', True)
            else findMinIdsRec rest minIds'

    isComplete minIdsW = case minIdsW of
      CMinIdsWrapper (MinIds m1 m2 m3) -> isJust m1 && isJust m2 && isJust m3
      VMinIdsWrapper (MinIds m1 m2 m3) -> isJust m1 && isJust m2 && isJust m3

-- (MinIds m1 m2 m3) isJust m1 && isJust m2 && isJust m3

deleteTablesAfterBlockId :: MonadIO m => BlockId -> Maybe TxId -> MinIdsWrapper -> ReaderT SqlBackend m Int64
deleteTablesAfterBlockId blkId mtxId minIdsW = do
  deleteWhere [AdaPotsBlockId >=. blkId]
  deleteWhere [ReverseIndexBlockId >=. blkId]
  deleteWhere [EpochParamBlockId >=. blkId]
  mvaId <- queryMinRefId VotingAnchorBlockId blkId
  whenJust mvaId $ \vaId -> do
    mocvdId <- queryMinRefId OffChainVoteDataVotingAnchorId vaId
    whenJust mocvdId $ \ocvdId -> do
      queryFirstAndDeleteAfter OffChainVoteGovActionDataOffChainVoteDataId ocvdId
      queryFirstAndDeleteAfter OffChainVoteDrepDataOffChainVoteDataId ocvdId
      queryFirstAndDeleteAfter OffChainVoteAuthorOffChainVoteDataId ocvdId
      queryFirstAndDeleteAfter OffChainVoteReferenceOffChainVoteDataId ocvdId
      queryFirstAndDeleteAfter OffChainVoteExternalUpdateOffChainVoteDataId ocvdId
      deleteWhere [OffChainVoteDataId >=. ocvdId]
    queryFirstAndDeleteAfter OffChainVoteDataVotingAnchorId vaId
    queryFirstAndDeleteAfter OffChainVoteFetchErrorVotingAnchorId vaId
    deleteWhere [VotingAnchorId >=. vaId]
  deleteTablesAfterTxId mtxId minIdsW
  deleteWhereCount [BlockId >=. blkId]

deleteTablesAfterTxId :: (MonadIO m) => Maybe TxId -> MinIdsWrapper -> ReaderT SqlBackend m ()
deleteTablesAfterTxId mtxId minIdsW = do
  case minIdsW of
    CMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) -> do
      whenJust mtxInId $ \txInId -> deleteWhere [TxInId >=. txInId]
      whenJust mtxOutId $ \txOutId -> deleteWhere [C.TxOutId >=. txOutId]
      whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [C.MaTxOutId >=. maTxOutId]
    VMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) -> do
      whenJust mtxInId $ \txInId -> deleteWhere [TxInId >=. txInId]
      whenJust mtxOutId $ \txOutId -> deleteWhere [V.TxOutId >=. txOutId]
      whenJust mmaTxOutId $ \maTxOutId -> deleteWhere [V.MaTxOutId >=. maTxOutId]

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
    queryFirstAndDeleteAfter TxCborTxId txId
    queryFirstAndDeleteAfter ParamProposalRegisteredTxId txId
    queryFirstAndDeleteAfter DelegationVoteTxId txId
    queryFirstAndDeleteAfter CommitteeRegistrationTxId txId
    queryFirstAndDeleteAfter CommitteeDeRegistrationTxId txId
    queryFirstAndDeleteAfter DrepRegistrationTxId txId
    queryFirstAndDeleteAfter VotingProcedureTxId txId
    mgaId <- queryMinRefId GovActionProposalTxId txId
    whenJust mgaId $ \gaId -> do
      queryFirstAndDeleteAfter TreasuryWithdrawalGovActionProposalId gaId
      queryFirstAndDeleteAfter' CommitteeGovActionProposalId gaId
      queryFirstAndDeleteAfter' ConstitutionGovActionProposalId gaId
      deleteWhere [GovActionProposalId >=. gaId]
    minPmr <- queryMinRefId PoolMetadataRefRegisteredTxId txId
    whenJust minPmr $ \pmrId -> do
      queryFirstAndDeleteAfter OffChainPoolDataPmrId pmrId
      queryFirstAndDeleteAfter OffChainPoolFetchErrorPmrId pmrId
      deleteWhere [PoolMetadataRefId >=. pmrId]
    minPoolUpdate <- queryMinRefId PoolUpdateRegisteredTxId txId
    whenJust minPoolUpdate $ \puid -> do
      queryFirstAndDeleteAfter PoolOwnerPoolUpdateId puid
      queryFirstAndDeleteAfter PoolRelayUpdateId puid
      deleteWhere [PoolUpdateId >=. puid]
    deleteWhere [TxId >=. txId]

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

queryFirstAndDeleteAfter' ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field, PersistEntityBackend record ~ SqlBackend) =>
  EntityField record (Maybe field) ->
  field ->
  ReaderT SqlBackend m ()
queryFirstAndDeleteAfter' txIdField txId = do
  mRecordId <- queryMinRefIdNullable txIdField txId
  whenJust mRecordId $ \recordId ->
    deleteWhere [persistIdField @record >=. recordId, txIdField !=. Nothing]

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
deleteDelistedPool poolHash = do
  keys <- selectKeysList [DelistedPoolHashRaw ==. poolHash] []
  mapM_ delete keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlock :: MonadIO m => TxOutTableType -> Block -> ReaderT SqlBackend m Bool
deleteBlock txOutTableType block = do
  mBlockId <- listToMaybe <$> selectKeysList [BlockHash ==. blockHash block] []
  case mBlockId of
    Nothing -> pure False
    Just blockId -> do
      void $ deleteBlocksBlockId nullTracer txOutTableType blockId
      pure True

deleteEpochRows :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
deleteEpochRows epochNum =
  deleteWhere [EpochNo >=. epochNum]

deleteDrepDistr :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
deleteDrepDistr epochNum =
  deleteWhere [DrepDistrEpochNo >. epochNum]

deleteRewardRest :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
deleteRewardRest epochNum =
  deleteWhere [RewardRestSpendableEpoch >. epochNum]

deletePoolStat :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
deletePoolStat epochNum = do
  deleteWhere [PoolStatEpochNo >. epochNum]

deleteAdaPots :: MonadIO m => BlockId -> ReaderT SqlBackend m ()
deleteAdaPots blkId = do
  deleteWhere [AdaPotsBlockId ==. blkId]
