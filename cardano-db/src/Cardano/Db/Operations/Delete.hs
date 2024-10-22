{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Db.Operations.Delete (
  deleteBlocksSlotNo,
  deleteBlocksSlotNoNoTrace,
  deleteDelistedPool,
  deleteBlocksBlockId,
  deleteBlocksForTests,
  deleteBlock,
  queryDelete,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning, nullTracer)
import Cardano.Db.Operations.Insert (
  setNullDropped,
  setNullEnacted,
  setNullExpired,
  setNullRatified,
 )
import Cardano.Db.Operations.Other.ConsumedTxOut (querySetNullTxOut)
import Cardano.Db.Operations.Other.MinId (MinIds (..), MinIdsWrapper (..), completeMinId, textToMinIds)
import Cardano.Db.Operations.Query
import Cardano.Db.Operations.Types (TxOutTableType (..))
import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Prelude (Int64)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Text (Text, intercalate, pack)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (persistIdField)
import Database.Persist (
  PersistEntity,
  PersistEntityBackend,
  PersistField,
  (!=.),
  (==.),
  (>.),
  (>=.),
 )
import Database.Persist.Sql (Filter, SqlBackend, delete, deleteWhere, deleteWhereCount, selectKeysList)

deleteBlocksSlotNoNoTrace :: MonadIO m => TxOutTableType -> SlotNo -> ReaderT SqlBackend m Bool
deleteBlocksSlotNoNoTrace txOutTableType slotNo = deleteBlocksSlotNo nullTracer txOutTableType slotNo Nothing

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlocksSlotNo ::
  MonadIO m =>
  Trace IO Text ->
  TxOutTableType ->
  SlotNo ->
  Maybe Bool ->
  ReaderT SqlBackend m Bool
deleteBlocksSlotNo trce txOutTableType (SlotNo slotNo) mIsConsumedTxOut = do
  mBlockId <- queryNearestBlockSlotNo slotNo
  case mBlockId of
    Nothing -> do
      liftIO $ logWarning trce $ "deleteBlocksSlotNo: No block contains the the slot: " <> pack (show slotNo)
      pure False
    Just (blockId, epochN) -> do
      void $ deleteBlocksBlockId trce txOutTableType blockId epochN mIsConsumedTxOut
      pure True

deleteBlocksForTests :: MonadIO m => TxOutTableType -> BlockId -> Word64 -> ReaderT SqlBackend m ()
deleteBlocksForTests txOutTableType blockId epochN = do
  void $ deleteBlocksBlockId nullTracer txOutTableType blockId epochN Nothing

-- | Delete starting from a 'BlockId'.
deleteBlocksBlockId ::
  MonadIO m =>
  Trace IO Text ->
  TxOutTableType ->
  BlockId ->
  -- | The 'EpochNo' of the block to delete.
  Word64 ->
  -- | Is ConsumeTxout
  Maybe Bool ->
  ReaderT SqlBackend m Int64
deleteBlocksBlockId trce txOutTableType blockId epochN mIsConsumedTxOut = do
  mMinIds <- fmap (textToMinIds txOutTableType =<<) <$> queryReverseIndexBlockId blockId
  (cminIds, completed) <- findMinIdsRec mMinIds mempty
  mTxId <- queryMinRefId TxBlockId blockId
  minIds <- if completed then pure cminIds else completeMinId mTxId cminIds
  deleteEpochLogs <- deleteUsingEpochNo epochN
  (deleteBlockCount, blockDeleteLogs) <- deleteTablesAfterBlockId txOutTableType blockId mTxId minIds
  setNullLogs <-
    maybe
      (pure ("ConsumedTxOut is not active so no Nulls set", 0))
      (\_ -> querySetNullTxOut txOutTableType mTxId)
      mIsConsumedTxOut
  -- log all the deleted rows in the rollback
  liftIO $ logInfo trce $ mkRollbackSummary (deleteEpochLogs <> blockDeleteLogs) setNullLogs
  pure deleteBlockCount
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

deleteUsingEpochNo :: MonadIO m => Word64 -> ReaderT SqlBackend m [(Text, Int64)]
deleteUsingEpochNo epochN = do
  countLogs <-
    concat
      <$> sequence
        [ onlyDelete "Epoch" [EpochNo ==. epochN]
        , onlyDelete "DrepDistr" [DrepDistrEpochNo >. epochN]
        , onlyDelete "RewardRest" [RewardRestSpendableEpoch >. epochN]
        , onlyDelete "PoolStat" [PoolStatEpochNo >. epochN]
        ]
  nullLogs <- do
    a <- setNullEnacted epochN
    b <- setNullRatified epochN
    c <- setNullDropped epochN
    e <- setNullExpired epochN
    pure [("GovActionProposal Nulled", a + b + c + e)]
  pure $ countLogs <> nullLogs

deleteTablesAfterBlockId ::
  MonadIO m =>
  TxOutTableType ->
  BlockId ->
  Maybe TxId ->
  MinIdsWrapper ->
  ReaderT SqlBackend m (Int64, [(Text, Int64)])
deleteTablesAfterBlockId txOutTableType blkId mtxId minIdsW = do
  initialLogs <-
    concat
      <$> sequence
        [ onlyDelete "AdaPots" [AdaPotsBlockId >=. blkId]
        , onlyDelete "ReverseIndex" [ReverseIndexBlockId >=. blkId]
        , onlyDelete "EpochParam" [EpochParamBlockId >=. blkId]
        ]

  -- Handle off-chain related deletions
  mvaId <- queryMinRefId VotingAnchorBlockId blkId
  offChainLogs <- case mvaId of
    Nothing -> pure []
    Just vaId -> do
      mocvdId <- queryMinRefId OffChainVoteDataVotingAnchorId vaId
      logsVoting <- case mocvdId of
        Nothing -> pure []
        Just ocvdId ->
          concat
            <$> sequence
              [ queryDeleteAndLog "OffChainVoteGovActionData" OffChainVoteGovActionDataOffChainVoteDataId ocvdId
              , queryDeleteAndLog "OffChainVoteDrepData" OffChainVoteDrepDataOffChainVoteDataId ocvdId
              , queryDeleteAndLog "OffChainVoteAuthor" OffChainVoteAuthorOffChainVoteDataId ocvdId
              , queryDeleteAndLog "OffChainVoteReference" OffChainVoteReferenceOffChainVoteDataId ocvdId
              , queryDeleteAndLog "OffChainVoteExternalUpdate" OffChainVoteExternalUpdateOffChainVoteDataId ocvdId
              ]

      offChain <-
        concat
          <$> sequence
            [ queryDeleteAndLog "OffChainVoteData" OffChainVoteDataVotingAnchorId vaId
            , queryDeleteAndLog "OffChainVoteFetchError" OffChainVoteFetchErrorVotingAnchorId vaId
            , onlyDelete "VotingAnchor" [VotingAnchorId >=. vaId]
            ]
      pure $ logsVoting <> offChain
  -- Additional deletions based on TxId and minimum IDs
  afterTxIdLogs <- deleteTablesAfterTxId txOutTableType mtxId minIdsW
  -- Final block deletions
  blockLogs <- onlyDelete "Block" [BlockId >=. blkId]
  -- Aggregate and return all logs
  pure (sum $ map snd blockLogs, initialLogs <> blockLogs <> offChainLogs <> afterTxIdLogs)

deleteTablesAfterTxId ::
  (MonadIO m) =>
  TxOutTableType ->
  Maybe TxId ->
  MinIdsWrapper ->
  ReaderT SqlBackend m [(Text, Int64)]
deleteTablesAfterTxId txOutTableType mtxId minIdsW = do
  -- Handle deletions and log accumulation from MinIdsWrapper
  minIdsLogs <- case minIdsW of
    CMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) ->
      concat
        <$> sequence
          [ maybe (pure []) (\txInId -> onlyDelete "TxIn" [TxInId >=. txInId]) mtxInId
          , maybe (pure []) (\txOutId -> onlyDelete "TxOut" [C.TxOutId >=. txOutId]) mtxOutId
          , maybe (pure []) (\maTxOutId -> onlyDelete "MaTxOut" [C.MaTxOutId >=. maTxOutId]) mmaTxOutId
          ]
    VMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) ->
      concat
        <$> sequence
          [ maybe (pure []) (\txInId -> onlyDelete "TxIn" [TxInId >=. txInId]) mtxInId
          , maybe (pure []) (\txOutId -> onlyDelete "TxOut" [V.TxOutId >=. txOutId]) mtxOutId
          , maybe (pure []) (\maTxOutId -> onlyDelete "MaTxOut" [V.MaTxOutId >=. maTxOutId]) mmaTxOutId
          ]
  -- Handle deletions and log accumulation using the specified TxId
  txIdLogs <- case mtxId of
    Nothing -> pure [] -- If no TxId is provided, skip further deletions
    Just txId -> do
      result <-
        -- Sequentially delete records with associated transaction ID
        concat
          <$> sequence
            [ case txOutTableType of
                TxOutCore -> queryDeleteAndLog "CollateralTxOut" C.CollateralTxOutTxId txId
                TxOutVariantAddress -> queryDeleteAndLog "CollateralTxOut" V.CollateralTxOutTxId txId
            , queryDeleteAndLog "CollateralTxIn" CollateralTxInTxInId txId
            , queryDeleteAndLog "ReferenceTxIn" ReferenceTxInTxInId txId
            , queryDeleteAndLog "PoolRetire" PoolRetireAnnouncedTxId txId
            , queryDeleteAndLog "StakeRegistration" StakeRegistrationTxId txId
            , queryDeleteAndLog "StakeDeregistration" StakeDeregistrationTxId txId
            , queryDeleteAndLog "Delegation" DelegationTxId txId
            , queryDeleteAndLog "TxMetadata" TxMetadataTxId txId
            , queryDeleteAndLog "Withdrawal" WithdrawalTxId txId
            , queryDeleteAndLog "Treasury" TreasuryTxId txId
            , queryDeleteAndLog "Reserve" ReserveTxId txId
            , queryDeleteAndLog "PotTransfer" PotTransferTxId txId
            , queryDeleteAndLog "MaTxMint" MaTxMintTxId txId
            , queryDeleteAndLog "Redeemer" RedeemerTxId txId
            , queryDeleteAndLog "Script" ScriptTxId txId
            , queryDeleteAndLog "Datum" DatumTxId txId
            , queryDeleteAndLog "RedeemerData" RedeemerDataTxId txId
            , queryDeleteAndLog "ExtraKeyWitness" ExtraKeyWitnessTxId txId
            , queryDeleteAndLog "TxCbor" TxCborTxId txId
            , queryDeleteAndLog "ParamProposal" ParamProposalRegisteredTxId txId
            , queryDeleteAndLog "DelegationVote" DelegationVoteTxId txId
            , queryDeleteAndLog "CommitteeRegistration" CommitteeRegistrationTxId txId
            , queryDeleteAndLog "CommitteeDeRegistration" CommitteeDeRegistrationTxId txId
            , queryDeleteAndLog "DrepRegistration" DrepRegistrationTxId txId
            , queryDeleteAndLog "VotingProcedure" VotingProcedureTxId txId
            ]
      -- Handle GovActionProposal related deletions if present
      mgaId <- queryMinRefId GovActionProposalTxId txId
      gaLogs <- case mgaId of
        Nothing -> pure [] -- No GovActionProposal ID found, skip this step
        Just gaId ->
          -- Delete records related to the GovActionProposal ID
          concat
            <$> sequence
              [ queryDeleteAndLog "TreasuryWithdrawal" TreasuryWithdrawalGovActionProposalId gaId
              , queryThenNull "Committee" CommitteeGovActionProposalId gaId
              , queryThenNull "Constitution" ConstitutionGovActionProposalId gaId
              , onlyDelete "GovActionProposal" [GovActionProposalId >=. gaId]
              ]
      -- Handle PoolMetadataRef related deletions if present
      minPmr <- queryMinRefId PoolMetadataRefRegisteredTxId txId
      pmrLogs <- case minPmr of
        Nothing -> pure [] -- No PoolMetadataRef ID found, skip this step
        Just pmrId ->
          -- Delete records related to PoolMetadataRef
          concat
            <$> sequence
              [ queryDeleteAndLog "OffChainPoolData" OffChainPoolDataPmrId pmrId
              , queryDeleteAndLog "OffChainPoolFetchError" OffChainPoolFetchErrorPmrId pmrId
              , onlyDelete "PoolMetadataRef" [PoolMetadataRefId >=. pmrId]
              ]
      -- Handle PoolUpdate related deletions if present
      minPoolUpdate <- queryMinRefId PoolUpdateRegisteredTxId txId
      poolUpdateLogs <- case minPoolUpdate of
        Nothing -> pure [] -- No PoolUpdate ID found, skip this step
        Just puid -> do
          -- Delete records related to PoolUpdate
          concat
            <$> sequence
              [ queryDeleteAndLog "PoolOwner" PoolOwnerPoolUpdateId puid
              , queryDeleteAndLog "PoolRelay" PoolRelayUpdateId puid
              , onlyDelete "PoolUpdate" [PoolUpdateId >=. puid]
              ]
      -- Final deletions for the given TxId
      txLogs <- onlyDelete "Tx" [TxId >=. txId]
      -- Combine all logs from the operations above
      pure $ result <> gaLogs <> pmrLogs <> poolUpdateLogs <> txLogs
  -- Return the combined logs of all operations
  pure $ minIdsLogs <> txIdLogs

queryDelete ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field, PersistEntityBackend record ~ SqlBackend) =>
  EntityField record field ->
  field ->
  ReaderT SqlBackend m ()
queryDelete fieldIdField fieldId = do
  mRecordId <- queryMinRefId fieldIdField fieldId
  case mRecordId of
    Nothing -> pure ()
    Just recordId -> deleteWhere [persistIdField @record >=. recordId]

queryDeleteAndLog ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field, PersistEntityBackend record ~ SqlBackend) =>
  Text ->
  EntityField record field ->
  field ->
  ReaderT SqlBackend m [(Text, Int64)]
queryDeleteAndLog tableName txIdField fieldId = do
  mRecordId <- queryMinRefId txIdField fieldId
  case mRecordId of
    Nothing -> pure [(tableName, 0)]
    Just recordId -> do
      count <- deleteWhereCount [persistIdField @record >=. recordId]
      pure [(tableName, count)]

onlyDelete ::
  forall m record.
  (MonadIO m, PersistEntity record, PersistEntityBackend record ~ SqlBackend) =>
  Text ->
  [Filter record] ->
  ReaderT SqlBackend m [(Text, Int64)]
onlyDelete tableName filters = do
  count <- deleteWhereCount filters
  pure [(tableName, count)]

queryThenNull ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field, PersistEntityBackend record ~ SqlBackend) =>
  Text ->
  EntityField record (Maybe field) ->
  field ->
  ReaderT SqlBackend m [(Text, Int64)]
queryThenNull tableName txIdField txId = do
  mRecordId <- queryMinRefIdNullable txIdField txId
  case mRecordId of
    Nothing -> pure [(tableName, 0)]
    Just recordId -> do
      count <- deleteWhereCount [persistIdField @record >=. recordId, txIdField !=. Nothing]
      pure [(tableName, count)]

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
  mBlockId <- queryBlockHash block
  case mBlockId of
    Nothing -> pure False
    Just (blockId, epochN) -> do
      void $ deleteBlocksBlockId nullTracer txOutTableType blockId epochN Nothing
      pure True

mkRollbackSummary :: [(Text, Int64)] -> (Text, Int64) -> Text
mkRollbackSummary logs setNullLogs =
  "\n----------------------- Rollback Summary: ----------------------- \n"
    <> formattedLog
    <> zeroDeletedEntry
    <> formatSetNullLog setNullLogs
    <> "\n"
  where
    (zeroDeletes, nonZeroDeletes) = partition ((== 0) . snd) logs

    formattedLog = intercalate "\n" (map formatEntry nonZeroDeletes)

    zeroDeletedEntry
      | null zeroDeletes = ""
      | otherwise = "\n\nNo Deletes in tables: " <> intercalate ", " (map fst zeroDeletes)

    formatEntry (tableName, rowCount) =
      "Table: " <> tableName <> " - Count: " <> pack (show rowCount)

    formatSetNullLog (nullMessage, nullCount) =
      "\n\nSet Null: "
        <> if nullCount == 0
          then nullMessage
          else "\n\nSet Null: " <> nullMessage <> " - Count: " <> pack (show nullCount)
