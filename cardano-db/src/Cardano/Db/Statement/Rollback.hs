{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Rollback where

import Cardano.Prelude (Int64, MonadIO, Proxy (..), catMaybes, forM)
import qualified Data.Text as Text
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SCE
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SCG
import qualified Cardano.Db.Schema.Core.MultiAsset as SCM
import qualified Cardano.Db.Schema.Core.OffChain as SCO
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Core.StakeDeligation as SCS
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), MinIdsWrapper (..))
import qualified Cardano.Db.Schema.Variants as SV
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Db.Statement.Function.Core (mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Delete (deleteWhereCount)
import Cardano.Db.Statement.Function.Query (queryMinRefId)
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Types (DbAction)

-- This creates a pipeline for multiple delete operations
runDeletePipeline ::
  forall m.
  MonadIO m =>
  -- | Operation name for logging
  Text.Text ->
  -- | List of (table name, delete session)
  [(Text.Text, HsqlSes.Session Int64)] ->
  DbAction m [(Text.Text, Int64)]
runDeletePipeline opName operations = do
  runDbSession (mkCallInfo opName) $ do
    forM operations $ \(tName, deleteSession) -> do
      count <- deleteSession
      pure (tName, count)

-- Function to create a delete session without immediately running it
prepareDelete ::
  forall a b.
  (DbInfo a) =>
  -- | Field name
  Text.Text ->
  -- | Value
  b ->
  -- | Operator
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Returns table name and session
  (Text.Text, HsqlSes.Session Int64)
prepareDelete fieldName value operator encoder =
  let tName = tableName (Proxy @a)
      deleteSession =
        HsqlSes.statement value $
          deleteWhereCount @a fieldName operator encoder
   in (tName, deleteSession)

deleteTablesAfterBlockId ::
  forall m.
  MonadIO m =>
  SV.TxOutVariantType ->
  Id.BlockId ->
  Maybe Id.TxId ->
  MinIdsWrapper ->
  DbAction m (Int64, [(Text.Text, Int64)])
deleteTablesAfterBlockId txOutVariantType blkId mtxId minIdsW = do
  let blockIdEncoder = Id.idEncoder Id.getBlockId

  -- Create a pipeline for initial deletions
  initialLogs <-
    runDeletePipeline
      "initialDelete"
      [ prepareDelete @SCE.AdaPots "block_id" blkId ">=" blockIdEncoder
      , prepareDelete @SCB.ReverseIndex "block_id" blkId ">=" blockIdEncoder
      , prepareDelete @SCE.EpochParam "block_id" blkId ">=" blockIdEncoder
      ]

  -- Handle off-chain related deletions
  mvaId <-
    queryMinRefId @SCG.VotingAnchor
      "block_id"
      blkId
      blockIdEncoder
      (Id.idDecoder Id.VotingAnchorId)

  offChainLogs <- case mvaId of
    Nothing -> pure []
    Just vaId -> do
      -- For VotingAnchorId, we need the correct encoder
      let vaIdEncoder = Id.idEncoder Id.getVotingAnchorId

      mocvdId <-
        queryMinRefId @SCO.OffChainVoteData
          "voting_anchor_id"
          vaId
          vaIdEncoder
          (Id.idDecoder Id.OffChainVoteDataId)

      logsVoting <- case mocvdId of
        Nothing -> pure []
        Just ocvdId -> do
          -- For OffChainVoteDataId, we need the correct encoder
          let ocvdIdEncoder = Id.idEncoder Id.getOffChainVoteDataId
              offChainVoteDataId = "off_chain_vote_data_id"

          runDeletePipeline
            "voteDataDelete"
            [ prepareDelete @SCO.OffChainVoteGovActionData offChainVoteDataId ocvdId ">=" ocvdIdEncoder
            , prepareDelete @SCO.OffChainVoteDrepData offChainVoteDataId ocvdId ">=" ocvdIdEncoder
            , prepareDelete @SCO.OffChainVoteAuthor offChainVoteDataId ocvdId ">=" ocvdIdEncoder
            , prepareDelete @SCO.OffChainVoteReference offChainVoteDataId ocvdId ">=" ocvdIdEncoder
            , prepareDelete @SCO.OffChainVoteExternalUpdate offChainVoteDataId ocvdId ">=" ocvdIdEncoder
            ]

      offChain <-
        runDeletePipeline
          "anchorDelete"
          [ prepareDelete @SCO.OffChainVoteData "voting_anchor_id" vaId ">=" vaIdEncoder
          , prepareDelete @SCO.OffChainVoteFetchError "voting_anchor_id" vaId ">=" vaIdEncoder
          , prepareDelete @SCG.VotingAnchor "id" vaId ">=" vaIdEncoder
          ]
      pure $ logsVoting <> offChain
  -- Additional deletions based on TxId and minimum IDs
  afterTxIdLogs <- deleteTablesAfterTxId txOutVariantType mtxId minIdsW
  -- Final block deletions
  blockLogs <-
    runDeletePipeline
      "blockDelete"
      [prepareDelete @SCB.Block "id" blkId ">=" blockIdEncoder]
  -- Aggregate and return all logs
  pure (sum $ map snd blockLogs, initialLogs <> blockLogs <> offChainLogs <> afterTxIdLogs)

deleteTablesAfterTxId ::
  forall m.
  MonadIO m =>
  SV.TxOutVariantType ->
  Maybe Id.TxId ->
  MinIdsWrapper ->
  DbAction m [(Text.Text, Int64)]
deleteTablesAfterTxId txOutVariantType mtxId minIdsW = do
  let txIdEncoder = Id.idEncoder Id.getTxId

  -- Handle deletions and log accumulation from MinIdsWrapper
  minIdsLogs <- case minIdsW of
    CMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) -> do
      let operations =
            catMaybes
              [ fmap (\txInId -> prepareOnlyDelete @SCB.TxIn "id" txInId ">=" (Id.idEncoder Id.getTxInId)) mtxInId
              , prepareTypedDelete @VC.TxOutCore "id" mtxOutId SV.unwrapTxOutIdCore (Id.idEncoder Id.getTxOutCoreId)
              , prepareTypedDelete @VC.MaTxOutCore "id" mmaTxOutId SV.unwrapMaTxOutIdCore (Id.idEncoder Id.getMaTxOutCoreId)
              ]
      if null operations
        then pure []
        else runDeletePipeline "cMinIdsDelete" operations
    VMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) -> do
      let operations =
            catMaybes
              [ fmap (\txInId -> prepareOnlyDelete @SCB.TxIn "id" txInId ">=" (Id.idEncoder Id.getTxInId)) mtxInId
              , prepareTypedDelete @VA.TxOutAddress "id" mtxOutId SV.unwrapTxOutIdAddress (Id.idEncoder Id.getTxOutAddressId)
              , prepareTypedDelete @VA.MaTxOutAddress "id" mmaTxOutId SV.unwrapMaTxOutIdAddress (Id.idEncoder Id.getMaTxOutAddressId)
              ]
      if null operations
        then pure []
        else runDeletePipeline "vMinIdsDelete" operations

  -- Handle deletions and log accumulation using the specified TxId
  txIdLogs <- case mtxId of
    Nothing -> pure [] -- If no TxId is provided, skip further deletions
    Just txId -> do
      -- Create a pipeline for transaction-related deletions
      result <-
        runDeletePipeline
          "txRelatedDelete"
          [ case txOutVariantType of
              SV.TxOutVariantCore -> prepareDelete @VC.CollateralTxOutCore "tx_id" txId ">=" txIdEncoder
              SV.TxOutVariantAddress -> prepareDelete @VA.CollateralTxOutAddress "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.CollateralTxIn "tx_in_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.ReferenceTxIn "tx_in_id" txId ">=" txIdEncoder
          , prepareDelete @SCP.PoolRetire "announced_tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCS.StakeRegistration "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCS.StakeDeregistration "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCS.Delegation "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.TxMetadata "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.Withdrawal "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCE.Treasury "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCE.Reserve "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCE.PotTransfer "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCM.MaTxMint "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.Redeemer "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.Script "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.Datum "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.RedeemerData "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.ExtraKeyWitness "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCB.TxCbor "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCG.ParamProposal "registered_tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCG.DelegationVote "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCG.CommitteeRegistration "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCG.CommitteeDeRegistration "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCG.DrepRegistration "tx_id" txId ">=" txIdEncoder
          , prepareDelete @SCG.VotingProcedure "tx_id" txId ">=" txIdEncoder
          ]

      -- Handle GovActionProposal related deletions if present
      mgaId <- queryMinRefId @SCG.GovActionProposal "tx_id" txId txIdEncoder (Id.idDecoder Id.GovActionProposalId)
      gaLogs <- case mgaId of
        Nothing -> pure [] -- No GovActionProposal ID found, skip this step
        Just gaId -> do
          let gaIdEncoder = Id.idEncoder Id.getGovActionProposalId
          runDeletePipeline
            "govActionDelete"
            [ prepareDelete @SCG.TreasuryWithdrawal "gov_action_proposal_id" gaId ">=" gaIdEncoder
            , prepareDelete @SCG.Committee "gov_action_proposal_id" gaId ">=" gaIdEncoder
            , prepareDelete @SCG.Constitution "gov_action_proposal_id" gaId ">=" gaIdEncoder
            , prepareDelete @SCG.GovActionProposal "id" gaId ">=" gaIdEncoder
            ]

      -- Handle PoolMetadataRef related deletions if present
      minPmr <- queryMinRefId @SCP.PoolMetadataRef "registered_tx_id" txId txIdEncoder (Id.idDecoder Id.PoolMetadataRefId)
      pmrLogs <- case minPmr of
        Nothing -> pure [] -- No PoolMetadataRef ID found, skip this step
        Just pmrId -> do
          let pmrIdEncoder = Id.idEncoder Id.getPoolMetadataRefId
          runDeletePipeline
            "poolMetadataRefDelete"
            [ prepareDelete @SCO.OffChainPoolData "pmr_id" pmrId ">=" pmrIdEncoder
            , prepareDelete @SCO.OffChainPoolFetchError "pmr_id" pmrId ">=" pmrIdEncoder
            , prepareDelete @SCP.PoolMetadataRef "id" pmrId ">=" pmrIdEncoder
            ]

      -- Handle PoolUpdate related deletions if present
      minPoolUpdate <- queryMinRefId @SCP.PoolUpdate "registered_tx_id" txId txIdEncoder (Id.idDecoder Id.PoolUpdateId)
      poolUpdateLogs <- case minPoolUpdate of
        Nothing -> pure [] -- No PoolUpdate ID found, skip this step
        Just puid -> do
          let puidEncoder = Id.idEncoder Id.getPoolUpdateId
          runDeletePipeline
            "poolUpdateDelete"
            [ prepareDelete @SCP.PoolOwner "pool_update_id" puid ">=" puidEncoder
            , prepareDelete @SCP.PoolRelay "update_id" puid ">=" puidEncoder
            , prepareDelete @SCP.PoolUpdate "id" puid ">=" puidEncoder
            ]
      -- Final deletions for the given TxId
      txLogs <- runDeletePipeline "" [prepareOnlyDelete @SCB.Tx "id" txId ">=" txIdEncoder]
      -- Combine all logs from the operations above
      pure $ result <> gaLogs <> pmrLogs <> poolUpdateLogs <> txLogs
  -- Return the combined logs of all operations
  pure $ minIdsLogs <> txIdLogs

-- Creates a delete statement that returns count
onlyDeleteStmt ::
  forall a b.
  (DbInfo a) =>
  -- | Field name
  Text.Text ->
  -- | Operator
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  HsqlStmt.Statement b Int64
onlyDeleteStmt = deleteWhereCount @a

-- Prepares a delete operation for pipeline
prepareOnlyDelete ::
  forall a b.
  (DbInfo a) =>
  -- | Field name
  Text.Text ->
  -- | Value
  b ->
  -- | Operator
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Returns table name and session
  (Text.Text, HsqlSes.Session Int64)
prepareOnlyDelete fieldName value operator encoder =
  let tName = tableName (Proxy @a)
      deleteSession = HsqlSes.statement value $ onlyDeleteStmt @a fieldName operator encoder
   in (tName, deleteSession)

-- Helper for creating delete operations with proper unwrapping
prepareTypedDelete ::
  forall a b w.
  (DbInfo a) =>
  Text.Text -> -- Field name
  Maybe w -> -- Wrapped ID (Maybe)
  (w -> Maybe b) -> -- Unwrapper function
  HsqlE.Params b -> -- Parameter encoder (already applied)
  Maybe (Text.Text, HsqlSes.Session Int64)
prepareTypedDelete fieldName mWrappedId unwrapper encoder =
  case mWrappedId of
    Nothing -> Nothing
    Just wrappedId ->
      case unwrapper wrappedId of
        Nothing -> Nothing
        Just i -> Just (prepareOnlyDelete @a fieldName i ">=" encoder)
