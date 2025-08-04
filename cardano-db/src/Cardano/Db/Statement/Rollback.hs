{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Rollback where

import Cardano.Prelude (Int64, Proxy (..), catMaybes, forM)
import qualified Data.Text as Text
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

-- Import from MinIds

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SCE
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SCG
import qualified Cardano.Db.Schema.Core.MultiAsset as SCM
import qualified Cardano.Db.Schema.Core.OffChain as SCO
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Core.StakeDelegation as SCS
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), MinIdsWrapper (..))
import qualified Cardano.Db.Schema.Variants as SV
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Db.Statement.Function.Core (runSession)
import Cardano.Db.Statement.Function.Delete (deleteWhereCount, deleteWhereCountWithNotNull)
import Cardano.Db.Statement.MinIds (queryMinRefId, queryMinRefIdNullable)
import Cardano.Db.Statement.Types (DbInfo (..), tableName)
import Cardano.Db.Types (DbM)

-- Function to create a delete session without immediately running it
prepareDelete ::
  forall a b.
  DbInfo a =>
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

-- Creates a delete statement that returns count
onlyDeleteStmt ::
  forall a b.
  DbInfo a =>
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
  DbInfo a =>
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
  DbInfo a =>
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

-----------------------------------------------------------------------------------------------------------------

deleteTablesAfterBlockId ::
  SV.TxOutVariantType ->
  Id.BlockId ->
  Maybe Id.TxId ->
  MinIdsWrapper ->
  DbM (Int64, [(Text.Text, Int64)])
deleteTablesAfterBlockId txOutVariantType blkId mtxId minIdsW = do
  let blockIdEncoder = Id.idEncoder Id.getBlockId
  -- Execute initial deletions in parallel using pipeline
  let adaPotsStmt = deleteWhereCount @SCE.AdaPots "block_id" ">=" blockIdEncoder
      reverseIndexStmt = deleteWhereCount @SCB.ReverseIndex "block_id" ">=" blockIdEncoder
      epochParamStmt = deleteWhereCount @SCE.EpochParam "block_id" ">=" blockIdEncoder

  (adaPotsCount, reverseIndexCount, epochParamCount) <- runSession $ HsqlSes.pipeline $ do
    ada <- HsqlP.statement blkId adaPotsStmt
    rev <- HsqlP.statement blkId reverseIndexStmt
    epoch <- HsqlP.statement blkId epochParamStmt
    pure (ada, rev, epoch)

  let initialLogs =
        [ (tableName (Proxy @SCE.AdaPots), adaPotsCount)
        , (tableName (Proxy @SCB.ReverseIndex), reverseIndexCount)
        , (tableName (Proxy @SCE.EpochParam), epochParamCount)
        ]

  -- Handle off-chain related deletions
  mvaId <-
    queryMinRefId @SCG.VotingAnchor
      "block_id"
      blkId
      blockIdEncoder

  offChainLogs <- case mvaId of
    Nothing -> pure []
    Just vaId -> do
      -- vaId is now raw Int64, so create encoder for Int64
      let vaIdEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)

      mocvdId <-
        queryMinRefId @SCO.OffChainVoteData
          "voting_anchor_id"
          vaId
          vaIdEncoder

      logsVoting <- case mocvdId of
        Nothing -> pure []
        Just ocvdId -> do
          -- ocvdId is raw Int64, so create encoder for Int64
          let ocvdIdEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
              offChainVoteDataId = "off_chain_vote_data_id"
              govActionStmt = deleteWhereCount @SCO.OffChainVoteGovActionData offChainVoteDataId ">=" ocvdIdEncoder
              drepDataStmt = deleteWhereCount @SCO.OffChainVoteDrepData offChainVoteDataId ">=" ocvdIdEncoder
              authorStmt = deleteWhereCount @SCO.OffChainVoteAuthor offChainVoteDataId ">=" ocvdIdEncoder
              referenceStmt = deleteWhereCount @SCO.OffChainVoteReference offChainVoteDataId ">=" ocvdIdEncoder
              extUpdateStmt = deleteWhereCount @SCO.OffChainVoteExternalUpdate offChainVoteDataId ">=" ocvdIdEncoder

          (govCount, drepCount, authorCount, refCount, extCount) <- runSession $ HsqlSes.pipeline $ do
            gov <- HsqlP.statement ocvdId govActionStmt
            drep <- HsqlP.statement ocvdId drepDataStmt
            auth <- HsqlP.statement ocvdId authorStmt
            ref <- HsqlP.statement ocvdId referenceStmt
            ext <- HsqlP.statement ocvdId extUpdateStmt
            pure (gov, drep, auth, ref, ext)

          pure
            [ (tableName (Proxy @SCO.OffChainVoteGovActionData), govCount)
            , (tableName (Proxy @SCO.OffChainVoteDrepData), drepCount)
            , (tableName (Proxy @SCO.OffChainVoteAuthor), authorCount)
            , (tableName (Proxy @SCO.OffChainVoteReference), refCount)
            , (tableName (Proxy @SCO.OffChainVoteExternalUpdate), extCount)
            ]

      -- Execute anchor deletions sequentially (after vote data is deleted)
      let anchorDeleteOps =
            [ prepareDelete @SCO.OffChainVoteData "voting_anchor_id" vaId ">=" vaIdEncoder
            , prepareDelete @SCO.OffChainVoteFetchError "voting_anchor_id" vaId ">=" vaIdEncoder
            , prepareDelete @SCG.VotingAnchor "id" vaId ">=" vaIdEncoder
            ]
      offChain <- forM anchorDeleteOps $ \(tableN, deleteSession) -> do
        count <- runSession deleteSession
        pure (tableN, count)

      pure $ logsVoting <> offChain

  -- Additional deletions based on TxId and minimum IDs (this is already sequential)
  afterTxIdLogs <- deleteTablesAfterTxId txOutVariantType mtxId minIdsW

  -- Final block deletion (delete block last since everything references it)
  let (tableN, deleteSession) = prepareDelete @SCB.Block "id" blkId ">=" blockIdEncoder
  blockCount <- runSession deleteSession
  let blockLogs = [(tableN, blockCount)]

  -- Aggregate and return all logs
  pure (sum $ map snd blockLogs, initialLogs <> blockLogs <> offChainLogs <> afterTxIdLogs)

-----------------------------------------------------------------------------------------------------------------

deleteTablesAfterTxId ::
  SV.TxOutVariantType ->
  Maybe Id.TxId ->
  MinIdsWrapper ->
  DbM [(Text.Text, Int64)]
deleteTablesAfterTxId txOutVariantType mtxId minIdsW = do
  -- Handle MinIdsWrapper deletions (keep existing sequential logic unchanged)
  minIdsLogs <- case minIdsW of
    CMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) -> do
      -- Step 1: Delete TxIn records first
      txInLogs <- case mtxInId of
        Nothing -> pure []
        Just txInId -> do
          let (tableN, deleteSession) = prepareOnlyDelete @SCB.TxIn "id" txInId ">=" (Id.idEncoder Id.getTxInId)
          count <- runSession deleteSession
          pure [(tableN, count)]

      -- Step 2: Delete TxOut records second (after TxIn references are gone)
      txOutLogs <- case prepareTypedDelete @VC.TxOutCore "id" mtxOutId SV.unwrapTxOutIdCore (Id.idEncoder Id.getTxOutCoreId) of
        Nothing -> pure []
        Just (tableN, deleteSession) -> do
          count <- runSession deleteSession
          pure [(tableN, count)]

      -- Step 3: Delete MaTxOut records third (after TxOut references are gone)
      maTxOutLogs <- case prepareTypedDelete @VC.MaTxOutCore "id" mmaTxOutId SV.unwrapMaTxOutIdCore (Id.idEncoder Id.getMaTxOutCoreId) of
        Nothing -> pure []
        Just (tableN, deleteSession) -> do
          count <- runSession deleteSession
          pure [(tableN, count)]

      pure $ concat [txInLogs, txOutLogs, maTxOutLogs]
    VMinIdsWrapper (MinIds mtxInId mtxOutId mmaTxOutId) -> do
      -- Step 1: Delete TxIn records first
      txInLogs <- case mtxInId of
        Nothing -> pure []
        Just txInId -> do
          let (tableN, deleteSession) = prepareOnlyDelete @SCB.TxIn "id" txInId ">=" (Id.idEncoder Id.getTxInId)
          count <- runSession deleteSession
          pure [(tableN, count)]

      -- Step 2: Delete TxOut records second (after TxIn references are gone)
      txOutLogs <- case prepareTypedDelete @VA.TxOutAddress "id" mtxOutId SV.unwrapTxOutIdAddress (Id.idEncoder Id.getTxOutAddressId) of
        Nothing -> pure []
        Just (tableN, deleteSession) -> do
          count <- runSession deleteSession
          pure [(tableN, count)]

      -- Step 3: Delete MaTxOut records third (after TxOut references are gone)
      maTxOutLogs <- case prepareTypedDelete @VA.MaTxOutAddress "id" mmaTxOutId SV.unwrapMaTxOutIdAddress (Id.idEncoder Id.getMaTxOutAddressId) of
        Nothing -> pure []
        Just (tableN, deleteSession) -> do
          count <- runSession deleteSession
          pure [(tableN, count)]

      pure $ concat [txInLogs, txOutLogs, maTxOutLogs]

  -- Handle deletions using the TxId with correct queryDeleteAndLog logic
  txIdLogs <- case mtxId of
    Nothing -> pure []
    Just txId -> do
      -- Execute transaction-related deletions using queryDeleteAndLog pattern
      -- Depending on the TxOut variant type, select appropriate delete operations
      -- let deleteOperations :: [DbM (Maybe (Text.Text, HsqlSes.Session Int64))]
      let deleteOperations = case txOutVariantType of
            SV.TxOutVariantCore ->
              [ prepareQueryDeleteAndLogTx @VC.CollateralTxOutCore "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.CollateralTxIn "tx_in_id" txId
              , prepareQueryDeleteAndLogTx @SCB.ReferenceTxIn "tx_in_id" txId
              , prepareQueryDeleteAndLogTx @SCP.PoolRetire "announced_tx_id" txId
              , prepareQueryDeleteAndLogTx @SCS.StakeRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCS.StakeDeregistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCS.Delegation "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.TxMetadata "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Withdrawal "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCE.Treasury "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCE.Reserve "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCE.PotTransfer "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCM.MaTxMint "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Redeemer "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Script "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Datum "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.RedeemerData "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.ExtraKeyWitness "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.TxCbor "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.ParamProposal "registered_tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.DelegationVote "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.CommitteeRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.CommitteeDeRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.DrepRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.VotingProcedure "tx_id" txId
              ]
            SV.TxOutVariantAddress ->
              [ prepareQueryDeleteAndLogTx @VA.CollateralTxOutAddress "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.CollateralTxIn "tx_in_id" txId
              , prepareQueryDeleteAndLogTx @SCB.ReferenceTxIn "tx_in_id" txId
              , prepareQueryDeleteAndLogTx @SCP.PoolRetire "announced_tx_id" txId
              , prepareQueryDeleteAndLogTx @SCS.StakeRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCS.StakeDeregistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCS.Delegation "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.TxMetadata "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Withdrawal "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCE.Treasury "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCE.Reserve "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCE.PotTransfer "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCM.MaTxMint "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Redeemer "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Script "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.Datum "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.RedeemerData "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.ExtraKeyWitness "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCB.TxCbor "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.ParamProposal "registered_tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.DelegationVote "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.CommitteeRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.CommitteeDeRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.DrepRegistration "tx_id" txId
              , prepareQueryDeleteAndLogTx @SCG.VotingProcedure "tx_id" txId
              ]

      -- Execute all delete operations and collect logs
      actualOps <- catMaybes <$> sequence deleteOperations
      result <- forM actualOps $ \(tableN, deleteSession) -> do
        count <- runSession deleteSession
        pure (tableN, count)

      -- Handle GovActionProposal related deletions
      mgaId <- queryMinRefId @SCG.GovActionProposal "tx_id" txId (Id.idEncoder Id.getTxId)
      gaLogs <- case mgaId of
        Nothing -> pure []
        Just gaId -> do
          -- gaId is raw Int64, so create encoder for Int64
          let gaIdEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
              gaDeleteOps =
                [ prepareQueryDeleteAndLog @SCG.TreasuryWithdrawal "gov_action_proposal_id" gaId gaIdEncoder
                , prepareQueryThenNull @SCG.Committee "gov_action_proposal_id" gaId gaIdEncoder
                , prepareQueryThenNull @SCG.Constitution "gov_action_proposal_id" gaId gaIdEncoder
                , prepareQueryDeleteAndLog @SCG.GovActionProposal "id" gaId gaIdEncoder
                ]
          actualGaOps <- catMaybes <$> sequence gaDeleteOps
          forM actualGaOps $ \(tableN, deleteSession) -> do
            count <- runSession deleteSession
            pure (tableN, count)

      -- Handle PoolMetadataRef related deletions
      minPmr <- queryMinRefId @SCP.PoolMetadataRef "registered_tx_id" txId (Id.idEncoder Id.getTxId)
      pmrLogs <- case minPmr of
        Nothing -> pure []
        Just pmrId -> do
          -- pmrId is raw Int64, so create encoder for Int64
          let pmrIdEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
              pmrDeleteOps =
                [ prepareQueryDeleteAndLog @SCO.OffChainPoolData "pmr_id" pmrId pmrIdEncoder
                , prepareQueryDeleteAndLog @SCO.OffChainPoolFetchError "pmr_id" pmrId pmrIdEncoder
                , prepareQueryDeleteAndLog @SCP.PoolMetadataRef "id" pmrId pmrIdEncoder
                ]
          actualPmrOps <- catMaybes <$> sequence pmrDeleteOps
          forM actualPmrOps $ \(tableN, deleteSession) -> do
            count <- runSession deleteSession
            pure (tableN, count)

      -- Handle PoolUpdate related deletions
      minPoolUpdate <- queryMinRefId @SCP.PoolUpdate "registered_tx_id" txId (Id.idEncoder Id.getTxId)
      poolUpdateLogs <- case minPoolUpdate of
        Nothing -> pure []
        Just puid -> do
          -- puid is raw Int64, so create encoder for Int64
          let puidEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
              puDeleteOps =
                [ prepareQueryDeleteAndLog @SCP.PoolOwner "pool_update_id" puid puidEncoder
                , prepareQueryDeleteAndLog @SCP.PoolRelay "update_id" puid puidEncoder
                , prepareQueryDeleteAndLog @SCP.PoolUpdate "id" puid puidEncoder
                ]
          actualPuOps <- catMaybes <$> sequence puDeleteOps
          forM actualPuOps $ \(tableN, deleteSession) -> do
            count <- runSession deleteSession
            pure (tableN, count)

      -- Final Tx deletion using direct delete (since we want to delete the tx itself)
      let (tableN, deleteSession) = prepareOnlyDelete @SCB.Tx "id" txId ">=" (Id.idEncoder Id.getTxId)
      txCount <- runSession deleteSession
      let txLogs = [(tableN, txCount)]

      pure $ result <> gaLogs <> pmrLogs <> poolUpdateLogs <> txLogs

  -- Return combined logs
  pure $ minIdsLogs <> txIdLogs

-----------------------------------------------------------------------------------------------------------------

prepareQueryDeleteAndLog ::
  forall a b.
  DbInfo a =>
  Text.Text -> -- Foreign key field name (e.g. "tx_id")
  b -> -- Foreign key value (e.g. txId)
  HsqlE.Params b -> -- Encoder for the foreign key
  DbM (Maybe (Text.Text, HsqlSes.Session Int64))
prepareQueryDeleteAndLog fkField fkValue fkEncoder = do
  -- Step 1: Find minimum record ID that references the foreign key
  mRecordId <- queryMinRefId @a fkField fkValue fkEncoder
  case mRecordId of
    Nothing -> pure Nothing -- No records to delete
    Just recordId -> do
      -- Step 2: Prepare to delete records where id >= recordId
      let tName = tableName (Proxy @a)
          deleteSession =
            HsqlSes.statement recordId $
              deleteWhereCount @a "id" ">=" (HsqlE.param $ HsqlE.nonNullable HsqlE.int8)
      pure $ Just (tName, deleteSession)

-- Even cleaner - make a helper for the common TxId case
prepareQueryDeleteAndLogTx ::
  forall a.
  DbInfo a =>
  Text.Text -> -- Foreign key field name (e.g. "tx_id")
  Id.TxId -> -- TxId value
  DbM (Maybe (Text.Text, HsqlSes.Session Int64))
prepareQueryDeleteAndLogTx fkField txId =
  prepareQueryDeleteAndLog @a fkField txId (Id.idEncoder Id.getTxId)

-- Helper for queryThenNull pattern (for nullable foreign keys)
prepareQueryThenNull ::
  forall a b.
  DbInfo a =>
  Text.Text -> -- Foreign key field name (e.g. "gov_action_proposal_id")
  b -> -- Foreign key value
  HsqlE.Params b -> -- Encoder for the foreign key
  DbM (Maybe (Text.Text, HsqlSes.Session Int64))
prepareQueryThenNull fkField fkValue fkEncoder = do
  -- Step 1: Find minimum record ID that references the foreign key (nullable version)
  mRecordId <- queryMinRefIdNullable @a fkField fkValue fkEncoder
  case mRecordId of
    Nothing -> pure Nothing -- No records to delete
    Just recordId -> do
      -- Step 2: Prepare to delete records where id >= recordId AND fkField IS NOT NULL
      let tName = tableName (Proxy @a)
          deleteSession =
            HsqlSes.statement recordId $
              deleteWhereCountWithNotNull @a "id" fkField (HsqlE.param $ HsqlE.nonNullable HsqlE.int8)
      pure $ Just (tName, deleteSession)
