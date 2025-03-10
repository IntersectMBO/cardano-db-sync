{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.OffChain where

import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlS

import qualified Cardano.Db.Schema.Core.OffChain as SO
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), mkDbTransaction, runDbT)
import Cardano.Db.Statement.Function.Insert (bulkInsertNoReturn, insert, insertCheckUnique)
import Cardano.Db.Statement.GovernanceAndVoting (queryVotingAnchorIdExists)
import Cardano.Db.Statement.Pool (queryPoolHashIdExists, queryPoolMetadataRefIdExists)
import Cardano.Db.Types (DbAction, DbTransMode (..))
import Cardano.Prelude (MonadIO, Text, when)

--------------------------------------------------------------------------------

-- | OffChainPoolData

--------------------------------------------------------------------------------
insertCheckOffChainPoolData :: MonadIO m => SO.OffChainPoolData -> DbAction m ()
insertCheckOffChainPoolData offChainPoolData = do
  let poolHashId = SO.offChainPoolDataPoolId offChainPoolData
  let metadataRefId = SO.offChainPoolDataPmrId offChainPoolData

  -- Use pipeline to check both IDs in a single database roundtrip
  (poolExists, metadataExists) <- runDbSession (mkCallInfo "insertCheckOffChainPoolData") $
    HsqlS.pipeline $ do
      p1 <- HsqlS.statement poolHashId queryPoolHashIdExistsStmt
      p2 <- HsqlS.statement metadataRefId queryPoolMetadataRefIdExistsStmt
      pure (p1, p2)

  -- Only insert if both exist
  when (poolExists && metadataExists) $
    runDbSession (mkCallInfo "insertOffChainPoolData") $
      HsqlS.statement offChainPoolData insertOffChainPoolDataStmt

insertOffChainPoolDataStmt :: HsqlS.Statement SO.OffChainPoolData ()
insertOffChainPoolDataStmt =
  insert
    SO.offChainPoolDataEncoder
    NoResult

insertCheckOffChainPoolData :: MonadIO m => SO.OffChainPoolData -> DbAction m ()
insertCheckOffChainPoolData offChainPoolData = do
  let poolHashId = SO.offChainPoolDataPoolId offChainPoolData
  let metadataRefId = SO.offChainPoolDataPmrId offChainPoolData

  -- Run checks in pipeline
  (poolExists, metadataExists) <- runDbSession (mkCallInfo "checkPoolAndMetadata") $
    HsqlS.pipeline $ do
      poolResult <- HsqlP.statement poolHashId queryPoolHashIdExistsStmt
      metadataResult <- HsqlP.statement metadataRefId queryPoolMetadataRefIdExistsStmt
      pure (poolResult, metadataResult)

  -- Only insert if both exist
  when (poolExists && metadataExists) $
    runDbSession (mkCallInfo "insertOffChainPoolData") $
      HsqlS.statement offChainPoolData insertOffChainPoolDataStmt

--------------------------------------------------------------------------------

-- | OffChainVoteAuthor

--------------------------------------------------------------------------------
bulkInsertOffChainVoteAuthors :: MonadIO m => [SO.OffChainVoteAuthor] -> DbAction m ()
bulkInsertOffChainVoteAuthors offChainVoteAuthors =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertOffChainVoteAuthors" $
      bulkInsertNoReturn
        extractOffChainVoteAuthor
        SO.offChainVoteAuthorBulkEncoder
        offChainVoteAuthors
  where
    extractOffChainVoteAuthor ::
      [SO.OffChainVoteAuthor] ->
      ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
    extractOffChainVoteAuthor xs =
      ( map SO.offChainVoteAuthorOffChainVoteDataId xs
      , map SO.offChainVoteAuthorName xs
      , map SO.offChainVoteAuthorWitnessAlgorithm xs
      , map SO.offChainVoteAuthorPublicKey xs
      , map SO.offChainVoteAuthorSignature xs
      , map SO.offChainVoteAuthorWarning xs
      )

insertOffChainVoteData :: MonadIO m => SO.OffChainVoteData -> DbAction m (Maybe Id.OffChainVoteDataId)
insertOffChainVoteData offChainVoteData = do
  foundVotingAnchorId <- queryVotingAnchorIdExists (SO.offChainVoteDataVotingAnchorId offChainVoteData)
  if foundVotingAnchorId
    then do
      runDbT TransWrite $
        mkDbTransaction "insertOffChainVoteData" $
          insertCheckUnique
            SO.offChainVoteDataEncoder
            (WithResult (HsqlD.singleRow $ Id.maybeIdDecoder Id.OffChainVoteDataId))
            offChainVoteData
    else pure Nothing

insertOffChainVoteDrepDataStmt :: HsqlS.Statement SO.OffChainVoteDrepData (Entity SO.OffChainVoteDrepData)
insertOffChainVoteDrepDataStmt =
  insert
    SO.offChainVoteDrepDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteDrepData)

insertOffChainVoteDrepData :: MonadIO m => SO.OffChainVoteDrepData -> DbAction m Id.OffChainVoteDataId
insertOffChainVoteDrepData drepData = do
  entity <-
    runDbSession (mkCallInfo "insertOffChainVoteDrepData") $
      HsqlS.statement drepData insertOffChainVoteDrepDataStmt
  pure $ entityKey entity

insertOffChainVoteDrepData :: MonadIO m => SO.OffChainVoteDrepData -> DbAction m Id.OffChainVoteDataId
insertOffChainVoteDrepData drepData =
  runDbT TransWrite $ mkDbTransaction "insertOffChainVoteDrepData" $ do
    entity <-
      insert
        SO.offChainVoteDrepDataEncoder
        (WithResult $ HsqlD.singleRow SO.entityOffChainVoteData)
        drepData
    pure (entityKey entity)

--------------------------------------------------------------------------------

-- | OffChainVoteExternalUpdate

--------------------------------------------------------------------------------
bulkInsertOffChainVoteExternalUpdate :: MonadIO m => [SO.OffChainVoteExternalUpdate] -> DbAction m ()
bulkInsertOffChainVoteExternalUpdate offChainVoteExternalUpdates =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertOffChainVoteExternalUpdate" $
      bulkInsertNoReturn
        extractOffChainVoteExternalUpdate
        SO.offChainVoteExternalUpdatesEncoder
        offChainVoteExternalUpdates
  where
    extractOffChainVoteExternalUpdate :: [SO.OffChainVoteExternalUpdate] -> ([Id.OffChainVoteDataId], [Text], [Text])
    extractOffChainVoteExternalUpdate xs =
      ( map SO.offChainVoteExternalUpdateOffChainVoteDataId xs
      , map SO.offChainVoteExternalUpdateTitle xs
      , map SO.offChainVoteExternalUpdateUri xs
      )

insertOffChainVoteFetchError :: MonadIO m => SO.OffChainVoteFetchError -> DbAction m ()
insertOffChainVoteFetchError offChainVoteFetchError = do
  foundVotingAnchor <-
    queryVotingAnchorIdExists (SO.offChainVoteFetchErrorVotingAnchorId offChainVoteFetchError)
  when foundVotingAnchor $ do
    runDbT TransWrite $ mkDbTransaction "insertOffChainVoteError" $ do
      void $
        insert
          SO.offChainVoteFetchErrorEncoder
          NoResult
          offChainVoteFetchError

--------------------------------------------------------------------------------

-- | OffChainVoteGovActionData

--------------------------------------------------------------------------------
insertOffChainVoteGovActionDataStmt :: HsqlS.Statement SO.OffChainVoteGovActionData (Entity SO.OffChainVoteGovActionData)
insertOffChainVoteGovActionDataStmt =
  insert
    SO.offChainVoteGovActionDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteGovActionData)

insertOffChainVoteGovActionData :: MonadIO m => SO.OffChainVoteGovActionData -> DbAction m Id.OffChainVoteGovActionDataId
insertOffChainVoteGovActionData offChainVoteGovActionData = do
  entity <-
    runDbSession (mkCallInfo "insertOffChainVoteGovActionData") $
      HsqlS.statement offChainVoteGovActionData insertOffChainVoteGovActionDataStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | OffChainVoteReference

--------------------------------------------------------------------------------
bulkInsertOffChainVoteReferences :: MonadIO m => [SO.OffChainVoteReference] -> DbAction m ()
bulkInsertOffChainVoteReferences offChainVoteReferences =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertOffChainVoteReferences" $
      bulkInsertNoReturn
        extractOffChainVoteReference
        SO.offChainVoteReferenceBulkEncoder
        offChainVoteReferences
  where
    extractOffChainVoteReference :: [SO.OffChainVoteReference] -> ([Id.OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
    extractOffChainVoteReference xs =
      ( map SO.offChainVoteReferenceOffChainVoteDataId xs
      , map SO.offChainVoteReferenceLabel xs
      , map SO.offChainVoteReferenceUri xs
      , map SO.offChainVoteReferenceHashDigest xs
      , map SO.offChainVoteReferenceHashAlgorithm xs
      )

-- off_chain_pool_data
-- off_chain_pool_fetch_error
-- off_chain_vote_author
-- off_chain_vote_data
-- off_chain_vote_drep_data
-- off_chain_vote_external_update
-- off_chain_vote_fetch_error
-- off_chain_vote_gov_action_data
-- off_chain_vote_reference
