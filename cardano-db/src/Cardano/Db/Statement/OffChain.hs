{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.OffChain where

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction, DbTransMode (..))
import Cardano.Prelude (MonadIO, Text, when)
import qualified Cardano.Db.Schema.Core.OffChain as SO
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..))
import Cardano.Db.Statement.Function.Insert (insert, bulkInsertNoReturn, insertCheckUnique)
import Cardano.Db.Statement.GovernanceAndVoting (queryVotingAnchorIdExists)
import Cardano.Db.Statement.Pool (queryPoolHashIdExists, queryPoolMetadataRefIdExists)
import qualified Hasql.Decoders as HsqlD


insertCheckOffChainPoolData :: MonadIO m => SO.OffChainPoolData -> DbAction m ()
insertCheckOffChainPoolData offChainPoolData = do
  foundPoolHashId <- queryPoolHashIdExists (SO.offChainPoolDataPoolId offChainPoolData)
  foundMetadataRefId <- queryPoolMetadataRefIdExists (SO.offChainPoolDataPmrId offChainPoolData)
  when (foundPoolHashId && foundMetadataRefId) $ do
    runDbT TransWrite $ mkDbTransaction "insertCheckOffChainPoolData" $
      insert
        SO.offChainPoolDataEncoder
        NoResult
        offChainPoolData

insertCheckOffChainPoolFetchError :: MonadIO m => SO.OffChainPoolFetchError -> DbAction m ()
insertCheckOffChainPoolFetchError offChainPoolFetchError = do
  foundPoolHashId <- queryPoolHashIdExists (SO.offChainPoolFetchErrorPoolId offChainPoolFetchError)
  foundMetadataRefId <- queryPoolMetadataRefIdExists (SO.offChainPoolFetchErrorPmrId offChainPoolFetchError)
  when (foundPoolHashId && foundMetadataRefId) $ do
    runDbT TransWrite $ mkDbTransaction "insertCheckOffChainPoolFetchError" $
      insert
        SO.offChainPoolFetchErrorEncoder
        NoResult
        offChainPoolFetchError
--------------------------------------------------------------------------------
-- | OffChainVoteAuthor
--------------------------------------------------------------------------------
insertManyOffChainVoteAuthors :: MonadIO m => [SO.OffChainVoteAuthor] -> DbAction m ()
insertManyOffChainVoteAuthors offChainVoteAuthors =
  runDbT TransWrite $ mkDbTransaction "insertManyOffChainVoteAuthors" $
    bulkInsertNoReturn
      extractOffChainVoteAuthor
      SO.offChainVoteAuthorManyEncoder
      offChainVoteAuthors
  where
    extractOffChainVoteAuthor
      :: [SO.OffChainVoteAuthor]
      -> ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
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
      runDbT TransWrite $ mkDbTransaction "insertOffChainVoteData" $
        insertCheckUnique
          SO.offChainVoteDataEncoder
          (WithResult (HsqlD.singleRow $ Id.maybeIdDecoder Id.OffChainVoteDataId))
          offChainVoteData
    else pure Nothing

insertOffChainVoteDrepData :: MonadIO m => SO.OffChainVoteDrepData -> DbAction m Id.OffChainVoteDataId
insertOffChainVoteDrepData drepData =
  runDbT TransWrite $ mkDbTransaction "insertOffChainVoteDrepData" $
    insert
      SO.offChainVoteDrepDataEncoder
      (WithResult (HsqlD.singleRow $ Id.idDecoder Id.OffChainVoteDataId))
      drepData

--------------------------------------------------------------------------------
-- | OffChainVoteExternalUpdate
--------------------------------------------------------------------------------
insertManyOffChainVoteExternalUpdate :: MonadIO m => [SO.OffChainVoteExternalUpdate] -> DbAction m ()
insertManyOffChainVoteExternalUpdate offChainVoteExternalUpdates =
  runDbT TransWrite $ mkDbTransaction "insertManyOffChainVoteExternalUpdate" $
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
    runDbT TransWrite $ mkDbTransaction "insertOffChainVoteError" $
      insert
        SO.offChainVoteFetchErrorEncoder
        NoResult
        offChainVoteFetchError

--------------------------------------------------------------------------------
-- | OffChainVoteGovActionData
--------------------------------------------------------------------------------
insertOffChainVoteGovActionData :: MonadIO m => SO.OffChainVoteGovActionData -> DbAction m Id.OffChainVoteGovActionDataId
insertOffChainVoteGovActionData offChainVoteGovActionData = runDbT TransWrite $ mkDbTransaction "insertOffChainVoteGovActionData" $
  insert
    SO.offChainVoteGovActionDataEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.OffChainVoteGovActionDataId))
    offChainVoteGovActionData

--------------------------------------------------------------------------------
-- | OffChainVoteReference
--------------------------------------------------------------------------------
insertManyOffChainVoteReferences :: MonadIO m => [SO.OffChainVoteReference] -> DbAction m ()
insertManyOffChainVoteReferences offChainVoteReferences =
  runDbT TransWrite $ mkDbTransaction "insertManyOffChainVoteReferences" $
    bulkInsertNoReturn
      extractOffChainVoteReference
      SO.offChainVoteReferenceManyEncoder
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
