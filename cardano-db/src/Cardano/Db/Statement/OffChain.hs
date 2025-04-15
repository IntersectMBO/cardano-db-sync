{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.OffChain where

import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlS

import qualified Cardano.Db.Schema.Core.OffChain as SO
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk, insertCheckUnique)
import Cardano.Db.Statement.GovernanceAndVoting (queryVotingAnchorIdExists)
import Cardano.Db.Statement.Pool (queryPoolHashIdExistsStmt, queryPoolMetadataRefIdExistsStmt)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction)
import Cardano.Prelude (MonadIO (..), Text, when)
import qualified Hasql.Statement as HsqlS

--------------------------------------------------------------------------------
-- OffChainPoolData
--------------------------------------------------------------------------------
insertOffChainPoolDataStmt :: HsqlS.Statement SO.OffChainPoolData ()
insertOffChainPoolDataStmt =
  insertCheckUnique
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
-- OffChainVoteAuthor
--------------------------------------------------------------------------------
insertBulkOffChainVoteAuthorsStmt :: HsqlS.Statement [SO.OffChainVoteAuthor] ()
insertBulkOffChainVoteAuthorsStmt =
  insertBulk
    extractOffChainVoteAuthor
    SO.offChainVoteAuthorBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteAuthor :: [SO.OffChainVoteAuthor] -> ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
    extractOffChainVoteAuthor xs =
      ( map SO.offChainVoteAuthorOffChainVoteDataId xs
      , map SO.offChainVoteAuthorName xs
      , map SO.offChainVoteAuthorWitnessAlgorithm xs
      , map SO.offChainVoteAuthorPublicKey xs
      , map SO.offChainVoteAuthorSignature xs
      , map SO.offChainVoteAuthorWarning xs
      )

insertBulkOffChainVoteAuthors :: MonadIO m => [SO.OffChainVoteAuthor] -> DbAction m ()
insertBulkOffChainVoteAuthors offChainVoteAuthors =
  runDbSession (mkCallInfo "insertBulkOffChainVoteAuthors") $
    HsqlS.statement offChainVoteAuthors insertBulkOffChainVoteAuthorsStmt

insertOffChainVoteDataStmt :: HsqlS.Statement SO.OffChainVoteData (Entity SO.OffChainVoteData)
insertOffChainVoteDataStmt =
  insertCheckUnique
    SO.offChainVoteDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteDataDecoder)

insertOffChainVoteData :: MonadIO m => SO.OffChainVoteData -> DbAction m (Maybe Id.OffChainVoteDataId)
insertOffChainVoteData offChainVoteData = do
  foundVotingAnchorId <- queryVotingAnchorIdExists (SO.offChainVoteDataVotingAnchorId offChainVoteData)
  if foundVotingAnchorId
    then do
      entity <-
        runDbSession (mkCallInfo "insertOffChainVoteData") $
          HsqlS.statement offChainVoteData insertOffChainVoteDataStmt
      pure $ Just (entityKey entity)
    else pure Nothing

insertOffChainVoteDrepDataStmt :: HsqlS.Statement SO.OffChainVoteDrepData (Entity SO.OffChainVoteDrepData)
insertOffChainVoteDrepDataStmt =
  insert
    SO.offChainVoteDrepDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteDrepDataDecoder)

insertOffChainVoteDrepData :: MonadIO m => SO.OffChainVoteDrepData -> DbAction m Id.OffChainVoteDrepDataId
insertOffChainVoteDrepData drepData = do
  entity <-
    runDbSession (mkCallInfo "insertOffChainVoteDrepData") $
      HsqlS.statement drepData insertOffChainVoteDrepDataStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- OffChainVoteExternalUpdate
--------------------------------------------------------------------------------
insertBulkOffChainVoteExternalUpdatesStmt :: HsqlS.Statement [SO.OffChainVoteExternalUpdate] ()
insertBulkOffChainVoteExternalUpdatesStmt =
  insertBulk
    extractOffChainVoteExternalUpdate
    SO.offChainVoteExternalUpdatesBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteExternalUpdate :: [SO.OffChainVoteExternalUpdate] -> ([Id.OffChainVoteDataId], [Text], [Text])
    extractOffChainVoteExternalUpdate xs =
      ( map SO.offChainVoteExternalUpdateOffChainVoteDataId xs
      , map SO.offChainVoteExternalUpdateTitle xs
      , map SO.offChainVoteExternalUpdateUri xs
      )

insertBulkOffChainVoteExternalUpdate :: MonadIO m => [SO.OffChainVoteExternalUpdate] -> DbAction m ()
insertBulkOffChainVoteExternalUpdate offChainVoteExternalUpdates =
  runDbSession (mkCallInfo "insertBulkOffChainVoteExternalUpdate") $
    HsqlS.statement offChainVoteExternalUpdates insertBulkOffChainVoteExternalUpdatesStmt

insertOffChainVoteFetchErrorStmt :: HsqlS.Statement SO.OffChainVoteFetchError ()
insertOffChainVoteFetchErrorStmt =
  insert
    SO.offChainVoteFetchErrorEncoder
    NoResult

insertOffChainVoteFetchError :: MonadIO m => SO.OffChainVoteFetchError -> DbAction m ()
insertOffChainVoteFetchError offChainVoteFetchError = do
  foundVotingAnchor <-
    queryVotingAnchorIdExists (SO.offChainVoteFetchErrorVotingAnchorId offChainVoteFetchError)
  when foundVotingAnchor $ do
    runDbSession (mkCallInfo "insertOffChainVoteFetchError") $
      HsqlS.statement offChainVoteFetchError insertOffChainVoteFetchErrorStmt

--------------------------------------------------------------------------------
-- OffChainVoteGovActionData
--------------------------------------------------------------------------------
insertOffChainVoteGovActionDataStmt :: HsqlS.Statement SO.OffChainVoteGovActionData (Entity SO.OffChainVoteGovActionData)
insertOffChainVoteGovActionDataStmt =
  insert
    SO.offChainVoteGovActionDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteGovActionDataDecoder)

insertOffChainVoteGovActionData :: MonadIO m => SO.OffChainVoteGovActionData -> DbAction m Id.OffChainVoteGovActionDataId
insertOffChainVoteGovActionData offChainVoteGovActionData = do
  entity <-
    runDbSession (mkCallInfo "insertOffChainVoteGovActionData") $
      HsqlS.statement offChainVoteGovActionData insertOffChainVoteGovActionDataStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- OffChainVoteReference
--------------------------------------------------------------------------------
insertBulkOffChainVoteReferencesStmt :: HsqlS.Statement [SO.OffChainVoteReference] ()
insertBulkOffChainVoteReferencesStmt =
  insertBulk
    extractOffChainVoteReference
    SO.offChainVoteReferenceBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteReference :: [SO.OffChainVoteReference] -> ([Id.OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
    extractOffChainVoteReference xs =
      ( map SO.offChainVoteReferenceOffChainVoteDataId xs
      , map SO.offChainVoteReferenceLabel xs
      , map SO.offChainVoteReferenceUri xs
      , map SO.offChainVoteReferenceHashDigest xs
      , map SO.offChainVoteReferenceHashAlgorithm xs
      )

insertBulkOffChainVoteReferences :: MonadIO m => [SO.OffChainVoteReference] -> DbAction m ()
insertBulkOffChainVoteReferences offChainVoteReferences =
  runDbSession (mkCallInfo "insertBulkOffChainVoteReferences") $
    HsqlS.statement offChainVoteReferences insertBulkOffChainVoteReferencesStmt

-- off_chain_pool_data
-- off_chain_pool_fetch_error
-- off_chain_vote_author
-- off_chain_vote_data
-- off_chain_vote_drep_data
-- off_chain_vote_external_update
-- off_chain_vote_fetch_error
-- off_chain_vote_gov_action_data
-- off_chain_vote_reference
