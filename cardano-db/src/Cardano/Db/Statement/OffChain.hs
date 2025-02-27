{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.OffChain where

import Cardano.Db.Schema.Ids (OffChainVoteDataId)
import Cardano.Db.Statement.Helpers (runDbT, mkDbTransaction, bulkInsertNoReturn)
import Cardano.Db.Types (DbAction, DbTxMode (..))
import Cardano.Prelude (MonadIO, Text)
import Cardano.Db.Schema.Core.OffChain (
  OffChainVoteAuthor(..),
  OffChainVoteExternalUpdate(..),
  OffChainVoteReference(..),
  offChainVoteAuthorManyEncoder,
  offChainVoteExternalUpdatesEncoder,
  offChainVoteReferenceManyEncoder,
  )

--------------------------------------------------------------------------------
-- INSERT Statements
--------------------------------------------------------------------------------

-- | Insert a list of 'OffChainVoteAuthor' into the database.
insertManyOffChainVoteAuthors :: MonadIO m => [OffChainVoteAuthor] -> DbAction m ()
insertManyOffChainVoteAuthors offChainVoteAuthors =
  runDbT Write $ mkDbTransaction "insertManyOffChainVoteAuthors" $
    bulkInsertNoReturn
      extractOffChainVoteAuthor
      offChainVoteAuthorManyEncoder
      offChainVoteAuthors
  where
    extractOffChainVoteAuthor
      :: [OffChainVoteAuthor]
      -> ([OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
    extractOffChainVoteAuthor xs =
        ( map offChainVoteAuthorOffChainVoteDataId xs
        , map offChainVoteAuthorName xs
        , map offChainVoteAuthorWitnessAlgorithm xs
        , map offChainVoteAuthorPublicKey xs
        , map offChainVoteAuthorSignature xs
        , map offChainVoteAuthorWarning xs
        )

-- | Insert a list of 'OffChainVoteExternalUpdate' into the database.
insertManyOffChainVoteExternalUpdate :: MonadIO m => [OffChainVoteExternalUpdate] -> DbAction m ()
insertManyOffChainVoteExternalUpdate offChainVoteExternalUpdates =
  runDbT Write $ mkDbTransaction "insertManyOffChainVoteExternalUpdate" $
    bulkInsertNoReturn
      extractOffChainVoteExternalUpdate
      offChainVoteExternalUpdatesEncoder
      offChainVoteExternalUpdates
  where
    extractOffChainVoteExternalUpdate :: [OffChainVoteExternalUpdate] -> ([OffChainVoteDataId], [Text], [Text])
    extractOffChainVoteExternalUpdate xs =
        ( map offChainVoteExternalUpdateOffChainVoteDataId xs
        , map offChainVoteExternalUpdateTitle xs
        , map offChainVoteExternalUpdateUri xs
        )

-- | Insert a list of 'OffChainVoteReference' into the database.
insertManyOffChainVoteReferences :: MonadIO m => [OffChainVoteReference] -> DbAction m ()
insertManyOffChainVoteReferences offChainVoteReferences =
  runDbT Write $ mkDbTransaction "insertManyOffChainVoteReferences" $
    bulkInsertNoReturn
      extractOffChainVoteReference
      offChainVoteReferenceManyEncoder
      offChainVoteReferences
  where
    extractOffChainVoteReference :: [OffChainVoteReference] -> ([OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
    extractOffChainVoteReference xs =
        ( map offChainVoteReferenceOffChainVoteDataId xs
        , map offChainVoteReferenceLabel xs
        , map offChainVoteReferenceUri xs
        , map offChainVoteReferenceHashDigest xs
        , map offChainVoteReferenceHashAlgorithm xs
        )

-- off_chain_pool_data
-- off_chain_pool_fetch_error
-- off_chain_vote_data
-- off_chain_vote_fetch_error
-- off_chain_vote_author
-- off_chain_vote_reference
-- off_chain_vote_external_update
-- off_chain_vote_gov_action_data
-- off_chain_vote_drep_data
