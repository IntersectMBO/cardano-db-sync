{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import Cardano.Prelude (ByteString, Int64, MonadError (..), MonadIO, Proxy (..), Word64)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEP
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Statement.Function.Query (existsById)
import Cardano.Db.Statement.Types (DbInfo (..), validateColumn)
import Cardano.Db.Types (DbAction, DbLovelace, hardcodedAlwaysAbstain, hardcodedAlwaysNoConfidence)

--------------------------------------------------------------------------------
-- Committee
--------------------------------------------------------------------------------
insertCommitteeStmt :: HsqlStmt.Statement SGV.Committee Id.CommitteeId
insertCommitteeStmt =
  insert
    SGV.committeeEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeId)

insertCommittee :: MonadIO m => SGV.Committee -> DbAction m Id.CommitteeId
insertCommittee committee = do
  runDbSession (mkDbCallStack "insertCommittee") $ HsqlSes.statement committee insertCommitteeStmt

queryProposalCommitteeStmt :: HsqlStmt.Statement (Maybe Id.GovActionProposalId) [Id.CommitteeId]
queryProposalCommitteeStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SGV.Committee)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM " <> table
          , " WHERE ($1::bigint IS NULL AND gov_action_proposal_id IS NULL)"
          , " OR ($1::bigint IS NOT NULL AND gov_action_proposal_id = $1)"
          ]

    encoder =
      HsqlE.param
        ( HsqlE.nullable $
            Id.getGovActionProposalId >$< HsqlE.int8
        )

    decoder =
      HsqlD.rowList
        ( HsqlD.column $
            HsqlD.nonNullable $
              Id.CommitteeId <$> HsqlD.int8
        )

queryProposalCommittee :: MonadIO m => Maybe Id.GovActionProposalId -> DbAction m [Id.CommitteeId]
queryProposalCommittee mgapId =
  runDbSession (mkDbCallStack "queryProposalCommittee") $
    HsqlSes.statement mgapId queryProposalCommitteeStmt

--------------------------------------------------------------------------------
-- CommitteeHash
--------------------------------------------------------------------------------

-- | Insert
insertCommitteeHashStmt :: HsqlStmt.Statement SGV.CommitteeHash Id.CommitteeHashId
insertCommitteeHashStmt =
  insertCheckUnique
    SGV.committeeHashEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeHashId)

insertCommitteeHash :: MonadIO m => SGV.CommitteeHash -> DbAction m Id.CommitteeHashId
insertCommitteeHash committeeHash = do
  runDbSession (mkDbCallStack "insertCommitteeHash") $ HsqlSes.statement committeeHash insertCommitteeHashStmt

-- | Query
queryCommitteeHashStmt :: HsqlStmt.Statement ByteString (Maybe Id.CommitteeHashId)
queryCommitteeHashStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SGV.CommitteeHash)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM " <> table
          , " WHERE raw = $1"
          , " LIMIT 1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.singleRow $ Id.maybeIdDecoder Id.CommitteeHashId

queryCommitteeHash :: MonadIO m => ByteString -> DbAction m (Maybe Id.CommitteeHashId)
queryCommitteeHash hash =
  runDbSession (mkDbCallStack "queryCommitteeHash") $
    HsqlSes.statement hash queryCommitteeHashStmt

--------------------------------------------------------------------------------
-- CommitteeMember
--------------------------------------------------------------------------------
insertCommitteeMemberStmt :: HsqlStmt.Statement SGV.CommitteeMember Id.CommitteeMemberId
insertCommitteeMemberStmt =
  insert
    SGV.committeeMemberEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeMemberId)

insertCommitteeMember :: MonadIO m => SGV.CommitteeMember -> DbAction m Id.CommitteeMemberId
insertCommitteeMember committeeMember = do
  runDbSession (mkDbCallStack "insertCommitteeMember") $ HsqlSes.statement committeeMember insertCommitteeMemberStmt

insertCommitteeDeRegistrationStmt :: HsqlStmt.Statement SGV.CommitteeDeRegistration Id.CommitteeDeRegistrationId
insertCommitteeDeRegistrationStmt =
  insert
    SGV.committeeDeRegistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeDeRegistrationId)

insertCommitteeDeRegistration :: MonadIO m => SGV.CommitteeDeRegistration -> DbAction m Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration = do
  runDbSession (mkDbCallStack "insertCommitteeDeRegistration") $
    HsqlSes.statement committeeDeRegistration insertCommitteeDeRegistrationStmt

insertCommitteeRegistrationStmt :: HsqlStmt.Statement SGV.CommitteeRegistration Id.CommitteeRegistrationId
insertCommitteeRegistrationStmt =
  insert
    SGV.committeeRegistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeRegistrationId)

insertCommitteeRegistration :: MonadIO m => SGV.CommitteeRegistration -> DbAction m Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration = do
  runDbSession (mkDbCallStack "insertCommitteeRegistration") $
    HsqlSes.statement committeeRegistration insertCommitteeRegistrationStmt

--------------------------------------------------------------------------------
-- Constitution
--------------------------------------------------------------------------------
insertConstitutionStmt :: HsqlStmt.Statement SGV.Constitution Id.ConstitutionId
insertConstitutionStmt =
  insert
    SGV.constitutionEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ConstitutionId)

insertConstitution :: MonadIO m => SGV.Constitution -> DbAction m Id.ConstitutionId
insertConstitution constitution = do
  runDbSession (mkDbCallStack "insertConstitution") $ HsqlSes.statement constitution insertConstitutionStmt

queryProposalConstitutionStmt :: HsqlStmt.Statement (Maybe Id.GovActionProposalId) [Id.ConstitutionId]
queryProposalConstitutionStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SGV.Constitution)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM " <> table
          , " WHERE ($1::bigint IS NULL AND gov_action_proposal_id IS NULL)"
          , " OR ($1::bigint IS NOT NULL AND gov_action_proposal_id = $1)"
          ]

    encoder =
      HsqlE.param
        ( HsqlE.nullable $
            Id.getGovActionProposalId >$< HsqlE.int8
        )

    decoder =
      HsqlD.rowList
        ( HsqlD.column $
            HsqlD.nonNullable $
              Id.ConstitutionId <$> HsqlD.int8
        )

queryProposalConstitution :: MonadIO m => Maybe Id.GovActionProposalId -> DbAction m [Id.ConstitutionId]
queryProposalConstitution mgapId =
  runDbSession (mkDbCallStack "queryProposalConstitution") $
    HsqlSes.statement mgapId queryProposalConstitutionStmt

--------------------------------------------------------------------------------
-- DelegationVote
--------------------------------------------------------------------------------
insertDelegationVoteStmt :: HsqlStmt.Statement SGV.DelegationVote Id.DelegationVoteId
insertDelegationVoteStmt =
  insert
    SGV.delegationVoteEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DelegationVoteId)

insertDelegationVote :: MonadIO m => SGV.DelegationVote -> DbAction m Id.DelegationVoteId
insertDelegationVote delegationVote = do
  runDbSession (mkDbCallStack "insertDelegationVote") $ HsqlSes.statement delegationVote insertDelegationVoteStmt

--------------------------------------------------------------------------------
-- Drep
--------------------------------------------------------------------------------

-- | INSERT
insertDrepHashStmt :: HsqlStmt.Statement SGV.DrepHash Id.DrepHashId
insertDrepHashStmt =
  insertCheckUnique
    SGV.drepHashEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DrepHashId)

insertDrepHash :: MonadIO m => SGV.DrepHash -> DbAction m Id.DrepHashId
insertDrepHash drepHash = do
  runDbSession (mkDbCallStack "insertDrepHash") $ HsqlSes.statement drepHash insertDrepHashStmt

insertDrepHashAbstainStmt :: HsqlStmt.Statement SGV.DrepHash Id.DrepHashId
insertDrepHashAbstainStmt =
  insert
    SGV.drepHashEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.DrepHashId))

insertDrepHashAlwaysAbstain :: MonadIO m => DbAction m Id.DrepHashId
insertDrepHashAlwaysAbstain = do
  qr <- queryDrepHashAlwaysAbstain
  maybe ins pure qr
  where
    ins =
      runDbSession (mkDbCallStack "insertDrepHashAlwaysAbstain") $
        HsqlSes.statement drepHashAbstain insertDrepHashAbstainStmt

    drepHashAbstain =
      SGV.DrepHash
        { SGV.drepHashRaw = Nothing
        , SGV.drepHashView = hardcodedAlwaysAbstain
        , SGV.drepHashHasScript = False
        }

insertDrepHashAlwaysNoConfidence :: MonadIO m => DbAction m Id.DrepHashId
insertDrepHashAlwaysNoConfidence = do
  qr <- queryDrepHashAlwaysNoConfidence
  maybe ins pure qr
  where
    ins =
      runDbSession (mkDbCallStack "insertDrepHashAlwaysNoConfidence") $
        HsqlSes.statement drepHashNoConfidence insertDrepHashAbstainStmt

    drepHashNoConfidence =
      SGV.DrepHash
        { SGV.drepHashRaw = Nothing
        , SGV.drepHashView = hardcodedAlwaysNoConfidence
        , SGV.drepHashHasScript = False
        }

insertDrepRegistrationStmt :: HsqlStmt.Statement SGV.DrepRegistration Id.DrepRegistrationId
insertDrepRegistrationStmt =
  insert
    SGV.drepRegistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DrepRegistrationId)

insertDrepRegistration :: MonadIO m => SGV.DrepRegistration -> DbAction m Id.DrepRegistrationId
insertDrepRegistration drepRegistration = do
  runDbSession (mkDbCallStack "insertDrepRegistration") $ HsqlSes.statement drepRegistration insertDrepRegistrationStmt

insertBulkDrepDistrStmt :: HsqlStmt.Statement [SGV.DrepDistr] ()
insertBulkDrepDistrStmt =
  insertBulk
    extractDrepDistr
    SGV.drepDistrBulkEncoder
    NoResultBulk
  where
    extractDrepDistr :: [SGV.DrepDistr] -> ([Id.DrepHashId], [Word64], [Word64], [Maybe Word64])
    extractDrepDistr xs =
      ( map SGV.drepDistrHashId xs
      , map SGV.drepDistrAmount xs
      , map SGV.drepDistrEpochNo xs
      , map SGV.drepDistrActiveUntil xs
      )

insertBulkDrepDistr :: MonadIO m => [SGV.DrepDistr] -> DbAction m ()
insertBulkDrepDistr drepDistrs = do
  runDbSession (mkDbCallStack "insertBulkDrepDistr") $
    HsqlSes.statement drepDistrs insertBulkDrepDistrStmt

-- | QUERY
queryDrepHashSpecialStmt ::
  forall a.
  DbInfo a =>
  Text.Text -> -- targetValue
  HsqlStmt.Statement () (Maybe Id.DrepHashId)
queryDrepHashSpecialStmt targetValue =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    rawCol = validateColumn @a "raw"
    viewCol = validateColumn @a "view"

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM "
          , table
          , " WHERE "
          , rawCol
          , " IS NULL"
          , " AND "
          , viewCol
          , " = '"
          , targetValue
          , "'"
          ]

    decoder =
      HsqlD.rowMaybe
        ( HsqlD.column $
            HsqlD.nonNullable $
              Id.DrepHashId <$> HsqlD.int8
        )

queryDrepHashAlwaysAbstain :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysAbstain =
  runDbSession (mkDbCallStack "queryDrepHashAlwaysAbstain") $
    HsqlSes.statement () $
      queryDrepHashSpecialStmt @SGV.DrepHash hardcodedAlwaysAbstain

queryDrepHashAlwaysNoConfidence :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysNoConfidence =
  runDbSession (mkDbCallStack "queryDrepHashAlwaysNoConfidence") $
    HsqlSes.statement () $
      queryDrepHashSpecialStmt @SGV.DrepHash hardcodedAlwaysNoConfidence

--------------------------------------------------------------------------------
-- GovActionProposal
--------------------------------------------------------------------------------

-- | INSERT
insertGovActionProposalStmt :: HsqlStmt.Statement SGV.GovActionProposal Id.GovActionProposalId
insertGovActionProposalStmt =
  insert
    SGV.govActionProposalEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.GovActionProposalId)

insertGovActionProposal :: MonadIO m => SGV.GovActionProposal -> DbAction m Id.GovActionProposalId
insertGovActionProposal govActionProposal = do
  runDbSession (mkDbCallStack "insertGovActionProposal") $
    HsqlSes.statement govActionProposal insertGovActionProposalStmt

-- | UPDATE

-- Statement for updateGovActionState
updateGovActionStateStmt ::
  Text.Text ->
  ResultType Int64 r ->
  HsqlStmt.Statement (Id.GovActionProposalId, Int64) r
updateGovActionStateStmt columnName resultType =
  HsqlStmt.Statement sql encoder decoder True
  where
    decoder = case resultType of
      NoResult -> HsqlD.noResult
      WithResult _ -> HsqlD.rowsAffected
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE gov_action_proposal"
          , " SET "
          , columnName
          , " = $2"
          , " WHERE id = $1 AND "
          , columnName
          , " IS NULL"
          ]
    encoder =
      mconcat
        [ fst >$< Id.idEncoder Id.getGovActionProposalId
        , snd >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
        ]

-- Statement for setGovActionStateNull
setGovActionStateNullStmt ::
  -- | Column name to update
  Text.Text ->
  HsqlStmt.Statement Int64 Int64
setGovActionStateNullStmt columnName =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE gov_action_proposal"
          , " SET "
          , columnName
          , " = NULL"
          , " WHERE "
          , columnName
          , " IS NOT NULL AND "
          , columnName
          , " > $1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowsAffected

-- Statements
updateGovActionEnactedStmt :: HsqlStmt.Statement (Id.GovActionProposalId, Int64) Int64
updateGovActionEnactedStmt = updateGovActionStateStmt "enacted_epoch" (WithResult HsqlD.rowsAffected)

updateGovActionRatifiedStmt :: HsqlStmt.Statement (Id.GovActionProposalId, Int64) ()
updateGovActionRatifiedStmt = updateGovActionStateStmt "ratified_epoch" NoResult

updateGovActionDroppedStmt :: HsqlStmt.Statement (Id.GovActionProposalId, Int64) ()
updateGovActionDroppedStmt = updateGovActionStateStmt "dropped_epoch" NoResult

updateGovActionExpiredStmt :: HsqlStmt.Statement (Id.GovActionProposalId, Int64) ()
updateGovActionExpiredStmt = updateGovActionStateStmt "expired_epoch" NoResult

setNullEnactedStmt :: HsqlStmt.Statement Int64 Int64
setNullEnactedStmt = setGovActionStateNullStmt "enacted_epoch"

setNullRatifiedStmt :: HsqlStmt.Statement Int64 Int64
setNullRatifiedStmt = setGovActionStateNullStmt "ratified_epoch"

setNullExpiredStmt :: HsqlStmt.Statement Int64 Int64
setNullExpiredStmt = setGovActionStateNullStmt "expired_epoch"

setNullDroppedStmt :: HsqlStmt.Statement Int64 Int64
setNullDroppedStmt = setGovActionStateNullStmt "dropped_epoch"

-- Executions
updateGovActionEnacted :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m Int64
updateGovActionEnacted gaid eNo =
  runDbSession (mkDbCallStack "updateGovActionEnacted") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionEnactedStmt

updateGovActionRatified :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionRatified gaid eNo =
  runDbSession (mkDbCallStack "updateGovActionRatified") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionRatifiedStmt

updateGovActionDropped :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionDropped gaid eNo =
  runDbSession (mkDbCallStack "updateGovActionDropped") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionDroppedStmt

updateGovActionExpired :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionExpired gaid eNo =
  runDbSession (mkDbCallStack "updateGovActionExpired") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionExpiredStmt

setNullEnacted :: MonadIO m => Word64 -> DbAction m Int64
setNullEnacted eNo =
  runDbSession (mkDbCallStack "setNullEnacted") $
    HsqlSes.statement (fromIntegral eNo) setNullEnactedStmt

setNullRatified :: MonadIO m => Word64 -> DbAction m Int64
setNullRatified eNo =
  runDbSession (mkDbCallStack "setNullRatified") $
    HsqlSes.statement (fromIntegral eNo) setNullRatifiedStmt

setNullExpired :: MonadIO m => Word64 -> DbAction m Int64
setNullExpired eNo =
  runDbSession (mkDbCallStack "setNullExpired") $
    HsqlSes.statement (fromIntegral eNo) setNullExpiredStmt

setNullDropped :: MonadIO m => Word64 -> DbAction m Int64
setNullDropped eNo =
  runDbSession (mkDbCallStack "setNullDropped") $
    HsqlSes.statement (fromIntegral eNo) setNullDroppedStmt

--------------------------------------------------------------------------------

queryGovActionProposalIdStmt :: HsqlStmt.Statement (Id.TxId, Word64) (Maybe Id.GovActionProposalId)
queryGovActionProposalIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM gov_action_proposal"
          , " WHERE tx_id = $1 AND index = $2"
          ]

    encoder =
      contramap fst (Id.idEncoder Id.getTxId)
        <> contramap snd (HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8))

    decoder = HsqlD.rowMaybe (Id.idDecoder Id.GovActionProposalId)

queryGovActionProposalId :: MonadIO m => Id.TxId -> Word64 -> DbAction m Id.GovActionProposalId
queryGovActionProposalId txId index = do
  let dbCallStack = mkDbCallStack "queryGovActionProposalId"
      errorMsg =
        "GovActionProposal not found with txId: "
          <> Text.pack (show txId)
          <> " and index: "
          <> Text.pack (show index)

  result <- runDbSession dbCallStack $ HsqlSes.statement (txId, index) queryGovActionProposalIdStmt
  case result of
    Just res -> pure res
    Nothing -> throwError $ DbError dbCallStack errorMsg Nothing

--------------------------------------------------------------------------------
-- ParamProposal
--------------------------------------------------------------------------------
insertParamProposalStmt :: HsqlStmt.Statement SGV.ParamProposal Id.ParamProposalId
insertParamProposalStmt =
  insert
    SGV.paramProposalEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ParamProposalId)

insertParamProposal :: MonadIO m => SGV.ParamProposal -> DbAction m Id.ParamProposalId
insertParamProposal paramProposal = do
  runDbSession (mkDbCallStack "insertParamProposal") $
    HsqlSes.statement paramProposal insertParamProposalStmt

--------------------------------------------------------------------------------
-- Treasury
--------------------------------------------------------------------------------
insertTreasuryStmt :: HsqlStmt.Statement SEP.Treasury Id.TreasuryId
insertTreasuryStmt =
  insert
    SEP.treasuryEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TreasuryId)

insertTreasury :: MonadIO m => SEP.Treasury -> DbAction m Id.TreasuryId
insertTreasury treasury = do
  runDbSession (mkDbCallStack "insertTreasury") $ HsqlSes.statement treasury insertTreasuryStmt

--------------------------------------------------------------------------------
insertTreasuryWithdrawalStmt :: HsqlStmt.Statement SGV.TreasuryWithdrawal Id.TreasuryWithdrawalId
insertTreasuryWithdrawalStmt =
  insert
    SGV.treasuryWithdrawalEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TreasuryWithdrawalId)

insertTreasuryWithdrawal :: MonadIO m => SGV.TreasuryWithdrawal -> DbAction m Id.TreasuryWithdrawalId
insertTreasuryWithdrawal treasuryWithdrawal = do
  runDbSession (mkDbCallStack "insertTreasuryWithdrawal") $
    HsqlSes.statement treasuryWithdrawal insertTreasuryWithdrawalStmt

--------------------------------------------------------------------------------
insertBulkTreasuryWithdrawalStmt :: HsqlStmt.Statement [SGV.TreasuryWithdrawal] ()
insertBulkTreasuryWithdrawalStmt =
  insertBulk
    extractTreasuryWithdrawal
    SGV.treasuryWithdrawalBulkEncoder
    NoResultBulk
  where
    extractTreasuryWithdrawal :: [SGV.TreasuryWithdrawal] -> ([Id.GovActionProposalId], [Id.StakeAddressId], [DbLovelace])
    extractTreasuryWithdrawal xs =
      ( map SGV.treasuryWithdrawalGovActionProposalId xs
      , map SGV.treasuryWithdrawalStakeAddressId xs
      , map SGV.treasuryWithdrawalAmount xs
      )

insertBulkTreasuryWithdrawal :: MonadIO m => [SGV.TreasuryWithdrawal] -> DbAction m ()
insertBulkTreasuryWithdrawal treasuryWithdrawals = do
  runDbSession (mkDbCallStack "insertBulkTreasuryWithdrawal") $
    HsqlSes.statement treasuryWithdrawals insertBulkTreasuryWithdrawalStmt

--------------------------------------------------------------------------------
-- Voting
--------------------------------------------------------------------------------

-- | INSERT
insertVotingAnchorStmt :: HsqlStmt.Statement SGV.VotingAnchor Id.VotingAnchorId
insertVotingAnchorStmt =
  insertCheckUnique
    SGV.votingAnchorEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.VotingAnchorId)

insertVotingAnchor :: MonadIO m => SGV.VotingAnchor -> DbAction m Id.VotingAnchorId
insertVotingAnchor votingAnchor = do
  runDbSession (mkDbCallStack "insertVotingAnchor") $
    HsqlSes.statement votingAnchor insertVotingAnchorStmt

insertVotingProcedureStmt :: HsqlStmt.Statement SGV.VotingProcedure Id.VotingProcedureId
insertVotingProcedureStmt =
  insert
    SGV.votingProcedureEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.VotingProcedureId)

insertVotingProcedure :: MonadIO m => SGV.VotingProcedure -> DbAction m Id.VotingProcedureId
insertVotingProcedure votingProcedure = do
  runDbSession (mkDbCallStack "insertVotingProcedure") $
    HsqlSes.statement votingProcedure insertVotingProcedureStmt

-- | QUERY
queryVotingAnchorIdExistsStmt :: HsqlStmt.Statement Id.VotingAnchorId Bool
queryVotingAnchorIdExistsStmt =
  existsById
    (Id.idEncoder Id.getVotingAnchorId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
queryVotingAnchorIdExists votingAnchorId =
  runDbSession (mkDbCallStack "queryVotingAnchorIdExists") $
    HsqlSes.statement votingAnchorId queryVotingAnchorIdExistsStmt
