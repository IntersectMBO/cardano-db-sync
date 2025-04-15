{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSess
import qualified Hasql.Statement as HsqlStm

import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEP
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.Query (existsById)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (DbAction, hardcodedAlwaysAbstain, hardcodedAlwaysNoConfidence)
import Cardano.Prelude (ByteString, Int64, MonadIO, Proxy (..), Word64)

--------------------------------------------------------------------------------
-- Committee
--------------------------------------------------------------------------------
insertCommitteeStmt :: HsqlStm.Statement SGV.Committee (Entity SGV.Committee)
insertCommitteeStmt =
  insert
    SGV.committeeEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeDecoder)

insertCommittee :: MonadIO m => SGV.Committee -> DbAction m Id.CommitteeId
insertCommittee committee = do
  entity <- runDbSession (mkCallInfo "insertCommittee") $ HsqlSess.statement committee insertCommitteeStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- CommitteeHash
--------------------------------------------------------------------------------

-- | Insert
insertCommitteeHashStmt :: HsqlStm.Statement SGV.CommitteeHash (Entity SGV.CommitteeHash)
insertCommitteeHashStmt =
  insert
    SGV.committeeHashEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeHashDecoder)

insertCommitteeHash :: MonadIO m => SGV.CommitteeHash -> DbAction m Id.CommitteeHashId
insertCommitteeHash committeeHash = do
  entity <- runDbSession (mkCallInfo "insertCommitteeHash") $ HsqlSess.statement committeeHash insertCommitteeHashStmt
  pure $ entityKey entity

-- | Query
queryCommitteeHashStmt :: HsqlStm.Statement ByteString (Maybe Id.CommitteeHashId)
queryCommitteeHashStmt =
  HsqlStm.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SGV.CommitteeHash)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM " <> table
          , " WHERE raw IS NULL"
          , " LIMIT 1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.singleRow $ Id.maybeIdDecoder Id.CommitteeHashId

queryCommitteeHash :: MonadIO m => ByteString -> DbAction m (Maybe Id.CommitteeHashId)
queryCommitteeHash hash =
  runDbSession (mkCallInfo "queryCommitteeHash") $
    HsqlSess.statement hash queryCommitteeHashStmt

--------------------------------------------------------------------------------
-- CommitteeMember
--------------------------------------------------------------------------------
insertCommitteeMemberStmt :: HsqlStm.Statement SGV.CommitteeMember (Entity SGV.CommitteeMember)
insertCommitteeMemberStmt =
  insert
    SGV.committeeMemberEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeMemberDecoder)

insertCommitteeMember :: MonadIO m => SGV.CommitteeMember -> DbAction m Id.CommitteeMemberId
insertCommitteeMember committeeMember = do
  entity <- runDbSession (mkCallInfo "insertCommitteeMember") $ HsqlSess.statement committeeMember insertCommitteeMemberStmt
  pure $ entityKey entity

insertCommitteeDeRegistrationStmt :: HsqlStm.Statement SGV.CommitteeDeRegistration (Entity SGV.CommitteeDeRegistration)
insertCommitteeDeRegistrationStmt =
  insert
    SGV.committeeDeRegistrationEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeDeRegistrationDecoder)

insertCommitteeDeRegistration :: MonadIO m => SGV.CommitteeDeRegistration -> DbAction m Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration = do
  entity <-
    runDbSession (mkCallInfo "insertCommitteeDeRegistration") $
      HsqlSess.statement committeeDeRegistration insertCommitteeDeRegistrationStmt
  pure $ entityKey entity

insertCommitteeRegistrationStmt :: HsqlStm.Statement SGV.CommitteeRegistration (Entity SGV.CommitteeRegistration)
insertCommitteeRegistrationStmt =
  insert
    SGV.committeeRegistrationEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeRegistrationDecoder)

insertCommitteeRegistration :: MonadIO m => SGV.CommitteeRegistration -> DbAction m Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration = do
  entity <-
    runDbSession (mkCallInfo "insertCommitteeRegistration") $
      HsqlSess.statement committeeRegistration insertCommitteeRegistrationStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Constitution
--------------------------------------------------------------------------------
insertConstitutionStmt :: HsqlStm.Statement SGV.Constitution (Entity SGV.Constitution)
insertConstitutionStmt =
  insert
    SGV.constitutionEncoder
    (WithResult $ HsqlD.singleRow SGV.entityConstitutionDecoder)

insertConstitution :: MonadIO m => SGV.Constitution -> DbAction m Id.ConstitutionId
insertConstitution constitution = do
  entity <- runDbSession (mkCallInfo "insertConstitution") $ HsqlSess.statement constitution insertConstitutionStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- DelegationVote
--------------------------------------------------------------------------------
insertDelegationVoteStmt :: HsqlStm.Statement SGV.DelegationVote (Entity SGV.DelegationVote)
insertDelegationVoteStmt =
  insert
    SGV.delegationVoteEncoder
    (WithResult $ HsqlD.singleRow SGV.entityDelegationVoteDecoder)

insertDelegationVote :: MonadIO m => SGV.DelegationVote -> DbAction m Id.DelegationVoteId
insertDelegationVote delegationVote = do
  entity <- runDbSession (mkCallInfo "insertDelegationVote") $ HsqlSess.statement delegationVote insertDelegationVoteStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Drep
--------------------------------------------------------------------------------

-- | INSERT
insertDrepHashStmt :: HsqlStm.Statement SGV.DrepHash (Entity SGV.DrepHash)
insertDrepHashStmt =
  insert
    SGV.drepHashEncoder
    (WithResult $ HsqlD.singleRow SGV.entityDrepHashDecoder)

insertDrepHash :: MonadIO m => SGV.DrepHash -> DbAction m Id.DrepHashId
insertDrepHash drepHash = do
  entity <- runDbSession (mkCallInfo "insertDrepHash") $ HsqlSess.statement drepHash insertDrepHashStmt
  pure $ entityKey entity

insertDrepHashAbstainStmt :: HsqlStm.Statement SGV.DrepHash (Entity SGV.DrepHash)
insertDrepHashAbstainStmt =
  insert
    SGV.drepHashEncoder
    (WithResult (HsqlD.singleRow SGV.entityDrepHashDecoder))

insertDrepHashAlwaysAbstain :: MonadIO m => DbAction m Id.DrepHashId
insertDrepHashAlwaysAbstain = do
  qr <- queryDrepHashAlwaysAbstain
  maybe ins pure qr
  where
    ins = do
      entity <-
        runDbSession (mkCallInfo "insertDrepHashAlwaysAbstain") $
          HsqlSess.statement drepHashAbstain insertDrepHashAbstainStmt
      pure (entityKey entity)

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
    ins = do
      entity <-
        runDbSession (mkCallInfo "insertDrepHashAlwaysNoConfidence") $
          HsqlSess.statement drepHashNoConfidence insertDrepHashAbstainStmt
      pure (entityKey entity)

    drepHashNoConfidence =
      SGV.DrepHash
        { SGV.drepHashRaw = Nothing
        , SGV.drepHashView = hardcodedAlwaysNoConfidence
        , SGV.drepHashHasScript = False
        }

insertDrepRegistrationStmt :: HsqlStm.Statement SGV.DrepRegistration (Entity SGV.DrepRegistration)
insertDrepRegistrationStmt =
  insert
    SGV.drepRegistrationEncoder
    (WithResult $ HsqlD.singleRow SGV.entityDrepRegistrationDecoder)

insertDrepRegistration :: MonadIO m => SGV.DrepRegistration -> DbAction m Id.DrepRegistrationId
insertDrepRegistration drepRegistration = do
  entity <- runDbSession (mkCallInfo "insertDrepRegistration") $ HsqlSess.statement drepRegistration insertDrepRegistrationStmt
  pure $ entityKey entity

-- | QUERY
queryDrepHashAlwaysStmt :: Text.Text -> HsqlStm.Statement () (Maybe Id.DrepHashId)
queryDrepHashAlwaysStmt hardcodedAlways =
  HsqlStm.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @SGV.DrepHash)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM " <> table
          , " WHERE raw IS NULL"
          , " AND view = '" <> hardcodedAlways <> "'"
          , " LIMIT 1"
          ]
    decoder = HsqlD.singleRow $ Id.maybeIdDecoder Id.DrepHashId

queryDrepHashAlwaysAbstainStmt :: HsqlStm.Statement () (Maybe Id.DrepHashId)
queryDrepHashAlwaysAbstainStmt = queryDrepHashAlwaysStmt hardcodedAlwaysAbstain

queryDrepHashAlwaysNoConfidenceStmt :: HsqlStm.Statement () (Maybe Id.DrepHashId)
queryDrepHashAlwaysNoConfidenceStmt = queryDrepHashAlwaysStmt hardcodedAlwaysNoConfidence

queryDrepHashAlwaysAbstain :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysAbstain =
  runDbSession (mkCallInfo "queryDrepHashAlwaysAbstain") $
    HsqlSess.statement () queryDrepHashAlwaysAbstainStmt

queryDrepHashAlwaysNoConfidence :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysNoConfidence =
  runDbSession (mkCallInfo "queryDrepHashAlwaysNoConfidence") $
    HsqlSess.statement () queryDrepHashAlwaysNoConfidenceStmt

--------------------------------------------------------------------------------
-- GovActionProposal
--------------------------------------------------------------------------------

-- | INSERT
insertGovActionProposalStmt :: HsqlStm.Statement SGV.GovActionProposal (Entity SGV.GovActionProposal)
insertGovActionProposalStmt =
  insert
    SGV.govActionProposalEncoder
    (WithResult $ HsqlD.singleRow SGV.entityGovActionProposalDecoder)

insertGovActionProposal :: MonadIO m => SGV.GovActionProposal -> DbAction m Id.GovActionProposalId
insertGovActionProposal govActionProposal = do
  entity <-
    runDbSession (mkCallInfo "insertGovActionProposal") $
      HsqlSess.statement govActionProposal insertGovActionProposalStmt
  pure $ entityKey entity

-- | UPDATE

-- Statement for updateGovActionState
updateGovActionStateStmt ::
  -- | Column name to update
  Text.Text ->
  -- | Whether to return affected rows count
  ResultType Int64 r ->
  HsqlStm.Statement (Id.GovActionProposalId, Int64) r
updateGovActionStateStmt columnName resultType =
  HsqlStm.Statement sql encoder decoder True
  where
    (decoder, returnClause) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec -> (dec, " RETURNING xmax != 0 AS changed")
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
          , returnClause
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
  HsqlStm.Statement Int64 Int64
setGovActionStateNullStmt columnName =
  HsqlStm.Statement sql encoder decoder True
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
          , " RETURNING xmax != 0 AS changed"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowsAffected

-- Statements
updateGovActionEnactedStmt :: HsqlStm.Statement (Id.GovActionProposalId, Int64) Int64
updateGovActionEnactedStmt = updateGovActionStateStmt "enacted_epoch" (WithResult HsqlD.rowsAffected)

updateGovActionRatifiedStmt :: HsqlStm.Statement (Id.GovActionProposalId, Int64) ()
updateGovActionRatifiedStmt = updateGovActionStateStmt "ratified_epoch" NoResult

updateGovActionDroppedStmt :: HsqlStm.Statement (Id.GovActionProposalId, Int64) ()
updateGovActionDroppedStmt = updateGovActionStateStmt "dropped_epoch" NoResult

updateGovActionExpiredStmt :: HsqlStm.Statement (Id.GovActionProposalId, Int64) ()
updateGovActionExpiredStmt = updateGovActionStateStmt "expired_epoch" NoResult

setNullEnactedStmt :: HsqlStm.Statement Int64 Int64
setNullEnactedStmt = setGovActionStateNullStmt "enacted_epoch"

setNullRatifiedStmt :: HsqlStm.Statement Int64 Int64
setNullRatifiedStmt = setGovActionStateNullStmt "ratified_epoch"

setNullExpiredStmt :: HsqlStm.Statement Int64 Int64
setNullExpiredStmt = setGovActionStateNullStmt "expired_epoch"

setNullDroppedStmt :: HsqlStm.Statement Int64 Int64
setNullDroppedStmt = setGovActionStateNullStmt "dropped_epoch"

-- Executions
updateGovActionEnacted :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m Int64
updateGovActionEnacted gaid eNo =
  runDbSession (mkCallInfo "updateGovActionEnacted") $
    HsqlSess.statement (gaid, fromIntegral eNo) updateGovActionEnactedStmt

updateGovActionRatified :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionRatified gaid eNo =
  runDbSession (mkCallInfo "updateGovActionRatified") $
    HsqlSess.statement (gaid, fromIntegral eNo) updateGovActionRatifiedStmt

updateGovActionDropped :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionDropped gaid eNo =
  runDbSession (mkCallInfo "updateGovActionDropped") $
    HsqlSess.statement (gaid, fromIntegral eNo) updateGovActionDroppedStmt

updateGovActionExpired :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionExpired gaid eNo =
  runDbSession (mkCallInfo "updateGovActionExpired") $
    HsqlSess.statement (gaid, fromIntegral eNo) updateGovActionExpiredStmt

setNullEnacted :: MonadIO m => Word64 -> DbAction m Int64
setNullEnacted eNo =
  runDbSession (mkCallInfo "setNullEnacted") $
    HsqlSess.statement (fromIntegral eNo) setNullEnactedStmt

setNullRatified :: MonadIO m => Word64 -> DbAction m Int64
setNullRatified eNo =
  runDbSession (mkCallInfo "setNullRatified") $
    HsqlSess.statement (fromIntegral eNo) setNullRatifiedStmt

setNullExpired :: MonadIO m => Word64 -> DbAction m Int64
setNullExpired eNo =
  runDbSession (mkCallInfo "setNullExpired") $
    HsqlSess.statement (fromIntegral eNo) setNullExpiredStmt

setNullDropped :: MonadIO m => Word64 -> DbAction m Int64
setNullDropped eNo =
  runDbSession (mkCallInfo "setNullDropped") $
    HsqlSess.statement (fromIntegral eNo) setNullDroppedStmt

queryGovActionProposalIdStmt :: HsqlStm.Statement (Id.TxId, Word64) (Maybe Id.GovActionProposalId)
queryGovActionProposalIdStmt =
  HsqlStm.Statement sql encoder decoder True
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
  let callInfo = mkCallInfo "queryGovActionProposalId"
      errorMsg =
        "GovActionProposal not found with txId: "
          <> Text.pack (show txId)
          <> " and index: "
          <> Text.pack (show index)

  result <- runDbSession callInfo $ HsqlSes.statement (txId, index) queryGovActionProposalIdStmt
  case result of
    Just res -> pure res
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing

--------------------------------------------------------------------------------
-- ParamProposal
--------------------------------------------------------------------------------
insertParamProposalStmt :: HsqlStm.Statement SGV.ParamProposal (Entity SGV.ParamProposal)
insertParamProposalStmt =
  insert
    SGV.paramProposalEncoder
    (WithResult $ HsqlD.singleRow SGV.entityParamProposalDecoder)

insertParamProposal :: MonadIO m => SGV.ParamProposal -> DbAction m Id.ParamProposalId
insertParamProposal paramProposal = do
  entity <-
    runDbSession (mkCallInfo "insertParamProposal") $
      HsqlSess.statement paramProposal insertParamProposalStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Treasury
--------------------------------------------------------------------------------
insertTreasuryStmt :: HsqlStm.Statement SEP.Treasury (Entity SEP.Treasury)
insertTreasuryStmt =
  insert
    SEP.treasuryEncoder
    (WithResult $ HsqlD.singleRow SEP.entityTreasuryDecoder)

insertTreasury :: MonadIO m => SEP.Treasury -> DbAction m Id.TreasuryId
insertTreasury treasury = do
  entity <- runDbSession (mkCallInfo "insertTreasury") $ HsqlSess.statement treasury insertTreasuryStmt
  pure $ entityKey entity

insertTreasuryWithdrawalStmt :: HsqlStm.Statement SGV.TreasuryWithdrawal (Entity SGV.TreasuryWithdrawal)
insertTreasuryWithdrawalStmt =
  insert
    SGV.treasuryWithdrawalEncoder
    (WithResult $ HsqlD.singleRow SGV.entityTreasuryWithdrawalDecoder)

insertTreasuryWithdrawal :: MonadIO m => SGV.TreasuryWithdrawal -> DbAction m Id.TreasuryWithdrawalId
insertTreasuryWithdrawal treasuryWithdrawal = do
  entity <-
    runDbSession (mkCallInfo "insertTreasuryWithdrawal") $
      HsqlSess.statement treasuryWithdrawal insertTreasuryWithdrawalStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Voting
--------------------------------------------------------------------------------

-- | INSERT
insertVotingAnchorStmt :: HsqlStm.Statement SGV.VotingAnchor (Entity SGV.VotingAnchor)
insertVotingAnchorStmt =
  insert
    SGV.votingAnchorEncoder
    (WithResult $ HsqlD.singleRow SGV.entityVotingAnchorDecoder)

insertVotingAnchor :: MonadIO m => SGV.VotingAnchor -> DbAction m Id.VotingAnchorId
insertVotingAnchor votingAnchor = do
  entity <-
    runDbSession (mkCallInfo "insertVotingAnchor") $
      HsqlSess.statement votingAnchor insertVotingAnchorStmt
  pure $ entityKey entity

insertVotingProcedureStmt :: HsqlStm.Statement SGV.VotingProcedure (Entity SGV.VotingProcedure)
insertVotingProcedureStmt =
  insert
    SGV.votingProcedureEncoder
    (WithResult $ HsqlD.singleRow SGV.entityVotingProcedureDecoder)

insertVotingProcedure :: MonadIO m => SGV.VotingProcedure -> DbAction m Id.VotingProcedureId
insertVotingProcedure votingProcedure = do
  entity <-
    runDbSession (mkCallInfo "insertVotingProcedure") $
      HsqlSess.statement votingProcedure insertVotingProcedureStmt
  pure $ entityKey entity

-- | QUERY
queryVotingAnchorIdStmt :: HsqlStm.Statement Id.VotingAnchorId Bool
queryVotingAnchorIdStmt =
  existsById
    (Id.idEncoder Id.getVotingAnchorId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
queryVotingAnchorIdExists votingAnchorId =
  runDbSession (mkCallInfo "queryVotingAnchorIdExists") $
    HsqlSess.statement votingAnchorId queryVotingAnchorIdStmt

-- These tables manage governance-related data, including DReps, committees, and voting procedures.

-- committee
-- committee_de_registration
-- committee_hash
-- committee_member
-- committee_registration
-- constitution
-- delegation_vote
-- drep_distr
-- drep_hash
-- drep_registration
-- event_info
-- gov_action_proposal
-- new_committee
-- param_proposal
-- treasury_withdrawal
-- voting_anchor
-- voting_procedure
