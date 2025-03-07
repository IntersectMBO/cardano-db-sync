{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import qualified Hasql.Decoders as HsqlD

import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as GaV
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction, DbTransMode (..))
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as EaP
import Cardano.Db.Schema.Ids (CommitteeId(..), idDecoder)
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..))
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.Query (queryIdExists)
import Cardano.Prelude (MonadIO)

--------------------------------------------------------------------------------
-- | Committee
--------------------------------------------------------------------------------
insertCommittee :: MonadIO m => GaV.Committee -> DbAction m Id.CommitteeId
insertCommittee committee = runDbT TransWrite $ mkDbTransaction "insertCommittee" $
  insert
    GaV.committeeEncoder
    (WithResult (HsqlD.singleRow $ idDecoder CommitteeId))
    committee

--------------------------------------------------------------------------------
-- | CommitteeHash
--------------------------------------------------------------------------------
insertCommitteeHash :: MonadIO m => GaV.CommitteeHash -> DbAction m Id.CommitteeHashId
insertCommitteeHash committeeHash = runDbT TransWrite $ mkDbTransaction "insertCommitteeHash" $
  insert
    GaV.committeeHashEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeHashId))
    committeeHash

insertCommitteeMember :: MonadIO m => GaV.CommitteeMember -> DbAction m Id.CommitteeMemberId
insertCommitteeMember committeeMember = runDbT TransWrite $ mkDbTransaction "insertCommitteeMember" $
  insert
    GaV.committeeMemberEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeMemberId))
    committeeMember

insertCommitteeDeRegistration :: MonadIO m => GaV.CommitteeDeRegistration -> DbAction m Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration = runDbT TransWrite $ mkDbTransaction "insertCommitteeDeRegistration" $
  insert
    GaV.committeeDeRegistrationEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeDeRegistrationId))
    committeeDeRegistration

insertCommitteeRegistration :: MonadIO m => GaV.CommitteeRegistration -> DbAction m Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration = runDbT TransWrite $ mkDbTransaction "insertCommitteeRegistration" $
  insert
    GaV.committeeRegistrationEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeRegistrationId))
    committeeRegistration

--------------------------------------------------------------------------------
-- | Constitution
--------------------------------------------------------------------------------
insertConstitution :: MonadIO m => GaV.Constitution -> DbAction m Id.ConstitutionId
insertConstitution constitution = runDbT TransWrite $ mkDbTransaction "insertConstitution" $
  insert
    GaV.constitutionEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.ConstitutionId))
    constitution

--------------------------------------------------------------------------------
-- | DelegationVote
--------------------------------------------------------------------------------
insertDelegationVote :: MonadIO m => GaV.DelegationVote -> DbAction m Id.DelegationVoteId
insertDelegationVote delegationVote = runDbT TransWrite $ mkDbTransaction "insertDelegationVote" $
  insert
    GaV.delegationVoteEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.DelegationVoteId))
    delegationVote

--------------------------------------------------------------------------------
-- | Drep
--------------------------------------------------------------------------------
insertDrepHash :: MonadIO m => GaV.DrepHash -> DbAction m Id.DrepHashId
insertDrepHash drepHash = runDbT TransWrite $ mkDbTransaction "insertDrepHash" $
  insert
    GaV.drepHashEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.DrepHashId))
    drepHash

insertDrepRegistration :: MonadIO m => GaV.DrepRegistration -> DbAction m Id.DrepRegistrationId
insertDrepRegistration drepRegistration = runDbT TransWrite $ mkDbTransaction "insertDrepRegistration" $
  insert
    GaV.drepRegistrationEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.DrepRegistrationId))
    drepRegistration

--------------------------------------------------------------------------------
-- | GovActionProposal
--------------------------------------------------------------------------------
insertGovActionProposal :: MonadIO m => GaV.GovActionProposal -> DbAction m Id.GovActionProposalId
insertGovActionProposal govActionProposal = runDbT TransWrite $ mkDbTransaction "insertGovActionProposal" $
  insert
    GaV.govActionProposalEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.GovActionProposalId))
    govActionProposal

--------------------------------------------------------------------------------
-- | ParamProposal
--------------------------------------------------------------------------------
insertParamProposal :: MonadIO m => GaV.ParamProposal -> DbAction m Id.ParamProposalId
insertParamProposal paramProposal = runDbT TransWrite $ mkDbTransaction "insertParamProposal" $
  insert
    GaV.paramProposalEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.ParamProposalId))
    paramProposal

--------------------------------------------------------------------------------
-- | Treasury
--------------------------------------------------------------------------------
insertTreasury :: MonadIO m => EaP.Treasury -> DbAction m Id.TreasuryId
insertTreasury treasury = runDbT TransWrite $ mkDbTransaction "insertTreasury" $
  insert
    EaP.treasuryEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.TreasuryId))
    treasury

insertTreasuryWithdrawal :: MonadIO m => GaV.TreasuryWithdrawal -> DbAction m Id.TreasuryWithdrawalId
insertTreasuryWithdrawal treasuryWithdrawal = runDbT TransWrite $ mkDbTransaction "insertTreasuryWithdrawal" $
  insert
    GaV.treasuryWithdrawalEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.TreasuryWithdrawalId))
    treasuryWithdrawal

--------------------------------------------------------------------------------
-- | Voting
--------------------------------------------------------------------------------
insertVotingAnchor :: MonadIO m => GaV.VotingAnchor -> DbAction m Id.VotingAnchorId
insertVotingAnchor votingAnchor = runDbT TransWrite $ mkDbTransaction "insertVotingAnchor" $
  insert
    GaV.votingAnchorEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.VotingAnchorId))
    votingAnchor

insertVotingProcedure :: MonadIO m => GaV.VotingProcedure -> DbAction m Id.VotingProcedureId
insertVotingProcedure votingProcedure = runDbT TransWrite $ mkDbTransaction "insertVotingProcedure" $
  insert
    GaV.votingProcedureEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.VotingProcedureId))
    votingProcedure

queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
queryVotingAnchorIdExists votingAnchorId = runDbT TransReadOnly $ mkDbTransaction "queryVotingAnchorIdExists" $
  queryIdExists @GaV.VotingAnchor
    (Id.idEncoder Id.getVotingAnchorId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
    votingAnchorId

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
