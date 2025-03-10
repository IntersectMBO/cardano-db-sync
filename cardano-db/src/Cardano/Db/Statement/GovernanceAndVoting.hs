{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import qualified Hasql.Decoders as HsqlD
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Transaction as HsqlT
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Encoders as HsqlE


import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Ids as Id
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEP
import Cardano.Db.Schema.Ids (CommitteeId(..), idDecoder)
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..))
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.Query (queryIdExists)
import Cardano.Db.Statement.Types (DbInfo(..), Entity (..))
import Cardano.Db.Types (DbAction, DbTransMode (..), hardcodedAlwaysAbstain, hardcodedAlwaysNoConfidence)
import Cardano.Prelude (MonadIO, Proxy (..), ByteString)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | Committee
--------------------------------------------------------------------------------
insertCommittee :: MonadIO m => SGV.Committee -> DbAction m Id.CommitteeId
insertCommittee committee = runDbT TransWrite $ mkDbTransaction "insertCommittee" $
  insert
    SGV.committeeEncoder
    (WithResult (HsqlD.singleRow $ idDecoder CommitteeId))
    committee

--------------------------------------------------------------------------------
-- | CommitteeHash
--------------------------------------------------------------------------------

-- Insert
insertCommitteeHash :: MonadIO m => SGV.CommitteeHash -> DbAction m Id.CommitteeHashId
insertCommitteeHash committeeHash = runDbT TransWrite $ mkDbTransaction "insertCommitteeHash" $
  insert
    SGV.committeeHashEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeHashId))
    committeeHash

-- Query
queryCommitteeHash :: MonadIO m => ByteString -> DbAction m (Maybe Id.CommitteeHashId)
queryCommitteeHash hash = runDbT TransReadOnly $ mkDbTransaction "queryCommitteeHash" $ do
  HsqlT.statement hash $ HsqlS.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SGV.CommitteeHash)
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT id FROM " <> table
      , " WHERE raw IS NULL"
      , " LIMIT 1"
      ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.singleRow $ Id.maybeIdDecoder Id.CommitteeHashId

--------------------------------------------------------------------------------
-- | CommitteeMember
--------------------------------------------------------------------------------
insertCommitteeMember :: MonadIO m => SGV.CommitteeMember -> DbAction m Id.CommitteeMemberId
insertCommitteeMember committeeMember = runDbT TransWrite $ mkDbTransaction "insertCommitteeMember" $
  insert
    SGV.committeeMemberEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeMemberId))
    committeeMember

insertCommitteeDeRegistration :: MonadIO m => SGV.CommitteeDeRegistration -> DbAction m Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration = runDbT TransWrite $ mkDbTransaction "insertCommitteeDeRegistration" $
  insert
    SGV.committeeDeRegistrationEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeDeRegistrationId))
    committeeDeRegistration

insertCommitteeRegistration :: MonadIO m => SGV.CommitteeRegistration -> DbAction m Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration = runDbT TransWrite $ mkDbTransaction "insertCommitteeRegistration" $
  insert
    SGV.committeeRegistrationEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.CommitteeRegistrationId))
    committeeRegistration

--------------------------------------------------------------------------------
-- | Constitution
--------------------------------------------------------------------------------
insertConstitution :: MonadIO m => SGV.Constitution -> DbAction m Id.ConstitutionId
insertConstitution constitution = runDbT TransWrite $ mkDbTransaction "insertConstitution" $
  insert
    SGV.constitutionEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.ConstitutionId))
    constitution

--------------------------------------------------------------------------------
-- | DelegationVote
--------------------------------------------------------------------------------
insertDelegationVote :: MonadIO m => SGV.DelegationVote -> DbAction m Id.DelegationVoteId
insertDelegationVote delegationVote = runDbT TransWrite $ mkDbTransaction "insertDelegationVote" $
  insert
    SGV.delegationVoteEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.DelegationVoteId))
    delegationVote

--------------------------------------------------------------------------------
-- | Drep
--------------------------------------------------------------------------------

-- INSERT
insertDrepHash :: MonadIO m => SGV.DrepHash -> DbAction m Id.DrepHashId
insertDrepHash drepHash = runDbT TransWrite $ mkDbTransaction "insertDrepHash" $ do
  entity <- insert
    SGV.drepHashEncoder
    (WithResult (HsqlD.singleRow SGV.entityDrepHashDecoder))
    drepHash
  pure (entityKey entity)

insertAlwaysAbstainDrepHash :: MonadIO m => DbAction m Id.DrepHashId
insertAlwaysAbstainDrepHash = do
  qr <- queryDrepHashAlwaysAbstain
  maybe ins pure qr
  where
    ins = runDbT TransWrite $ mkDbTransaction "insertAlwaysAbstainDrepHash" $ do
      entity <- insert
        SGV.drepHashEncoder
        (WithResult (HsqlD.singleRow SGV.entityDrepHashDecoder))
        SGV.DrepHash
          { SGV.drepHashRaw = Nothing
          , SGV.drepHashView = hardcodedAlwaysAbstain
          , SGV.drepHashHasScript = False
          }
      pure (entityKey entity)

insertDrepRegistration :: MonadIO m => SGV.DrepRegistration -> DbAction m Id.DrepRegistrationId
insertDrepRegistration drepRegistration = runDbT TransWrite $ mkDbTransaction "insertDrepRegistration" $
  insert
    SGV.drepRegistrationEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.DrepRegistrationId))
    drepRegistration

-- QUERY
queryDrepHashAlwaysAbstain :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysAbstain =
  runDbT TransReadOnly $ mkDbTransaction "queryDrepHashAlwaysAbstain" $
    queryDrepHashAlways hardcodedAlwaysAbstain

queryDrepHashAlwaysNoConfidence :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysNoConfidence =
  runDbT TransReadOnly $ mkDbTransaction "queryDrepHashAlwaysNoConfidence" $
    queryDrepHashAlways hardcodedAlwaysNoConfidence

queryDrepHashAlways :: Text.Text -> HsqlT.Transaction (Maybe Id.DrepHashId)
queryDrepHashAlways hardcodedAlways =
  HsqlT.statement () $ HsqlS.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @SGV.DrepHash)
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT id FROM " <> table
      , " WHERE raw IS NULL"
      , " AND view = '" <> hardcodedAlways <> "'"
      , " LIMIT 1"
      ]
    decoder = HsqlD.singleRow $ Id.maybeIdDecoder Id.DrepHashId

--------------------------------------------------------------------------------
-- | GovActionProposal
--------------------------------------------------------------------------------
insertGovActionProposal :: MonadIO m => SGV.GovActionProposal -> DbAction m Id.GovActionProposalId
insertGovActionProposal govActionProposal = runDbT TransWrite $ mkDbTransaction "insertGovActionProposal" $
  insert
    SGV.govActionProposalEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.GovActionProposalId))
    govActionProposal

--------------------------------------------------------------------------------
-- | ParamProposal
--------------------------------------------------------------------------------
insertParamProposal :: MonadIO m => SGV.ParamProposal -> DbAction m Id.ParamProposalId
insertParamProposal paramProposal = runDbT TransWrite $ mkDbTransaction "insertParamProposal" $
  insert
    SGV.paramProposalEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.ParamProposalId))
    paramProposal

--------------------------------------------------------------------------------
-- | Treasury
--------------------------------------------------------------------------------
insertTreasury :: MonadIO m => SEP.Treasury -> DbAction m Id.TreasuryId
insertTreasury treasury = runDbT TransWrite $ mkDbTransaction "insertTreasury" $
  insert
    SEP.treasuryEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.TreasuryId))
    treasury

insertTreasuryWithdrawal :: MonadIO m => SGV.TreasuryWithdrawal -> DbAction m Id.TreasuryWithdrawalId
insertTreasuryWithdrawal treasuryWithdrawal = runDbT TransWrite $ mkDbTransaction "insertTreasuryWithdrawal" $
  insert
    SGV.treasuryWithdrawalEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.TreasuryWithdrawalId))
    treasuryWithdrawal

--------------------------------------------------------------------------------
-- | Voting
--------------------------------------------------------------------------------
insertVotingAnchor :: MonadIO m => SGV.VotingAnchor -> DbAction m Id.VotingAnchorId
insertVotingAnchor votingAnchor = runDbT TransWrite $ mkDbTransaction "insertVotingAnchor" $
  insert
    SGV.votingAnchorEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.VotingAnchorId))
    votingAnchor

insertVotingProcedure :: MonadIO m => SGV.VotingProcedure -> DbAction m Id.VotingProcedureId
insertVotingProcedure votingProcedure = runDbT TransWrite $ mkDbTransaction "insertVotingProcedure" $
  insert
    SGV.votingProcedureEncoder
    (WithResult (HsqlD.singleRow $ idDecoder Id.VotingProcedureId))
    votingProcedure

queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
queryVotingAnchorIdExists votingAnchorId = runDbT TransReadOnly $ mkDbTransaction "queryVotingAnchorIdExists" $
  queryIdExists @SGV.VotingAnchor
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
