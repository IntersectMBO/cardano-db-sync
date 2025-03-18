{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT

import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Ids as Id
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEP
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..))
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.Query (queryIdExists)
import Cardano.Db.Statement.Types (DbInfo(..), Entity (..))
import Cardano.Db.Types (DbAction, DbTransMode (..), hardcodedAlwaysAbstain, hardcodedAlwaysNoConfidence)
import Cardano.Prelude (MonadIO, Proxy (..), ByteString, Word64, Int64)

--------------------------------------------------------------------------------
-- | Committee
--------------------------------------------------------------------------------
insertCommittee :: MonadIO m => SGV.Committee -> DbAction m Id.CommitteeId
insertCommittee committee =
  runDbT TransWrite $ mkDbTransaction "insertCommittee" $ do
    entity <- insert
      SGV.committeeEncoder
      (WithResult $ HsqlD.singleRow SGV.entityCommitteeDecoder)
      committee
    pure (entityKey entity)

--------------------------------------------------------------------------------
-- | CommitteeHash
--------------------------------------------------------------------------------

-- Insert
insertCommitteeHash :: MonadIO m => SGV.CommitteeHash -> DbAction m Id.CommitteeHashId
insertCommitteeHash committeeHash =
  runDbT TransWrite $ mkDbTransaction "insertCommitteeHash" $ do
    entity <- insert
      SGV.committeeHashEncoder
      (WithResult (HsqlD.singleRow SGV.entityCommitteeHashDecoder))
      committeeHash
    pure (entityKey entity)

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
insertCommitteeMember committeeMember =
  runDbT TransWrite $ mkDbTransaction "insertCommitteeMember" $ do
    entity <- insert
      SGV.committeeMemberEncoder
      (WithResult (HsqlD.singleRow SGV.entityCommitteeMemberDecoder))
      committeeMember
    pure (entityKey entity)

insertCommitteeDeRegistration :: MonadIO m => SGV.CommitteeDeRegistration -> DbAction m Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration =
  runDbT TransWrite $ mkDbTransaction "insertCommitteeDeRegistration" $ do
    entity <- insert
      SGV.committeeDeRegistrationEncoder
      (WithResult (HsqlD.singleRow SGV.entityCommitteeDeRegistrationDecoder))
      committeeDeRegistration
    pure (entityKey entity)

insertCommitteeRegistration :: MonadIO m => SGV.CommitteeRegistration -> DbAction m Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration =
  runDbT TransWrite $ mkDbTransaction "insertCommitteeRegistration" $ do
    entity <- insert
      SGV.committeeRegistrationEncoder
      (WithResult (HsqlD.singleRow SGV.entityCommitteeRegistrationDecoder))
      committeeRegistration
    pure (entityKey entity)
--------------------------------------------------------------------------------
-- | Constitution
--------------------------------------------------------------------------------
insertConstitution :: MonadIO m => SGV.Constitution -> DbAction m Id.ConstitutionId
insertConstitution constitution =
  runDbT TransWrite $ mkDbTransaction "insertConstitution" $ do
    entity <- insert
      SGV.constitutionEncoder
      (WithResult (HsqlD.singleRow SGV.entityConstitutionDecoder))
      constitution
    pure (entityKey entity)

--------------------------------------------------------------------------------
-- | DelegationVote
--------------------------------------------------------------------------------
insertDelegationVote :: MonadIO m => SGV.DelegationVote -> DbAction m Id.DelegationVoteId
insertDelegationVote delegationVote =
  runDbT TransWrite $ mkDbTransaction "insertDelegationVote" $ do
    entity <- insert
      SGV.delegationVoteEncoder
      (WithResult (HsqlD.singleRow SGV.entityDelegationVoteDecoder))
      delegationVote
    pure (entityKey entity)
--------------------------------------------------------------------------------
-- | Drep
--------------------------------------------------------------------------------

-- INSERT
insertDrepHash :: MonadIO m => SGV.DrepHash -> DbAction m Id.DrepHashId
insertDrepHash drepHash =
  runDbT TransWrite $ mkDbTransaction "insertDrepHash" $ do
    entity <- insert
      SGV.drepHashEncoder
      (WithResult (HsqlD.singleRow SGV.entityDrepHashDecoder))
      drepHash
    pure (entityKey entity)

insertDrepHashAlwaysAbstain :: MonadIO m => DbAction m Id.DrepHashId
insertDrepHashAlwaysAbstain = do
  qr <- queryDrepHashAlwaysAbstain
  maybe ins pure qr
  where
    ins = runDbT TransWrite $ mkDbTransaction "insertDrepHashAlwaysAbstain" $ do
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
insertDrepRegistration drepRegistration =
  runDbT TransWrite $ mkDbTransaction "insertDrepRegistration" $ do
    entity <- insert
      SGV.drepRegistrationEncoder
      (WithResult (HsqlD.singleRow SGV.entityDrepRegistrationDecoder))
      drepRegistration
    pure (entityKey entity)

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

-- | INSERT
insertGovActionProposal :: MonadIO m => SGV.GovActionProposal -> DbAction m Id.GovActionProposalId
insertGovActionProposal govActionProposal =
  runDbT TransWrite $ mkDbTransaction "insertGovActionProposal" $ do
    entity <- insert
      SGV.govActionProposalEncoder
      (WithResult (HsqlD.singleRow SGV.entityGovActionProposalDecoder))
      govActionProposal
    pure (entityKey entity)

-- | UPDATE
updateGovActionEnacted :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m Int64
updateGovActionEnacted gaid eNo = runDbT TransWrite $ mkDbTransaction "updateGovActionEnacted" $
    updateGovActionStateTransaction gaid eNo "enacted_epoch" (WithResult HsqlD.rowsAffected)

updateGovActionRatified :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionRatified gaid eNo = runDbT TransWrite $ mkDbTransaction "updateGovActionRatified" $
  updateGovActionStateTransaction gaid eNo "ratified_epoch" NoResult

updateGovActionDropped :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionDropped gaid eNo = runDbT TransWrite $ mkDbTransaction "updateGovActionDropped" $
  updateGovActionStateTransaction gaid eNo "dropped_epoch" NoResult

updateGovActionExpired :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionExpired gaid eNo = runDbT TransWrite $ mkDbTransaction "updateGovActionExpired" $
  updateGovActionStateTransaction gaid eNo "expired_epoch" NoResult

setNullEnacted :: MonadIO m => Word64 -> DbAction m Int64
setNullEnacted eNo =  runDbT TransWrite $ mkDbTransaction "setNullEnacted" $
  setGovActionStateNullTransaction eNo "enacted_epoch"

setNullRatified :: MonadIO m => Word64 -> DbAction m Int64
setNullRatified eNo = runDbT TransWrite $ mkDbTransaction "setNullRatified" $
  setGovActionStateNullTransaction eNo "ratified_epoch"

setNullExpired :: MonadIO m => Word64 -> DbAction m Int64
setNullExpired eNo = runDbT TransWrite $ mkDbTransaction "setNullExpired" $
  setGovActionStateNullTransaction eNo "expired_epoch"

setNullDropped :: MonadIO m => Word64 -> DbAction m Int64
setNullDropped eNo = runDbT TransWrite $ mkDbTransaction "setNullDropped" $
  setGovActionStateNullTransaction eNo "dropped_epoch"

updateGovActionStateTransaction
  :: forall r.
     Id.GovActionProposalId  -- ^ ID of the proposal to update
  -> Word64                  -- ^ Epoch number
  -> Text.Text               -- ^ Column name to update
  -> ResultType Int64 r      -- ^ Whether to return affected rows count
  -> HsqlT.Transaction r     -- ^ Transaction result
updateGovActionStateTransaction gaid eNo columnName resultType = do
    let params = (gaid, fromIntegral eNo :: Int64)
    HsqlT.statement params $ HsqlS.Statement sql encoder decoder True
  where
    (decoder, returnClause) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec -> (dec, " RETURNING xmax != 0 AS changed")
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "UPDATE gov_action_proposal"
      , " SET ", columnName, " = $2"
      , " WHERE id = $1 AND ", columnName, " IS NULL"
      , returnClause
      ]

    encoder = mconcat
      [ fst >$< Id.idEncoder Id.getGovActionProposalId
      , snd >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
      ]

setGovActionStateNullTransaction
  :: Word64              -- ^ Epoch number
  -> Text.Text           -- ^ Column name to update
  -> HsqlT.Transaction Int64    -- ^ Number of rows affected
setGovActionStateNullTransaction eNo columnName = do
    let param = fromIntegral eNo :: Int64
    HsqlT.statement param $ HsqlS.Statement sql encoder decoder True
  where
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "UPDATE gov_action_proposal"
      , " SET ", columnName, " = NULL"
      , " WHERE ", columnName, " IS NOT NULL AND ", columnName, " > $1"
      , " RETURNING xmax != 0 AS changed" -- xmax trick to count affected rows
      ]

    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowsAffected

--------------------------------------------------------------------------------
-- | ParamProposal
--------------------------------------------------------------------------------
insertParamProposal :: MonadIO m => SGV.ParamProposal -> DbAction m Id.ParamProposalId
insertParamProposal paramProposal =
  runDbT TransWrite $ mkDbTransaction "insertParamProposal" $ do
    entity <- insert
      SGV.paramProposalEncoder
      (WithResult (HsqlD.singleRow SGV.entityParamProposalDecoder))
      paramProposal
    pure (entityKey entity)

--------------------------------------------------------------------------------
-- | Treasury
--------------------------------------------------------------------------------
insertTreasury :: MonadIO m => SEP.Treasury -> DbAction m Id.TreasuryId
insertTreasury treasury =
  runDbT TransWrite $ mkDbTransaction "insertTreasury" $ do
    entity <- insert
      SEP.treasuryEncoder
      (WithResult (HsqlD.singleRow SEP.entityTreasuryDecoder))
      treasury
    pure (entityKey entity)

insertTreasuryWithdrawal :: MonadIO m => SGV.TreasuryWithdrawal -> DbAction m Id.TreasuryWithdrawalId
insertTreasuryWithdrawal treasuryWithdrawal =
  runDbT TransWrite $ mkDbTransaction "insertTreasuryWithdrawal" $ do
    entity <- insert
      SGV.treasuryWithdrawalEncoder
      (WithResult (HsqlD.singleRow SGV.entityTreasuryWithdrawalDecoder))
      treasuryWithdrawal
    pure (entityKey entity)

--------------------------------------------------------------------------------
-- | Voting
--------------------------------------------------------------------------------
insertVotingAnchor :: MonadIO m => SGV.VotingAnchor -> DbAction m Id.VotingAnchorId
insertVotingAnchor votingAnchor = runDbT TransWrite $ mkDbTransaction "insertVotingAnchor" $ do
  entity <- insert
    SGV.votingAnchorEncoder
    (WithResult (HsqlD.singleRow SGV.entityVotingAnchorDecoder))
    votingAnchor
  pure (entityKey entity)

insertVotingProcedure :: MonadIO m => SGV.VotingProcedure -> DbAction m Id.VotingProcedureId
insertVotingProcedure votingProcedure = runDbT TransWrite $ mkDbTransaction "insertVotingProcedure" $ do
  entity <- insert
    SGV.votingProcedureEncoder
    (WithResult (HsqlD.singleRow SGV.entityVotingProcedureDecoder))
    votingProcedure
  pure (entityKey entity)

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
