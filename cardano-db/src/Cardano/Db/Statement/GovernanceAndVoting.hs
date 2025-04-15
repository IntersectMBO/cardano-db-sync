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
import Cardano.Db.Statement.Function.Core (ResultType (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert)
import Cardano.Db.Statement.Function.Query (existsById)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), validateColumn)
import Cardano.Db.Types (DbAction, DbCallInfo (..), hardcodedAlwaysAbstain, hardcodedAlwaysNoConfidence)

--------------------------------------------------------------------------------
-- Committee
--------------------------------------------------------------------------------
insertCommitteeStmt :: HsqlStmt.Statement SGV.Committee (Entity SGV.Committee)
insertCommitteeStmt =
  insert
    SGV.committeeEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeDecoder)

insertCommittee :: MonadIO m => SGV.Committee -> DbAction m Id.CommitteeId
insertCommittee committee = do
  entity <- runDbSession (mkCallInfo "insertCommittee") $ HsqlSes.statement committee insertCommitteeStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "queryProposalCommittee") $
    HsqlSes.statement mgapId queryProposalCommitteeStmt

--------------------------------------------------------------------------------
-- CommitteeHash
--------------------------------------------------------------------------------

-- | Insert
insertCommitteeHashStmt :: HsqlStmt.Statement SGV.CommitteeHash (Entity SGV.CommitteeHash)
insertCommitteeHashStmt =
  insert
    SGV.committeeHashEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeHashDecoder)

insertCommitteeHash :: MonadIO m => SGV.CommitteeHash -> DbAction m Id.CommitteeHashId
insertCommitteeHash committeeHash = do
  entity <- runDbSession (mkCallInfo "insertCommitteeHash") $ HsqlSes.statement committeeHash insertCommitteeHashStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "queryCommitteeHash") $
    HsqlSes.statement hash queryCommitteeHashStmt

--------------------------------------------------------------------------------
-- CommitteeMember
--------------------------------------------------------------------------------
insertCommitteeMemberStmt :: HsqlStmt.Statement SGV.CommitteeMember (Entity SGV.CommitteeMember)
insertCommitteeMemberStmt =
  insert
    SGV.committeeMemberEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeMemberDecoder)

insertCommitteeMember :: MonadIO m => SGV.CommitteeMember -> DbAction m Id.CommitteeMemberId
insertCommitteeMember committeeMember = do
  entity <- runDbSession (mkCallInfo "insertCommitteeMember") $ HsqlSes.statement committeeMember insertCommitteeMemberStmt
  pure $ entityKey entity

insertCommitteeDeRegistrationStmt :: HsqlStmt.Statement SGV.CommitteeDeRegistration (Entity SGV.CommitteeDeRegistration)
insertCommitteeDeRegistrationStmt =
  insert
    SGV.committeeDeRegistrationEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeDeRegistrationDecoder)

insertCommitteeDeRegistration :: MonadIO m => SGV.CommitteeDeRegistration -> DbAction m Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration = do
  entity <-
    runDbSession (mkCallInfo "insertCommitteeDeRegistration") $
      HsqlSes.statement committeeDeRegistration insertCommitteeDeRegistrationStmt
  pure $ entityKey entity

insertCommitteeRegistrationStmt :: HsqlStmt.Statement SGV.CommitteeRegistration (Entity SGV.CommitteeRegistration)
insertCommitteeRegistrationStmt =
  insert
    SGV.committeeRegistrationEncoder
    (WithResult $ HsqlD.singleRow SGV.entityCommitteeRegistrationDecoder)

insertCommitteeRegistration :: MonadIO m => SGV.CommitteeRegistration -> DbAction m Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration = do
  entity <-
    runDbSession (mkCallInfo "insertCommitteeRegistration") $
      HsqlSes.statement committeeRegistration insertCommitteeRegistrationStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Constitution
--------------------------------------------------------------------------------
insertConstitutionStmt :: HsqlStmt.Statement SGV.Constitution (Entity SGV.Constitution)
insertConstitutionStmt =
  insert
    SGV.constitutionEncoder
    (WithResult $ HsqlD.singleRow SGV.entityConstitutionDecoder)

insertConstitution :: MonadIO m => SGV.Constitution -> DbAction m Id.ConstitutionId
insertConstitution constitution = do
  entity <- runDbSession (mkCallInfo "insertConstitution") $ HsqlSes.statement constitution insertConstitutionStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "queryProposalConstitution") $
    HsqlSes.statement mgapId queryProposalConstitutionStmt

--------------------------------------------------------------------------------
-- DelegationVote
--------------------------------------------------------------------------------
insertDelegationVoteStmt :: HsqlStmt.Statement SGV.DelegationVote (Entity SGV.DelegationVote)
insertDelegationVoteStmt =
  insert
    SGV.delegationVoteEncoder
    (WithResult $ HsqlD.singleRow SGV.entityDelegationVoteDecoder)

insertDelegationVote :: MonadIO m => SGV.DelegationVote -> DbAction m Id.DelegationVoteId
insertDelegationVote delegationVote = do
  entity <- runDbSession (mkCallInfo "insertDelegationVote") $ HsqlSes.statement delegationVote insertDelegationVoteStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Drep
--------------------------------------------------------------------------------

-- | INSERT
insertDrepHashStmt :: HsqlStmt.Statement SGV.DrepHash (Entity SGV.DrepHash)
insertDrepHashStmt =
  insert
    SGV.drepHashEncoder
    (WithResult $ HsqlD.singleRow SGV.entityDrepHashDecoder)

insertDrepHash :: MonadIO m => SGV.DrepHash -> DbAction m Id.DrepHashId
insertDrepHash drepHash = do
  entity <- runDbSession (mkCallInfo "insertDrepHash") $ HsqlSes.statement drepHash insertDrepHashStmt
  pure $ entityKey entity

insertDrepHashAbstainStmt :: HsqlStmt.Statement SGV.DrepHash (Entity SGV.DrepHash)
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
          HsqlSes.statement drepHashAbstain insertDrepHashAbstainStmt
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
          HsqlSes.statement drepHashNoConfidence insertDrepHashAbstainStmt
      pure (entityKey entity)

    drepHashNoConfidence =
      SGV.DrepHash
        { SGV.drepHashRaw = Nothing
        , SGV.drepHashView = hardcodedAlwaysNoConfidence
        , SGV.drepHashHasScript = False
        }

insertDrepRegistrationStmt :: HsqlStmt.Statement SGV.DrepRegistration (Entity SGV.DrepRegistration)
insertDrepRegistrationStmt =
  insert
    SGV.drepRegistrationEncoder
    (WithResult $ HsqlD.singleRow SGV.entityDrepRegistrationDecoder)

insertDrepRegistration :: MonadIO m => SGV.DrepRegistration -> DbAction m Id.DrepRegistrationId
insertDrepRegistration drepRegistration = do
  entity <- runDbSession (mkCallInfo "insertDrepRegistration") $ HsqlSes.statement drepRegistration insertDrepRegistrationStmt
  pure $ entityKey entity

-- | QUERY
queryDrepHashSpecialStmt ::
  forall a.
  (DbInfo a) =>
  Text.Text -> -- targetValue
  HsqlStmt.Statement () (Maybe Id.DrepHashId)
queryDrepHashSpecialStmt targetValue =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    rawCol = validateColumn @a "raw"
    viewCol = validateColumn @a "view"
    idCol = validateColumn @a "id"

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , idCol
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
  runDbSession (mkCallInfo "queryDrepHashAlwaysAbstain") $
    HsqlSes.statement () $
      queryDrepHashSpecialStmt @SGV.DrepHash hardcodedAlwaysAbstain

queryDrepHashAlwaysNoConfidence :: MonadIO m => DbAction m (Maybe Id.DrepHashId)
queryDrepHashAlwaysNoConfidence =
  runDbSession (mkCallInfo "queryDrepHashAlwaysNoConfidence") $
    HsqlSes.statement () $
      queryDrepHashSpecialStmt @SGV.DrepHash hardcodedAlwaysNoConfidence

--------------------------------------------------------------------------------
-- GovActionProposal
--------------------------------------------------------------------------------

-- | INSERT
insertGovActionProposalStmt :: HsqlStmt.Statement SGV.GovActionProposal (Entity SGV.GovActionProposal)
insertGovActionProposalStmt =
  insert
    SGV.govActionProposalEncoder
    (WithResult $ HsqlD.singleRow SGV.entityGovActionProposalDecoder)

insertGovActionProposal :: MonadIO m => SGV.GovActionProposal -> DbAction m Id.GovActionProposalId
insertGovActionProposal govActionProposal = do
  entity <-
    runDbSession (mkCallInfo "insertGovActionProposal") $
      HsqlSes.statement govActionProposal insertGovActionProposalStmt
  pure $ entityKey entity

-- | UPDATE

-- Statement for updateGovActionState
updateGovActionStateStmt ::
  -- | Column name to update
  Text.Text ->
  -- | Whether to return affected rows count
  ResultType Int64 r ->
  HsqlStmt.Statement (Id.GovActionProposalId, Int64) r
updateGovActionStateStmt columnName resultType =
  HsqlStmt.Statement sql encoder decoder True
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
          , " RETURNING xmax != 0 AS changed"
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
  runDbSession (mkCallInfo "updateGovActionEnacted") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionEnactedStmt

updateGovActionRatified :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionRatified gaid eNo =
  runDbSession (mkCallInfo "updateGovActionRatified") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionRatifiedStmt

updateGovActionDropped :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionDropped gaid eNo =
  runDbSession (mkCallInfo "updateGovActionDropped") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionDroppedStmt

updateGovActionExpired :: MonadIO m => Id.GovActionProposalId -> Word64 -> DbAction m ()
updateGovActionExpired gaid eNo =
  runDbSession (mkCallInfo "updateGovActionExpired") $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionExpiredStmt

setNullEnacted :: MonadIO m => Word64 -> DbAction m Int64
setNullEnacted eNo =
  runDbSession (mkCallInfo "setNullEnacted") $
    HsqlSes.statement (fromIntegral eNo) setNullEnactedStmt

setNullRatified :: MonadIO m => Word64 -> DbAction m Int64
setNullRatified eNo =
  runDbSession (mkCallInfo "setNullRatified") $
    HsqlSes.statement (fromIntegral eNo) setNullRatifiedStmt

setNullExpired :: MonadIO m => Word64 -> DbAction m Int64
setNullExpired eNo =
  runDbSession (mkCallInfo "setNullExpired") $
    HsqlSes.statement (fromIntegral eNo) setNullExpiredStmt

setNullDropped :: MonadIO m => Word64 -> DbAction m Int64
setNullDropped eNo =
  runDbSession (mkCallInfo "setNullDropped") $
    HsqlSes.statement (fromIntegral eNo) setNullDroppedStmt

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
insertParamProposalStmt :: HsqlStmt.Statement SGV.ParamProposal (Entity SGV.ParamProposal)
insertParamProposalStmt =
  insert
    SGV.paramProposalEncoder
    (WithResult $ HsqlD.singleRow SGV.entityParamProposalDecoder)

insertParamProposal :: MonadIO m => SGV.ParamProposal -> DbAction m Id.ParamProposalId
insertParamProposal paramProposal = do
  entity <-
    runDbSession (mkCallInfo "insertParamProposal") $
      HsqlSes.statement paramProposal insertParamProposalStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Treasury
--------------------------------------------------------------------------------
insertTreasuryStmt :: HsqlStmt.Statement SEP.Treasury (Entity SEP.Treasury)
insertTreasuryStmt =
  insert
    SEP.treasuryEncoder
    (WithResult $ HsqlD.singleRow SEP.entityTreasuryDecoder)

insertTreasury :: MonadIO m => SEP.Treasury -> DbAction m Id.TreasuryId
insertTreasury treasury = do
  entity <- runDbSession (mkCallInfo "insertTreasury") $ HsqlSes.statement treasury insertTreasuryStmt
  pure $ entityKey entity

insertTreasuryWithdrawalStmt :: HsqlStmt.Statement SGV.TreasuryWithdrawal (Entity SGV.TreasuryWithdrawal)
insertTreasuryWithdrawalStmt =
  insert
    SGV.treasuryWithdrawalEncoder
    (WithResult $ HsqlD.singleRow SGV.entityTreasuryWithdrawalDecoder)

insertTreasuryWithdrawal :: MonadIO m => SGV.TreasuryWithdrawal -> DbAction m Id.TreasuryWithdrawalId
insertTreasuryWithdrawal treasuryWithdrawal = do
  entity <-
    runDbSession (mkCallInfo "insertTreasuryWithdrawal") $
      HsqlSes.statement treasuryWithdrawal insertTreasuryWithdrawalStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Voting
--------------------------------------------------------------------------------

-- | INSERT
insertVotingAnchorStmt :: HsqlStmt.Statement SGV.VotingAnchor (Entity SGV.VotingAnchor)
insertVotingAnchorStmt =
  insert
    SGV.votingAnchorEncoder
    (WithResult $ HsqlD.singleRow SGV.entityVotingAnchorDecoder)

insertVotingAnchor :: MonadIO m => SGV.VotingAnchor -> DbAction m Id.VotingAnchorId
insertVotingAnchor votingAnchor = do
  entity <-
    runDbSession (mkCallInfo "insertVotingAnchor") $
      HsqlSes.statement votingAnchor insertVotingAnchorStmt
  pure $ entityKey entity

insertVotingProcedureStmt :: HsqlStmt.Statement SGV.VotingProcedure (Entity SGV.VotingProcedure)
insertVotingProcedureStmt =
  insert
    SGV.votingProcedureEncoder
    (WithResult $ HsqlD.singleRow SGV.entityVotingProcedureDecoder)

insertVotingProcedure :: MonadIO m => SGV.VotingProcedure -> DbAction m Id.VotingProcedureId
insertVotingProcedure votingProcedure = do
  entity <-
    runDbSession (mkCallInfo "insertVotingProcedure") $
      HsqlSes.statement votingProcedure insertVotingProcedureStmt
  pure $ entityKey entity

-- | QUERY
queryVotingAnchorIdExistsStmt :: HsqlStmt.Statement Id.VotingAnchorId Bool
queryVotingAnchorIdExistsStmt =
  existsById
    (Id.idEncoder Id.getVotingAnchorId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
queryVotingAnchorIdExists votingAnchorId =
  runDbSession (mkCallInfo "queryVotingAnchorIdExists") $
    HsqlSes.statement votingAnchorId queryVotingAnchorIdExistsStmt

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
