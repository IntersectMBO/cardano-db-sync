{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import Cardano.Prelude (HasCallStack, Int64, Proxy (..), Word64)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbLookupError (..), mkDbCallStack)
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEP
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), runSession)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Statement.Types (DbInfo (..), validateColumn)
import Cardano.Db.Types (DbLovelace, DbM, hardcodedAlwaysAbstain, hardcodedAlwaysNoConfidence)

--------------------------------------------------------------------------------
-- Committee
--------------------------------------------------------------------------------
insertCommitteeStmt :: HsqlStmt.Statement SGV.Committee Id.CommitteeId
insertCommitteeStmt =
  insert
    SGV.committeeEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeId)

insertCommittee :: HasCallStack => SGV.Committee -> DbM Id.CommitteeId
insertCommittee committee = do
  runSession mkDbCallStack $ HsqlSes.statement committee insertCommitteeStmt

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

queryProposalCommittee :: HasCallStack => Maybe Id.GovActionProposalId -> DbM [Id.CommitteeId]
queryProposalCommittee mgapId =
  runSession mkDbCallStack $
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

insertCommitteeHash :: HasCallStack => SGV.CommitteeHash -> DbM Id.CommitteeHashId
insertCommitteeHash committeeHash = do
  runSession mkDbCallStack $ HsqlSes.statement committeeHash insertCommitteeHashStmt

--------------------------------------------------------------------------------
-- CommitteeMember
--------------------------------------------------------------------------------
insertCommitteeMemberStmt :: HsqlStmt.Statement SGV.CommitteeMember Id.CommitteeMemberId
insertCommitteeMemberStmt =
  insert
    SGV.committeeMemberEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeMemberId)

insertCommitteeMember :: HasCallStack => SGV.CommitteeMember -> DbM Id.CommitteeMemberId
insertCommitteeMember committeeMember = do
  runSession mkDbCallStack $ HsqlSes.statement committeeMember insertCommitteeMemberStmt

insertCommitteeDeRegistrationStmt :: HsqlStmt.Statement SGV.CommitteeDeRegistration Id.CommitteeDeRegistrationId
insertCommitteeDeRegistrationStmt =
  insert
    SGV.committeeDeRegistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeDeRegistrationId)

insertCommitteeDeRegistration :: HasCallStack => SGV.CommitteeDeRegistration -> DbM Id.CommitteeDeRegistrationId
insertCommitteeDeRegistration committeeDeRegistration = do
  runSession mkDbCallStack $
    HsqlSes.statement committeeDeRegistration insertCommitteeDeRegistrationStmt

insertCommitteeRegistrationStmt :: HsqlStmt.Statement SGV.CommitteeRegistration Id.CommitteeRegistrationId
insertCommitteeRegistrationStmt =
  insert
    SGV.committeeRegistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CommitteeRegistrationId)

insertCommitteeRegistration :: HasCallStack => SGV.CommitteeRegistration -> DbM Id.CommitteeRegistrationId
insertCommitteeRegistration committeeRegistration = do
  runSession mkDbCallStack $
    HsqlSes.statement committeeRegistration insertCommitteeRegistrationStmt

--------------------------------------------------------------------------------
-- Constitution
--------------------------------------------------------------------------------
insertConstitutionStmt :: HsqlStmt.Statement SGV.Constitution Id.ConstitutionId
insertConstitutionStmt =
  insert
    SGV.constitutionEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ConstitutionId)

insertConstitution :: HasCallStack => SGV.Constitution -> DbM Id.ConstitutionId
insertConstitution constitution = do
  runSession mkDbCallStack $ HsqlSes.statement constitution insertConstitutionStmt

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

queryProposalConstitution :: HasCallStack => Maybe Id.GovActionProposalId -> DbM [Id.ConstitutionId]
queryProposalConstitution mgapId =
  runSession mkDbCallStack $
    HsqlSes.statement mgapId queryProposalConstitutionStmt

--------------------------------------------------------------------------------
-- DelegationVote
--------------------------------------------------------------------------------
insertDelegationVoteStmt :: HsqlStmt.Statement SGV.DelegationVote Id.DelegationVoteId
insertDelegationVoteStmt =
  insert
    SGV.delegationVoteEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DelegationVoteId)

insertDelegationVote :: HasCallStack => SGV.DelegationVote -> DbM Id.DelegationVoteId
insertDelegationVote delegationVote = do
  runSession mkDbCallStack $ HsqlSes.statement delegationVote insertDelegationVoteStmt

--------------------------------------------------------------------------------
-- Drep
--------------------------------------------------------------------------------

-- | INSERT
insertDrepHashStmt :: HsqlStmt.Statement SGV.DrepHash Id.DrepHashId
insertDrepHashStmt =
  insertCheckUnique
    SGV.drepHashEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DrepHashId)

insertDrepHash :: HasCallStack => SGV.DrepHash -> DbM Id.DrepHashId
insertDrepHash drepHash = do
  runSession mkDbCallStack $ HsqlSes.statement drepHash insertDrepHashStmt

insertDrepHashAbstainStmt :: HsqlStmt.Statement SGV.DrepHash Id.DrepHashId
insertDrepHashAbstainStmt =
  insert
    SGV.drepHashEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.DrepHashId))

insertDrepHashAlwaysAbstain :: HasCallStack => DbM Id.DrepHashId
insertDrepHashAlwaysAbstain = do
  qr <- queryDrepHashAlwaysAbstain
  maybe ins pure qr
  where
    ins =
      runSession mkDbCallStack $
        HsqlSes.statement drepHashAbstain insertDrepHashAbstainStmt

    drepHashAbstain =
      SGV.DrepHash
        { SGV.drepHashRaw = Nothing
        , SGV.drepHashView = hardcodedAlwaysAbstain
        , SGV.drepHashHasScript = False
        }

insertDrepHashAlwaysNoConfidence :: HasCallStack => DbM Id.DrepHashId
insertDrepHashAlwaysNoConfidence = do
  qr <- queryDrepHashAlwaysNoConfidence
  maybe ins pure qr
  where
    ins =
      runSession mkDbCallStack $
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

insertDrepRegistration :: HasCallStack => SGV.DrepRegistration -> DbM Id.DrepRegistrationId
insertDrepRegistration drepRegistration = do
  runSession mkDbCallStack $ HsqlSes.statement drepRegistration insertDrepRegistrationStmt

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

insertBulkDrepDistr :: HasCallStack => [SGV.DrepDistr] -> DbM ()
insertBulkDrepDistr drepDistrs = do
  runSession mkDbCallStack $
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

queryDrepHashAlwaysAbstain :: HasCallStack => DbM (Maybe Id.DrepHashId)
queryDrepHashAlwaysAbstain =
  runSession mkDbCallStack $
    HsqlSes.statement () $
      queryDrepHashSpecialStmt @SGV.DrepHash hardcodedAlwaysAbstain

queryDrepHashAlwaysNoConfidence :: HasCallStack => DbM (Maybe Id.DrepHashId)
queryDrepHashAlwaysNoConfidence =
  runSession mkDbCallStack $
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

insertGovActionProposal :: HasCallStack => SGV.GovActionProposal -> DbM Id.GovActionProposalId
insertGovActionProposal govActionProposal = do
  runSession mkDbCallStack $
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
updateGovActionEnacted :: HasCallStack => Id.GovActionProposalId -> Word64 -> DbM Int64
updateGovActionEnacted gaid eNo =
  runSession mkDbCallStack $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionEnactedStmt

updateGovActionRatified :: HasCallStack => Id.GovActionProposalId -> Word64 -> DbM ()
updateGovActionRatified gaid eNo =
  runSession mkDbCallStack $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionRatifiedStmt

updateGovActionDropped :: HasCallStack => Id.GovActionProposalId -> Word64 -> DbM ()
updateGovActionDropped gaid eNo =
  runSession mkDbCallStack $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionDroppedStmt

updateGovActionExpired :: HasCallStack => Id.GovActionProposalId -> Word64 -> DbM ()
updateGovActionExpired gaid eNo =
  runSession mkDbCallStack $
    HsqlSes.statement (gaid, fromIntegral eNo) updateGovActionExpiredStmt

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

queryGovActionProposalId :: HasCallStack => Id.TxId -> Word64 -> DbM (Either DbLookupError Id.GovActionProposalId)
queryGovActionProposalId txId index = do
  let errorMsg =
        "GovActionProposal not found with txId: "
          <> Text.pack (show txId)
          <> " and index: "
          <> Text.pack (show index)

  result <- runSession mkDbCallStack $ HsqlSes.statement (txId, index) queryGovActionProposalIdStmt
  case result of
    Just res -> pure $ Right res
    Nothing -> pure $ Left $ DbLookupError mkDbCallStack errorMsg

--------------------------------------------------------------------------------
-- ParamProposal
--------------------------------------------------------------------------------
insertParamProposalStmt :: HsqlStmt.Statement SGV.ParamProposal Id.ParamProposalId
insertParamProposalStmt =
  insert
    SGV.paramProposalEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ParamProposalId)

insertParamProposal :: HasCallStack => SGV.ParamProposal -> DbM Id.ParamProposalId
insertParamProposal paramProposal = do
  runSession mkDbCallStack $ HsqlSes.statement paramProposal insertParamProposalStmt

--------------------------------------------------------------------------------
-- Treasury
--------------------------------------------------------------------------------
insertTreasuryStmt :: HsqlStmt.Statement SEP.Treasury Id.TreasuryId
insertTreasuryStmt =
  insert
    SEP.treasuryEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TreasuryId)

insertTreasury :: HasCallStack => SEP.Treasury -> DbM Id.TreasuryId
insertTreasury treasury = do
  runSession mkDbCallStack $ HsqlSes.statement treasury insertTreasuryStmt

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

insertBulkTreasuryWithdrawal :: HasCallStack => [SGV.TreasuryWithdrawal] -> DbM ()
insertBulkTreasuryWithdrawal treasuryWithdrawals = do
  runSession mkDbCallStack $ HsqlSes.statement treasuryWithdrawals insertBulkTreasuryWithdrawalStmt

--------------------------------------------------------------------------------
-- Voting
--------------------------------------------------------------------------------

-- | INSERT
insertVotingAnchorStmt :: HsqlStmt.Statement SGV.VotingAnchor Id.VotingAnchorId
insertVotingAnchorStmt =
  insertCheckUnique
    SGV.votingAnchorEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.VotingAnchorId)

insertVotingAnchor :: HasCallStack => SGV.VotingAnchor -> DbM Id.VotingAnchorId
insertVotingAnchor votingAnchor = do
  runSession mkDbCallStack $ HsqlSes.statement votingAnchor insertVotingAnchorStmt

insertVotingProcedureStmt :: HsqlStmt.Statement SGV.VotingProcedure Id.VotingProcedureId
insertVotingProcedureStmt =
  insert
    SGV.votingProcedureEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.VotingProcedureId)

insertVotingProcedure :: HasCallStack => SGV.VotingProcedure -> DbM Id.VotingProcedureId
insertVotingProcedure votingProcedure = do
  runSession mkDbCallStack $ HsqlSes.statement votingProcedure insertVotingProcedureStmt
