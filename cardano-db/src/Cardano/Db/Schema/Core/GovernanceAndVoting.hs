{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Schema.Core.GovernanceAndVoting where

import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (
  AnchorType,
  DbLovelace,
  DbWord64,
  GovActionType,
  Vote,
  VoteUrl,
  VoterRole,
  anchorTypeDecoder,
  anchorTypeEncoder,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  govActionTypeDecoder,
  govActionTypeEncoder,
  maybeDbLovelaceDecoder,
  maybeDbLovelaceEncoder,
  maybeDbWord64Decoder,
  maybeDbWord64Encoder,
  voteDecoder,
  voteEncoder,
  voteUrlDecoder,
  voteUrlEncoder,
  voterRoleDecoder,
  voterRoleEncoder,
 )

-----------------------------------------------------------------------------------------------------------------------------------
-- GOVERNANCE AND VOTING
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: drep_hash
-- Description: Stores hashes of DRep (Decentralized Reputation) records, which are used in governance processes.
-----------------------------------------------------------------------------------------------------------------------------------
data DrepHash = DrepHash
  { drepHashRaw :: !(Maybe ByteString) -- sqltype=hash28type
  , drepHashView :: !Text
  , drepHashHasScript :: !Bool
  }
  deriving (Eq, Show, Generic)

type instance Key DrepHash = Id.DrepHashId
instance DbInfo DrepHash where
  uniqueFields _ = ["raw", "has_script"]

entityDrepHashDecoder :: D.Row (Entity DrepHash)
entityDrepHashDecoder =
  Entity
    <$> Id.idDecoder Id.DrepHashId -- entityKey
    <*> drepHashDecoder -- entityVal

drepHashDecoder :: D.Row DrepHash
drepHashDecoder =
  DrepHash
    <$> D.column (D.nullable D.bytea) -- drepHashRaw
    <*> D.column (D.nonNullable D.text) -- drepHashView
    <*> D.column (D.nonNullable D.bool) -- drepHashHasScript

entityDrepHashEncoder :: E.Params (Entity DrepHash)
entityDrepHashEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getDrepHashId
    , entityVal >$< drepHashEncoder
    ]

drepHashEncoder :: E.Params DrepHash
drepHashEncoder =
  mconcat
    [ drepHashRaw >$< E.param (E.nullable E.bytea)
    , drepHashView >$< E.param (E.nonNullable E.text)
    , drepHashHasScript >$< E.param (E.nonNullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: drep_registration
-- Description: Contains details about the registration of DReps, including their public keys and other identifying information.
-----------------------------------------------------------------------------------------------------------------------------------
data DrepRegistration = DrepRegistration
  { drepRegistrationTxId :: !Id.TxId -- noreference
  , drepRegistrationCertIndex :: !Word16
  , drepRegistrationDeposit :: !(Maybe Int64)
  , drepRegistrationDrepHashId :: !Id.DrepHashId -- noreference
  , drepRegistrationVotingAnchorId :: !(Maybe Id.VotingAnchorId) -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key DrepRegistration = Id.DrepRegistrationId
instance DbInfo DrepRegistration

entityDrepRegistrationDecoder :: D.Row (Entity DrepRegistration)
entityDrepRegistrationDecoder =
  Entity
    <$> Id.idDecoder Id.DrepRegistrationId -- entityKey
    <*> drepRegistrationDecoder -- entityVal

drepRegistrationDecoder :: D.Row DrepRegistration
drepRegistrationDecoder =
  DrepRegistration
    <$> Id.idDecoder Id.TxId -- drepRegistrationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- drepRegistrationCertIndex
    <*> D.column (D.nullable D.int8) -- drepRegistrationDeposit
    <*> Id.idDecoder Id.DrepHashId -- drepRegistrationId.DrepHashId
    <*> Id.maybeIdDecoder Id.VotingAnchorId -- drepRegistrationVotingAnchorId

entityDrepRegistrationEncoder :: E.Params (Entity DrepRegistration)
entityDrepRegistrationEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getDrepRegistrationId
    , entityVal >$< drepRegistrationEncoder
    ]

drepRegistrationEncoder :: E.Params DrepRegistration
drepRegistrationEncoder =
  mconcat
    [ drepRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , drepRegistrationDeposit >$< E.param (E.nullable E.int8)
    , drepRegistrationDrepHashId >$< Id.idEncoder Id.getDrepHashId
    , drepRegistrationVotingAnchorId >$< Id.maybeIdEncoder Id.getVotingAnchorId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: drep_distr
-- Description: Contains information about the distribution of DRep tokens, including the amount distributed and the epoch in which the distribution occurred.
-----------------------------------------------------------------------------------------------------------------------------------
data DrepDistr = DrepDistr
  { drepDistrHashId :: !Id.DrepHashId -- noreference
  , drepDistrAmount :: !Word64
  , drepDistrEpochNo :: !Word64 -- sqltype=word31type
  , drepDistrActiveUntil :: !(Maybe Word64) -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key DrepDistr = Id.DrepDistrId
instance DbInfo DrepDistr where
  uniqueFields _ = ["hash_id", "epoch_no"]

entityDrepDistrDecoder :: D.Row (Entity DrepDistr)
entityDrepDistrDecoder =
  Entity
    <$> Id.idDecoder Id.DrepDistrId -- entityKey
    <*> drepDistrDecoder -- entityVal

drepDistrDecoder :: D.Row DrepDistr
drepDistrDecoder =
  DrepDistr
    <$> Id.idDecoder Id.DrepHashId -- drepDistrHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- drepDistrAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- drepDistrEpochNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- drepDistrActiveUntil

entityDrepDistrEncoder :: E.Params (Entity DrepDistr)
entityDrepDistrEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getDrepDistrId
    , entityVal >$< drepDistrEncoder
    ]

drepDistrEncoder :: E.Params DrepDistr
drepDistrEncoder =
  mconcat
    [ drepDistrHashId >$< Id.idEncoder Id.getDrepHashId
    , drepDistrAmount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , drepDistrEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , drepDistrActiveUntil >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: delegation_vote
-- Description: Tracks votes cast by stakeholders to delegate their voting rights to other entities within the governance framework.
-----------------------------------------------------------------------------------------------------------------------------------
data DelegationVote = DelegationVote
  { delegationVoteAddrId :: !Id.StakeAddressId -- noreference
  , delegationVoteCertIndex :: !Word16
  , delegationVoteDrepHashId :: !Id.DrepHashId -- noreference
  , delegationVoteTxId :: !Id.TxId -- noreference
  , delegationVoteRedeemerId :: !(Maybe Id.RedeemerId) -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key DelegationVote = Id.DelegationVoteId
instance DbInfo DelegationVote

entityDelegationVoteDecoder :: D.Row (Entity DelegationVote)
entityDelegationVoteDecoder =
  Entity
    <$> Id.idDecoder Id.DelegationVoteId -- entityKey
    <*> delegationVoteDecoder -- entityVal

delegationVoteDecoder :: D.Row DelegationVote
delegationVoteDecoder =
  DelegationVote
    <$> Id.idDecoder Id.StakeAddressId -- delegationVoteAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- delegationVoteCertIndex
    <*> Id.idDecoder Id.DrepHashId -- delegationVoteId.DrepHashId
    <*> Id.idDecoder Id.TxId -- delegationVoteTxId
    <*> Id.maybeIdDecoder Id.RedeemerId -- delegationVoteRedeemerId

entityDelegationVoteEncoder :: E.Params (Entity DelegationVote)
entityDelegationVoteEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getDelegationVoteId
    , entityVal >$< delegationVoteEncoder
    ]

delegationVoteEncoder :: E.Params DelegationVote
delegationVoteEncoder =
  mconcat
    [ delegationVoteAddrId >$< Id.idEncoder Id.getStakeAddressId
    , delegationVoteCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegationVoteDrepHashId >$< Id.idEncoder Id.getDrepHashId
    , delegationVoteTxId >$< Id.idEncoder Id.getTxId
    , delegationVoteRedeemerId >$< Id.maybeIdEncoder Id.getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: gov_action_proposal
-- Description: Contains proposals for governance actions, including the type of action, the amount of the deposit, and the expiration date.
-----------------------------------------------------------------------------------------------------------------------------------
data GovActionProposal = GovActionProposal
  { govActionProposalTxId :: !Id.TxId -- noreference
  , govActionProposalIndex :: !Word64
  , govActionProposalPrevGovActionProposal :: !(Maybe Id.GovActionProposalId) -- noreference
  , govActionProposalDeposit :: !DbLovelace -- sqltype=lovelace
  , govActionProposalReturnAddress :: !Id.StakeAddressId -- noreference
  , govActionProposalExpiration :: !(Maybe Word64) -- sqltype=word31type
  , govActionProposalVotingAnchorId :: !(Maybe Id.VotingAnchorId) -- noreference
  , govActionProposalType :: !GovActionType -- sqltype=govactiontype
  , govActionProposalDescription :: !Text -- sqltype=jsonb
  , govActionProposalParamProposal :: !(Maybe Id.ParamProposalId) -- noreference
  , govActionProposalRatifiedEpoch :: !(Maybe Word64) -- sqltype=word31type
  , govActionProposalEnactedEpoch :: !(Maybe Word64) -- sqltype=word31type
  , govActionProposalDroppedEpoch :: !(Maybe Word64) -- sqltype=word31type
  , govActionProposalExpiredEpoch :: !(Maybe Word64) -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key GovActionProposal = Id.GovActionProposalId
instance DbInfo GovActionProposal

entityGovActionProposalDecoder :: D.Row (Entity GovActionProposal)
entityGovActionProposalDecoder =
  Entity
    <$> Id.idDecoder Id.GovActionProposalId -- entityKey
    <*> govActionProposalDecoder -- entityVal

govActionProposalDecoder :: D.Row GovActionProposal
govActionProposalDecoder =
  GovActionProposal
    <$> Id.idDecoder Id.TxId -- govActionProposalTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- govActionProposalIndex
    <*> Id.maybeIdDecoder Id.GovActionProposalId -- govActionProposalPrevGovActionProposal
    <*> dbLovelaceDecoder -- govActionProposalDeposit
    <*> Id.idDecoder Id.StakeAddressId -- govActionProposalReturnAddress
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalExpiration
    <*> Id.maybeIdDecoder Id.VotingAnchorId -- govActionProposalVotingAnchorId
    <*> D.column (D.nonNullable govActionTypeDecoder) -- govActionProposalType
    <*> D.column (D.nonNullable D.text) -- govActionProposalDescription
    <*> Id.maybeIdDecoder Id.ParamProposalId -- govActionProposalParamProposal
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalRatifiedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalEnactedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalDroppedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalExpiredEpoch

entityGovActionProposalEncoder :: E.Params (Entity GovActionProposal)
entityGovActionProposalEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getGovActionProposalId
    , entityVal >$< govActionProposalEncoder
    ]

govActionProposalEncoder :: E.Params GovActionProposal
govActionProposalEncoder =
  mconcat
    [ govActionProposalTxId >$< Id.idEncoder Id.getTxId
    , govActionProposalIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , govActionProposalPrevGovActionProposal >$< Id.maybeIdEncoder Id.getGovActionProposalId
    , govActionProposalDeposit >$< dbLovelaceEncoder
    , govActionProposalReturnAddress >$< Id.idEncoder Id.getStakeAddressId
    , govActionProposalExpiration >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalVotingAnchorId >$< Id.maybeIdEncoder Id.getVotingAnchorId
    , govActionProposalType >$< E.param (E.nonNullable govActionTypeEncoder)
    , govActionProposalDescription >$< E.param (E.nonNullable E.text)
    , govActionProposalParamProposal >$< Id.maybeIdEncoder Id.getParamProposalId
    , govActionProposalRatifiedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalEnactedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalDroppedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalExpiredEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: voting_procedure
-- Description: Defines the procedures and rules governing the voting process, including quorum requirements and tallying mechanisms.
-----------------------------------------------------------------------------------------------------------------------------------
data VotingProcedure = VotingProcedure
  { votingProcedureTxId :: !Id.TxId -- noreference
  , votingProcedureIndex :: !Word16
  , votingProcedureGovActionProposalId :: !Id.GovActionProposalId -- noreference
  , votingProcedureVoterRole :: !VoterRole -- sqltype=voterrole
  , votingProcedureDrepVoter :: !(Maybe Id.DrepHashId) -- noreference
  , votingProcedurePoolVoter :: !(Maybe Id.PoolHashId) -- noreference
  , votingProcedureVote :: !Vote -- sqltype=vote
  , votingProcedureVotingAnchorId :: !(Maybe Id.VotingAnchorId) -- noreference
  , votingProcedureCommitteeVoter :: !(Maybe Id.CommitteeHashId) -- noreference
  , votingProcedureInvalid :: !(Maybe Id.EventInfoId) -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key VotingProcedure = Id.VotingProcedureId
instance DbInfo VotingProcedure

entityVotingProcedureDecoder :: D.Row (Entity VotingProcedure)
entityVotingProcedureDecoder =
  Entity
    <$> Id.idDecoder Id.VotingProcedureId -- entityKey
    <*> votingProcedureDecoder -- entityVal

votingProcedureDecoder :: D.Row VotingProcedure
votingProcedureDecoder =
  VotingProcedure
    <$> Id.idDecoder Id.TxId -- votingProcedureTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- votingProcedureIndex
    <*> Id.idDecoder Id.GovActionProposalId -- votingProcedureGovActionProposalId
    <*> D.column (D.nonNullable voterRoleDecoder) -- votingProcedureVoterRole
    <*> Id.maybeIdDecoder Id.DrepHashId -- votingProcedureDrepVoter
    <*> Id.maybeIdDecoder Id.PoolHashId -- votingProcedurePoolVoter
    <*> D.column (D.nonNullable voteDecoder) -- votingProcedureVote
    <*> Id.maybeIdDecoder Id.VotingAnchorId -- votingProcedureVotingAnchorId
    <*> Id.maybeIdDecoder Id.CommitteeHashId -- votingProcedureCommitteeVoter
    <*> Id.maybeIdDecoder Id.EventInfoId -- votingProcedureInvalid

entityVotingProcedureEncoder :: E.Params (Entity VotingProcedure)
entityVotingProcedureEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getVotingProcedureId
    , entityVal >$< votingProcedureEncoder
    ]

votingProcedureEncoder :: E.Params VotingProcedure
votingProcedureEncoder =
  mconcat
    [ votingProcedureTxId >$< Id.idEncoder Id.getTxId
    , votingProcedureIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , votingProcedureGovActionProposalId >$< Id.idEncoder Id.getGovActionProposalId
    , votingProcedureVoterRole >$< E.param (E.nonNullable voterRoleEncoder)
    , votingProcedureDrepVoter >$< Id.maybeIdEncoder Id.getDrepHashId
    , votingProcedurePoolVoter >$< Id.maybeIdEncoder Id.getPoolHashId
    , votingProcedureVote >$< E.param (E.nonNullable voteEncoder)
    , votingProcedureVotingAnchorId >$< Id.maybeIdEncoder Id.getVotingAnchorId
    , votingProcedureCommitteeVoter >$< Id.maybeIdEncoder Id.getCommitteeHashId
    , votingProcedureInvalid >$< Id.maybeIdEncoder Id.getEventInfoId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: voting_anchor
-- Description: Acts as an anchor point for votes, ensuring they are securely recorded and linked to specific proposals.
-----------------------------------------------------------------------------------------------------------------------------------
data VotingAnchor = VotingAnchor
  { votingAnchorUrl :: !VoteUrl -- sqltype=varchar
  , votingAnchorDataHash :: !ByteString
  , votingAnchorType :: !AnchorType -- sqltype=anchorType
  , votingAnchorBlockId :: !Id.BlockId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key VotingAnchor = Id.VotingAnchorId
instance DbInfo VotingAnchor where
  uniqueFields _ = ["data_hash", "url", "type"]

entityVotingAnchorDecoder :: D.Row (Entity VotingAnchor)
entityVotingAnchorDecoder =
  Entity
    <$> Id.idDecoder Id.VotingAnchorId
    <*> votingAnchorDecoder

votingAnchorDecoder :: D.Row VotingAnchor
votingAnchorDecoder =
  VotingAnchor
    <$> D.column (D.nonNullable voteUrlDecoder) -- votingAnchorUrl
    <*> D.column (D.nonNullable D.bytea) -- votingAnchorDataHash
    <*> D.column (D.nonNullable anchorTypeDecoder) -- votingAnchorType
    <*> Id.idDecoder Id.BlockId -- votingAnchorBlockId

entityVotingAnchorEncoder :: E.Params (Entity VotingAnchor)
entityVotingAnchorEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getVotingAnchorId
    , entityVal >$< votingAnchorEncoder
    ]

votingAnchorEncoder :: E.Params VotingAnchor
votingAnchorEncoder =
  mconcat
    [ votingAnchorUrl >$< E.param (E.nonNullable voteUrlEncoder)
    , votingAnchorDataHash >$< E.param (E.nonNullable E.bytea)
    , votingAnchorType >$< E.param (E.nonNullable anchorTypeEncoder)
    , votingAnchorBlockId >$< Id.idEncoder Id.getBlockId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: constitution
-- Description: Holds the on-chain constitution, which defines the rules and principles of the blockchain's governance system.
-----------------------------------------------------------------------------------------------------------------------------------
data Constitution = Constitution
  { constitutionGovActionProposalId :: !(Maybe Id.GovActionProposalId) -- noreference
  , constitutionVotingAnchorId :: !Id.VotingAnchorId -- noreference
  , constitutionScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  }
  deriving (Eq, Show, Generic)

type instance Key Constitution = Id.ConstitutionId
instance DbInfo Constitution

entityConstitutionDecoder :: D.Row (Entity Constitution)
entityConstitutionDecoder =
  Entity
    <$> Id.idDecoder Id.ConstitutionId -- entityKey
    <*> constitutionDecoder -- entityVal

constitutionDecoder :: D.Row Constitution
constitutionDecoder =
  Constitution
    <$> Id.maybeIdDecoder Id.GovActionProposalId -- constitutionGovActionProposalId
    <*> Id.idDecoder Id.VotingAnchorId -- constitutionVotingAnchorId
    <*> D.column (D.nullable D.bytea) -- constitutionScriptHash

entityConstitutionEncoder :: E.Params (Entity Constitution)
entityConstitutionEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getConstitutionId
    , entityVal >$< constitutionEncoder
    ]

constitutionEncoder :: E.Params Constitution
constitutionEncoder =
  mconcat
    [ constitutionGovActionProposalId >$< Id.maybeIdEncoder Id.getGovActionProposalId
    , constitutionVotingAnchorId >$< Id.idEncoder Id.getVotingAnchorId
    , constitutionScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: committee
-- Description: Contains information about the committee, including the quorum requirements and the proposal being considered.
-----------------------------------------------------------------------------------------------------------------------------------
data Committee = Committee
  { committeeGovActionProposalId :: !(Maybe Id.GovActionProposalId) -- noreference
  , committeeQuorumNumerator :: !Word64
  , committeeQuorumDenominator :: !Word64
  }
  deriving (Eq, Show, Generic)

type instance Key Committee = Id.CommitteeId
instance DbInfo Committee

entityCommitteeDecoder :: D.Row (Entity Committee)
entityCommitteeDecoder =
  Entity
    <$> Id.idDecoder Id.CommitteeId -- entityKey
    <*> committeeDecoder -- entityVal

committeeDecoder :: D.Row Committee
committeeDecoder =
  Committee
    <$> Id.maybeIdDecoder Id.GovActionProposalId -- committeeGovActionProposalId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeQuorumNumerator
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeQuorumDenominator

entityCommitteeEncoder :: E.Params (Entity Committee)
entityCommitteeEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getCommitteeId
    , entityVal >$< committeeEncoder
    ]

committeeEncoder :: E.Params Committee
committeeEncoder =
  mconcat
    [ committeeGovActionProposalId >$< Id.maybeIdEncoder Id.getGovActionProposalId
    , committeeQuorumNumerator >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , committeeQuorumDenominator >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: committee_hash
-- Description: Stores hashes of committee records, which are used in governance processes.
data CommitteeHash = CommitteeHash
  { committeeHashRaw :: !ByteString -- sqltype=hash28type
  , committeeHashHasScript :: !Bool
  }
  deriving (Eq, Show, Generic)

type instance Key CommitteeHash = Id.CommitteeHashId
instance DbInfo CommitteeHash where
  uniqueFields _ = ["raw", "has_script"]

entityCommitteeHashDecoder :: D.Row (Entity CommitteeHash)
entityCommitteeHashDecoder =
  Entity
    <$> Id.idDecoder Id.CommitteeHashId -- entityKey
    <*> committeeHashDecoder -- entityVal

committeeHashDecoder :: D.Row CommitteeHash
committeeHashDecoder =
  CommitteeHash
    <$> D.column (D.nonNullable D.bytea) -- committeeHashRaw
    <*> D.column (D.nonNullable D.bool) -- committeeHashHasScript

entityCommitteeHashEncoder :: E.Params (Entity CommitteeHash)
entityCommitteeHashEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getCommitteeHashId
    , entityVal >$< committeeHashEncoder
    ]

committeeHashEncoder :: E.Params CommitteeHash
committeeHashEncoder =
  mconcat
    [ committeeHashRaw >$< E.param (E.nonNullable E.bytea)
    , committeeHashHasScript >$< E.param (E.nonNullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: committeemember
-- Description: Contains information about committee members.
data CommitteeMember = CommitteeMember
  { committeeMemberCommitteeId :: !Id.CommitteeId -- OnDeleteCascade -- here intentionally we use foreign keys
  , committeeMemberCommitteeHashId :: !Id.CommitteeHashId -- noreference
  , committeeMemberExpirationEpoch :: !Word64 -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key CommitteeMember = Id.CommitteeMemberId
instance DbInfo CommitteeMember

entityCommitteeMemberDecoder :: D.Row (Entity CommitteeMember)
entityCommitteeMemberDecoder =
  Entity
    <$> Id.idDecoder Id.CommitteeMemberId -- entityKey
    <*> committeeMemberDecoder -- entityVal

committeeMemberDecoder :: D.Row CommitteeMember
committeeMemberDecoder =
  CommitteeMember
    <$> Id.idDecoder Id.CommitteeId -- committeeMemberCommitteeId
    <*> Id.idDecoder Id.CommitteeHashId -- committeeMemberCommitteeHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeMemberExpirationEpoch

entityCommitteeMemberEncoder :: E.Params (Entity CommitteeMember)
entityCommitteeMemberEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getCommitteeMemberId
    , entityVal >$< committeeMemberEncoder
    ]

committeeMemberEncoder :: E.Params CommitteeMember
committeeMemberEncoder =
  mconcat
    [ committeeMemberCommitteeId >$< Id.idEncoder Id.getCommitteeId
    , committeeMemberCommitteeHashId >$< Id.idEncoder Id.getCommitteeHashId
    , committeeMemberExpirationEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: committeeregistration
-- Description: Contains information about the registration of committee members, including their public keys and other identifying information.
data CommitteeRegistration = CommitteeRegistration
  { committeeRegistrationTxId :: !Id.TxId -- noreference
  , committeeRegistrationCertIndex :: !Word16
  , committeeRegistrationColdKeyId :: !Id.CommitteeHashId -- noreference
  , committeeRegistrationHotKeyId :: !Id.CommitteeHashId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key CommitteeRegistration = Id.CommitteeRegistrationId
instance DbInfo CommitteeRegistration

entityCommitteeRegistrationDecoder :: D.Row (Entity CommitteeRegistration)
entityCommitteeRegistrationDecoder =
  Entity
    <$> Id.idDecoder Id.CommitteeRegistrationId -- entityKey
    <*> committeeRegistrationDecoder -- entityVal

committeeRegistrationDecoder :: D.Row CommitteeRegistration
committeeRegistrationDecoder =
  CommitteeRegistration
    <$> Id.idDecoder Id.TxId -- committeeRegistrationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- committeeRegistrationCertIndex
    <*> Id.idDecoder Id.CommitteeHashId -- committeeRegistrationColdKeyId
    <*> Id.idDecoder Id.CommitteeHashId -- committeeRegistrationHotKeyId

entityCommitteeRegistrationEncoder :: E.Params (Entity CommitteeRegistration)
entityCommitteeRegistrationEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getCommitteeRegistrationId
    , entityVal >$< committeeRegistrationEncoder
    ]

committeeRegistrationEncoder :: E.Params CommitteeRegistration
committeeRegistrationEncoder =
  mconcat
    [ committeeRegistrationTxId >$< Id.idEncoder Id.getTxId
    , committeeRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeRegistrationColdKeyId >$< Id.idEncoder Id.getCommitteeHashId
    , committeeRegistrationHotKeyId >$< Id.idEncoder Id.getCommitteeHashId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: committeede_registration
-- Description: Contains information about the deregistration of committee members, including their public keys and other identifying information.
data CommitteeDeRegistration = CommitteeDeRegistration
  { committeeDeRegistration_TxId :: !Id.TxId -- noreference
  , committeeDeRegistration_CertIndex :: !Word16
  , committeeDeRegistration_VotingAnchorId :: !(Maybe Id.VotingAnchorId) -- noreference
  , committeeDeRegistration_ColdKeyId :: !Id.CommitteeHashId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key CommitteeDeRegistration = Id.CommitteeDeRegistrationId
instance DbInfo CommitteeDeRegistration

entityCommitteeDeRegistrationDecoder :: D.Row (Entity CommitteeDeRegistration)
entityCommitteeDeRegistrationDecoder =
  Entity
    <$> Id.idDecoder Id.CommitteeDeRegistrationId -- entityKey
    <*> committeeDeRegistrationDecoder -- entityVal

committeeDeRegistrationDecoder :: D.Row CommitteeDeRegistration
committeeDeRegistrationDecoder =
  CommitteeDeRegistration
    <$> Id.idDecoder Id.TxId -- committeeDeRegistration_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- committeeDeRegistration_CertIndex
    <*> Id.maybeIdDecoder Id.VotingAnchorId -- committeeDeRegistration_VotingAnchorId
    <*> Id.idDecoder Id.CommitteeHashId -- committeeDeRegistration_ColdKeyId

entityCommitteeDeRegistrationEncoder :: E.Params (Entity CommitteeDeRegistration)
entityCommitteeDeRegistrationEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getCommitteeDeRegistrationId
    , entityVal >$< committeeDeRegistrationEncoder
    ]

committeeDeRegistrationEncoder :: E.Params CommitteeDeRegistration
committeeDeRegistrationEncoder =
  mconcat
    [ committeeDeRegistration_TxId >$< Id.idEncoder Id.getTxId
    , committeeDeRegistration_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeDeRegistration_VotingAnchorId >$< Id.maybeIdEncoder Id.getVotingAnchorId
    , committeeDeRegistration_ColdKeyId >$< Id.idEncoder Id.getCommitteeHashId
    ]

-- |
-- Table Name: param_proposal
-- Description: Contains proposals for changes to the protocol parameters, including the proposed values and the expiration date.
data ParamProposal = ParamProposal
  { paramProposalEpochNo :: !(Maybe Word64) -- sqltype=word31type
  , paramProposalKey :: !(Maybe ByteString) -- sqltype=hash28type
  , paramProposalMinFeeA :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMinFeeB :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxBlockSize :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxTxSize :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxBhSize :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalKeyDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , paramProposalPoolDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , paramProposalMaxEpoch :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalOptimalPoolCount :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalInfluence :: !(Maybe Double)
  , paramProposalMonetaryExpandRate :: !(Maybe Double)
  , paramProposalTreasuryGrowthRate :: !(Maybe Double)
  , paramProposalDecentralisation :: !(Maybe Double)
  , paramProposalEntropy :: !(Maybe ByteString) -- sqltype=hash32type
  , paramProposalProtocolMajor :: !(Maybe Word16) -- sqltype=word31type
  , paramProposalProtocolMinor :: !(Maybe Word16) -- sqltype=word31type
  , paramProposalMinUtxoValue :: !(Maybe DbLovelace) -- sqltype=lovelace
  , paramProposalMinPoolCost :: !(Maybe DbLovelace) -- sqltype=lovelace
  , paramProposalCostModelId :: !(Maybe Id.CostModelId) -- noreference
  , paramProposalPriceMem :: !(Maybe Double)
  , paramProposalPriceStep :: !(Maybe Double)
  , paramProposalMaxTxExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxTxExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxBlockExMem :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxBlockExSteps :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMaxValSize :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalCollateralPercent :: !(Maybe Word16) -- sqltype=word31type
  , paramProposalMaxCollateralInputs :: !(Maybe Word16) -- sqltype=word31type
  , paramProposalRegisteredTxId :: !Id.TxId -- noreference
  , paramProposalCoinsPerUtxoSize :: !(Maybe DbLovelace) -- sqltype=lovelace
  , paramProposalPvtMotionNoConfidence :: !(Maybe Double)
  , paramProposalPvtCommitteeNormal :: !(Maybe Double)
  , paramProposalPvtCommitteeNoConfidence :: !(Maybe Double)
  , paramProposalPvtHardForkInitiation :: !(Maybe Double)
  , paramProposalPvtppSecurityGroup :: !(Maybe Double)
  , paramProposalDvtMotionNoConfidence :: !(Maybe Double)
  , paramProposalDvtCommitteeNormal :: !(Maybe Double)
  , paramProposalDvtCommitteeNoConfidence :: !(Maybe Double)
  , paramProposalDvtUpdateToConstitution :: !(Maybe Double)
  , paramProposalDvtHardForkInitiation :: !(Maybe Double)
  , paramProposalDvtPPNetworkGroup :: !(Maybe Double)
  , paramProposalDvtPPEconomicGroup :: !(Maybe Double)
  , paramProposalDvtPPTechnicalGroup :: !(Maybe Double)
  , paramProposalDvtPPGovGroup :: !(Maybe Double)
  , paramProposalDvtTreasuryWithdrawal :: !(Maybe Double)
  , paramProposalCommitteeMinSize :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalCommitteeMaxTermLength :: !(Maybe DbWord64) --
  , paramProposalGovActionLifetime :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalGovActionDeposit :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalDrepDeposit :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalDrepActivity :: !(Maybe DbWord64) -- sqltype=word64type
  , paramProposalMinFeeRefScriptCostPerByte :: !(Maybe Double)
  }
  deriving (Show, Eq, Generic)

type instance Key ParamProposal = Id.ParamProposalId
instance DbInfo ParamProposal

entityParamProposalDecoder :: D.Row (Entity ParamProposal)
entityParamProposalDecoder =
  Entity
    <$> Id.idDecoder Id.ParamProposalId -- entityKey
    <*> paramProposalDecoder -- entityVal

paramProposalDecoder :: D.Row ParamProposal
paramProposalDecoder =
  ParamProposal
    <$> D.column (D.nullable $ fromIntegral <$> D.int8) -- paramProposalEpochNo
    <*> D.column (D.nullable D.bytea) -- paramProposalKey
    <*> maybeDbWord64Decoder -- paramProposalMinFeeA
    <*> maybeDbWord64Decoder -- paramProposalMinFeeB
    <*> maybeDbWord64Decoder -- paramProposalMaxBlockSize
    <*> maybeDbWord64Decoder -- paramProposalMaxTxSize
    <*> maybeDbWord64Decoder -- paramProposalMaxBhSize
    <*> maybeDbLovelaceDecoder -- paramProposalKeyDeposit
    <*> maybeDbLovelaceDecoder -- paramProposalPoolDeposit
    <*> maybeDbWord64Decoder -- paramProposalMaxEpoch
    <*> maybeDbWord64Decoder -- paramProposalOptimalPoolCount
    <*> D.column (D.nullable D.float8) -- paramProposalInfluence
    <*> D.column (D.nullable D.float8) -- paramProposalMonetaryExpandRate
    <*> D.column (D.nullable D.float8) -- paramProposalTreasuryGrowthRate
    <*> D.column (D.nullable D.float8) -- paramProposalDecentralisation
    <*> D.column (D.nullable D.bytea) -- paramProposalEntropy
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposalProtocolMajor
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposalProtocolMinor
    <*> maybeDbLovelaceDecoder -- paramProposalMinUtxoValue
    <*> maybeDbLovelaceDecoder -- paramProposalMinPoolCost
    <*> Id.maybeIdDecoder Id.CostModelId -- paramProposalCostModelId
    <*> D.column (D.nullable D.float8) -- paramProposalPriceMem
    <*> D.column (D.nullable D.float8) -- paramProposalPriceStep
    <*> maybeDbWord64Decoder -- paramProposalMaxTxExMem
    <*> maybeDbWord64Decoder -- paramProposalMaxTxExSteps
    <*> maybeDbWord64Decoder -- paramProposalMaxBlockExMem
    <*> maybeDbWord64Decoder -- paramProposalMaxBlockExSteps
    <*> maybeDbWord64Decoder -- paramProposalMaxValSize
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposalCollateralPercent
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposalMaxCollateralInputs
    <*> Id.idDecoder Id.TxId -- paramProposalRegisteredTxId
    <*> maybeDbLovelaceDecoder -- paramProposalCoinsPerUtxoSize
    <*> D.column (D.nullable D.float8) -- paramProposalPvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposalPvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- paramProposalPvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposalPvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- paramProposalPvtppSecurityGroup
    <*> D.column (D.nullable D.float8) -- paramProposalDvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposalDvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- paramProposalDvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposalDvtUpdateToConstitution
    <*> D.column (D.nullable D.float8) -- paramProposalDvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- paramProposalDvtPPNetworkGroup
    <*> D.column (D.nullable D.float8) -- paramProposalDvtPPEconomicGroup
    <*> D.column (D.nullable D.float8) -- paramProposalDvtPPTechnicalGroup
    <*> D.column (D.nullable D.float8) -- paramProposalDvtPPGovGroup
    <*> D.column (D.nullable D.float8) -- paramProposalDvtTreasuryWithdrawal
    <*> maybeDbWord64Decoder -- paramProposalCommitteeMinSize
    <*> maybeDbWord64Decoder -- paramProposalCommitteeMaxTermLength
    <*> maybeDbWord64Decoder -- paramProposalGovActionLifetime
    <*> maybeDbWord64Decoder -- paramProposalGovActionDeposit
    <*> maybeDbWord64Decoder -- paramProposalDrepDeposit
    <*> maybeDbWord64Decoder -- paramProposalDrepActivity
    <*> D.column (D.nullable D.float8) -- paramProposalMinFeeRefScriptCostPerByte

entityParamProposalEncoder :: E.Params (Entity ParamProposal)
entityParamProposalEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getParamProposalId
    , entityVal >$< paramProposalEncoder
    ]

paramProposalEncoder :: E.Params ParamProposal
paramProposalEncoder =
  mconcat
    [ paramProposalEpochNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , paramProposalKey >$< E.param (E.nullable E.bytea)
    , paramProposalMinFeeA >$< maybeDbWord64Encoder
    , paramProposalMinFeeB >$< maybeDbWord64Encoder
    , paramProposalMaxBlockSize >$< maybeDbWord64Encoder
    , paramProposalMaxTxSize >$< maybeDbWord64Encoder
    , paramProposalMaxBhSize >$< maybeDbWord64Encoder
    , paramProposalKeyDeposit >$< maybeDbLovelaceEncoder
    , paramProposalPoolDeposit >$< maybeDbLovelaceEncoder
    , paramProposalMaxEpoch >$< maybeDbWord64Encoder
    , paramProposalOptimalPoolCount >$< maybeDbWord64Encoder
    , paramProposalInfluence >$< E.param (E.nullable E.float8)
    , paramProposalMonetaryExpandRate >$< E.param (E.nullable E.float8)
    , paramProposalTreasuryGrowthRate >$< E.param (E.nullable E.float8)
    , paramProposalDecentralisation >$< E.param (E.nullable E.float8)
    , paramProposalEntropy >$< E.param (E.nullable E.bytea)
    , paramProposalProtocolMajor >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposalProtocolMinor >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposalMinUtxoValue >$< maybeDbLovelaceEncoder
    , paramProposalCoinsPerUtxoSize >$< maybeDbLovelaceEncoder
    , paramProposalCostModelId >$< Id.maybeIdEncoder Id.getCostModelId
    , paramProposalPriceMem >$< E.param (E.nullable E.float8)
    , paramProposalPriceStep >$< E.param (E.nullable E.float8)
    , paramProposalMaxTxExMem >$< maybeDbWord64Encoder
    , paramProposalMaxTxExSteps >$< maybeDbWord64Encoder
    , paramProposalMaxBlockExMem >$< maybeDbWord64Encoder
    , paramProposalMaxBlockExSteps >$< maybeDbWord64Encoder
    , paramProposalMaxValSize >$< maybeDbWord64Encoder
    , paramProposalCollateralPercent >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposalMaxCollateralInputs >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposalRegisteredTxId >$< Id.idEncoder Id.getTxId
    , paramProposalMinPoolCost >$< maybeDbLovelaceEncoder
    , paramProposalPvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposalPvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , paramProposalPvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposalPvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , paramProposalPvtppSecurityGroup >$< E.param (E.nullable E.float8)
    , paramProposalDvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposalDvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , paramProposalDvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposalDvtUpdateToConstitution >$< E.param (E.nullable E.float8)
    , paramProposalDvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , paramProposalDvtPPNetworkGroup >$< E.param (E.nullable E.float8)
    , paramProposalDvtPPEconomicGroup >$< E.param (E.nullable E.float8)
    , paramProposalDvtPPTechnicalGroup >$< E.param (E.nullable E.float8)
    , paramProposalDvtPPGovGroup >$< E.param (E.nullable E.float8)
    , paramProposalDvtTreasuryWithdrawal >$< E.param (E.nullable E.float8)
    , paramProposalCommitteeMinSize >$< maybeDbWord64Encoder
    , paramProposalCommitteeMaxTermLength >$< maybeDbWord64Encoder
    , paramProposalGovActionLifetime >$< maybeDbWord64Encoder
    , paramProposalGovActionDeposit >$< maybeDbWord64Encoder
    , paramProposalDrepDeposit >$< maybeDbWord64Encoder
    , paramProposalDrepActivity >$< maybeDbWord64Encoder
    , paramProposalMinFeeRefScriptCostPerByte >$< E.param (E.nullable E.float8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: treasury_withdrawal
-- Description:
data TreasuryWithdrawal = TreasuryWithdrawal
  { treasuryWithdrawalGovActionProposalId :: !Id.GovActionProposalId -- noreference
  , treasuryWithdrawalStakeAddressId :: !Id.StakeAddressId -- noreference
  , treasuryWithdrawalAmount :: !DbLovelace -- sqltype=lovelace
  }
  deriving (Eq, Show, Generic)

type instance Key TreasuryWithdrawal = Id.TreasuryWithdrawalId
instance DbInfo TreasuryWithdrawal

entityTreasuryWithdrawalDecoder :: D.Row (Entity TreasuryWithdrawal)
entityTreasuryWithdrawalDecoder =
  Entity
    <$> Id.idDecoder Id.TreasuryWithdrawalId -- entityKey
    <*> treasuryWithdrawalDecoder -- entityVal

treasuryWithdrawalDecoder :: D.Row TreasuryWithdrawal
treasuryWithdrawalDecoder =
  TreasuryWithdrawal
    <$> Id.idDecoder Id.GovActionProposalId -- treasuryWithdrawalGovActionProposalId
    <*> Id.idDecoder Id.StakeAddressId -- treasuryWithdrawalStakeAddressId
    <*> dbLovelaceDecoder -- treasuryWithdrawalAmount

entityTreasuryWithdrawalEncoder :: E.Params (Entity TreasuryWithdrawal)
entityTreasuryWithdrawalEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getTreasuryWithdrawalId
    , entityVal >$< treasuryWithdrawalEncoder
    ]

treasuryWithdrawalEncoder :: E.Params TreasuryWithdrawal
treasuryWithdrawalEncoder =
  mconcat
    [ treasuryWithdrawalGovActionProposalId >$< Id.idEncoder Id.getGovActionProposalId
    , treasuryWithdrawalStakeAddressId >$< Id.idEncoder Id.getStakeAddressId
    , treasuryWithdrawalAmount >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: event_info
-- Description: Contains information about events, including the epoch in which they occurred and the type of event.
data EventInfo = EventInfo
  { eventInfoTxId :: !(Maybe Id.TxId) -- noreference
  , eventInfoEpoch :: !Word64 -- sqltype=word31type
  , eventInfoType :: !Text
  , eventInfoExplanation :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key EventInfo = Id.EventInfoId
instance DbInfo EventInfo

entityEventInfoDecoder :: D.Row (Entity EventInfo)
entityEventInfoDecoder =
  Entity
    <$> Id.idDecoder Id.EventInfoId -- entityKey
    <*> eventInfoDecoder -- entityVal

eventInfoDecoder :: D.Row EventInfo
eventInfoDecoder =
  EventInfo
    <$> Id.maybeIdDecoder Id.TxId -- eventInfoTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- eventInfoEpoch
    <*> D.column (D.nonNullable D.text) -- eventInfoType
    <*> D.column (D.nullable D.text) -- eventInfoExplanation

entityEventInfoEncoder :: E.Params (Entity EventInfo)
entityEventInfoEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getEventInfoId
    , entityVal >$< eventInfoEncoder
    ]

eventInfoEncoder :: E.Params EventInfo
eventInfoEncoder =
  mconcat
    [ eventInfoTxId >$< Id.maybeIdEncoder Id.getTxId
    , eventInfoEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , eventInfoType >$< E.param (E.nonNullable E.text)
    , eventInfoExplanation >$< E.param (E.nullable E.text)
    ]
