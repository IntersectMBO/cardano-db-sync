{-# LANGUAGE DeriveGeneric #-}

module Cardano.Db.Schema.Core.GovernanceAndVoting where

import Cardano.Db.Schema.Ids
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import Data.Functor.Contravariant
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E
import Cardano.Db.Types (
  DbLovelace,
  GovActionType,
  VoterRole,
  Vote,
  VoteUrl,
  AnchorType,
  DbWord64,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  maybeDbWord64Encoder,
  maybeDbLovelaceEncoder,
  govActionTypeDecoder,
  govActionTypeEncoder,
  voterRoleDecoder,
  voteDecoder,
  voterRoleEncoder,
  voteEncoder,
  voteUrlDecoder,
  anchorTypeDecoder,
  voteUrlEncoder,
  anchorTypeEncoder,
  maybeDbWord64Decoder,
  maybeDbLovelaceDecoder, HasDbInfo (..)
  )

-----------------------------------------------------------------------------------------------------------------------------------
-- GOVERNANCE AND VOTING
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_hash
Description: Stores hashes of DRep (Decentralized Reputation) records, which are used in governance processes.
-}
data DrepHash = DrepHash
  { drepHash_Id :: !DrepHashId
  , drepHash_Raw :: !(Maybe ByteString) -- sqltype=hash28type
  , drepHash_View :: !Text
  , drepHash_HasScript :: !Bool
  } deriving (Eq, Show, Generic)

instance HasDbInfo DrepHash

drepHashDecoder :: D.Row DrepHash
drepHashDecoder =
  DrepHash
    <$> idDecoder DrepHashId -- drepHash_Id
    <*> D.column (D.nullable D.bytea) -- drepHash_Raw
    <*> D.column (D.nonNullable D.text) -- drepHash_View
    <*> D.column (D.nonNullable D.bool) -- drepHash_HasScript

drepHashEncoder :: E.Params DrepHash
drepHashEncoder =
  mconcat
    [ drepHash_Id >$< idEncoder getDrepHashId
    , drepHash_Raw >$< E.param (E.nullable E.bytea)
    , drepHash_View >$< E.param (E.nonNullable E.text)
    , drepHash_HasScript >$< E.param (E.nonNullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_registration
Description: Contains details about the registration of DReps, including their public keys and other identifying information.
-}
data DrepRegistration = DrepRegistration
  { drepRegistration_Id :: !DrepRegistrationId
  , drepRegistration_TxId :: !TxId          -- noreference
  , drepRegistration_CertIndex :: !Word16
  , drepRegistration_Deposit :: !(Maybe Int64)
  , drepRegistration_DrepHashId :: !DrepHashId   -- noreference
  , drepRegistration_VotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo DrepRegistration

drepRegistrationDecoder :: D.Row DrepRegistration
drepRegistrationDecoder =
  DrepRegistration
    <$> idDecoder DrepRegistrationId -- drepRegistration_Id
    <*> idDecoder TxId -- drepRegistration_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- drepRegistration_CertIndex
    <*> D.column (D.nullable D.int8) -- drepRegistration_Deposit
    <*> idDecoder DrepHashId -- drepRegistration_DrepHashId
    <*> maybeIdDecoder VotingAnchorId -- drepRegistration_VotingAnchorId

drepRegistrationEncoder :: E.Params DrepRegistration
drepRegistrationEncoder =
  mconcat
    [ drepRegistration_Id >$< idEncoder getDrepRegistrationId
    , drepRegistration_TxId >$< idEncoder getTxId
    , drepRegistration_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , drepRegistration_Deposit >$< E.param (E.nullable E.int8)
    , drepRegistration_DrepHashId >$< idEncoder getDrepHashId
    , drepRegistration_VotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_distr
Description: Contains information about the distribution of DRep tokens, including the amount distributed and the epoch in which the distribution occurred.
-}
data DrepDistr = DrepDistr
  { drepDistr_Id :: !DrepDistrId
  , drepDistr_HashId :: !DrepHashId         -- noreference
  , drepDistr_Amount :: !Word64
  , drepDistr_EpochNo :: !Word64            -- sqltype=word31type
  , drepDistr_ActiveUntil :: !(Maybe Word64)  -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance HasDbInfo DrepDistr

drepDistrDecoder :: D.Row DrepDistr
drepDistrDecoder =
  DrepDistr
    <$> idDecoder DrepDistrId -- drepDistr_Id
    <*> idDecoder DrepHashId -- drepDistr_HashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- drepDistr_Amount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- drepDistr_EpochNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- drepDistr_ActiveUntil

drepDistrEncoder :: E.Params DrepDistr
drepDistrEncoder =
  mconcat
    [ drepDistr_Id >$< idEncoder getDrepDistrId
    , drepDistr_HashId >$< idEncoder getDrepHashId
    , drepDistr_Amount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , drepDistr_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , drepDistr_ActiveUntil >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delegation_vote
Description: Tracks votes cast by stakeholders to delegate their voting rights to other entities within the governance framework.
-}
data DelegationVote = DelegationVote
  { delegationVote_Id :: !DelegationVoteId
  , delegationVote_AddrId :: !StakeAddressId -- noreference
  , delegationVote_CertIndex :: !Word16
  , delegationVote_DrepHashId :: !DrepHashId -- noreference
  , delegationVote_TxId :: !TxId             -- noreference
  , delegationVote_RedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo DelegationVote

delegationVoteDecoder :: D.Row DelegationVote
delegationVoteDecoder =
  DelegationVote
    <$> idDecoder DelegationVoteId -- delegationVote_Id
    <*> idDecoder StakeAddressId -- delegationVote_AddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- delegationVote_CertIndex
    <*> idDecoder DrepHashId -- delegationVote_DrepHashId
    <*> idDecoder TxId -- delegationVote_TxId
    <*> maybeIdDecoder RedeemerId -- delegationVote_RedeemerId

delegationVoteEncoder :: E.Params DelegationVote
delegationVoteEncoder =
  mconcat
    [ delegationVote_Id >$< idEncoder getDelegationVoteId
    , delegationVote_AddrId >$< idEncoder getStakeAddressId
    , delegationVote_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegationVote_DrepHashId >$< idEncoder getDrepHashId
    , delegationVote_TxId >$< idEncoder getTxId
    , delegationVote_RedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: gov_action_proposal
Description: Contains proposals for governance actions, including the type of action, the amount of the deposit, and the expiration date.
-}
data GovActionProposal = GovActionProposal
  { govActionProposal_Id :: !GovActionProposalId
  , govActionProposal_TxId :: !TxId           -- noreference
  , govActionProposal_Index :: !Word64
  , govActionProposal_PrevGovActionProposal :: !(Maybe GovActionProposalId) -- noreference
  , govActionProposal_Deposit :: !DbLovelace  -- sqltype=lovelace
  , govActionProposal_ReturnAddress :: !StakeAddressId -- noreference
  , govActionProposal_Expiration :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposal_VotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , govActionProposal_Type :: !GovActionType   -- sqltype=govactiontype
  , govActionProposal_Description :: !Text     -- sqltype=jsonb
  , govActionProposal_ParamProposal :: !(Maybe ParamProposalId) -- noreference
  , govActionProposal_RatifiedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposal_EnactedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposal_DroppedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposal_ExpiredEpoch :: !(Maybe Word64)  -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance HasDbInfo GovActionProposal

govActionProposalDecoder :: D.Row GovActionProposal
govActionProposalDecoder =
  GovActionProposal
    <$> idDecoder GovActionProposalId -- govActionProposal_Id
    <*> idDecoder TxId -- govActionProposal_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- govActionProposal_Index
    <*> maybeIdDecoder GovActionProposalId -- govActionProposal_PrevGovActionProposal
    <*> dbLovelaceDecoder -- govActionProposal_Deposit
    <*> idDecoder StakeAddressId -- govActionProposal_ReturnAddress
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposal_Expiration
    <*> maybeIdDecoder VotingAnchorId -- govActionProposal_VotingAnchorId
    <*> D.column (D.nonNullable govActionTypeDecoder) -- govActionProposal_Type
    <*> D.column (D.nonNullable D.text) -- govActionProposal_Description
    <*> maybeIdDecoder ParamProposalId -- govActionProposal_ParamProposal
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposal_RatifiedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposal_EnactedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposal_DroppedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposal_ExpiredEpoch

govActionProposalEncoder :: E.Params GovActionProposal
govActionProposalEncoder =
  mconcat
    [ govActionProposal_Id >$< idEncoder getGovActionProposalId
    , govActionProposal_TxId >$< idEncoder getTxId
    , govActionProposal_Index >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , govActionProposal_PrevGovActionProposal >$< maybeIdEncoder getGovActionProposalId
    , govActionProposal_Deposit >$< dbLovelaceEncoder
    , govActionProposal_ReturnAddress >$< idEncoder getStakeAddressId
    , govActionProposal_Expiration >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposal_VotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    , govActionProposal_Type >$< E.param (E.nonNullable govActionTypeEncoder)
    , govActionProposal_Description >$< E.param (E.nonNullable E.text)
    , govActionProposal_ParamProposal >$< maybeIdEncoder getParamProposalId
    , govActionProposal_RatifiedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposal_EnactedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposal_DroppedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposal_ExpiredEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: voting_procedure
Description: Defines the procedures and rules governing the voting process, including quorum requirements and tallying mechanisms.
-}
data VotingProcedure = VotingProcedure
  { votingProcedure_Id :: !VotingProcedureId
  , votingProcedure_TxId :: !TxId                    -- noreference
  , votingProcedure_Index :: !Word16
  , votingProcedure_GovActionProposalId :: !GovActionProposalId -- noreference
  , votingProcedure_VoterRole :: !VoterRole           -- sqltype=voterrole
  , votingProcedure_DrepVoter :: !(Maybe DrepHashId)    -- noreference
  , votingProcedure_PoolVoter :: !(Maybe PoolHashId)    -- noreference
  , votingProcedure_Vote :: !Vote                     -- sqltype=vote
  , votingProcedure_VotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , votingProcedure_CommitteeVoter :: !(Maybe CommitteeHashId) -- noreference
  , votingProcedure_Invalid :: !(Maybe EventInfoId)    -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo VotingProcedure

votingProcedureDecoder :: D.Row VotingProcedure
votingProcedureDecoder =
  VotingProcedure
    <$> idDecoder VotingProcedureId -- votingProcedure_Id
    <*> idDecoder TxId -- votingProcedure_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- votingProcedure_Index
    <*> idDecoder GovActionProposalId -- votingProcedure_GovActionProposalId
    <*> D.column (D.nonNullable voterRoleDecoder) -- votingProcedure_VoterRole
    <*> maybeIdDecoder DrepHashId -- votingProcedure_DrepVoter
    <*> maybeIdDecoder PoolHashId -- votingProcedure_PoolVoter
    <*> D.column (D.nonNullable voteDecoder) -- votingProcedure_Vote
    <*> maybeIdDecoder VotingAnchorId -- votingProcedure_VotingAnchorId
    <*> maybeIdDecoder CommitteeHashId -- votingProcedure_CommitteeVoter
    <*> maybeIdDecoder EventInfoId -- votingProcedure_Invalid

votingProcedureEncoder :: E.Params VotingProcedure
votingProcedureEncoder =
  mconcat
    [ votingProcedure_Id >$< idEncoder getVotingProcedureId
    , votingProcedure_TxId >$< idEncoder getTxId
    , votingProcedure_Index >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , votingProcedure_GovActionProposalId >$< idEncoder getGovActionProposalId
    , votingProcedure_VoterRole >$< E.param (E.nonNullable voterRoleEncoder)
    , votingProcedure_DrepVoter >$< maybeIdEncoder getDrepHashId
    , votingProcedure_PoolVoter >$< maybeIdEncoder getPoolHashId
    , votingProcedure_Vote >$< E.param (E.nonNullable voteEncoder)
    , votingProcedure_VotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    , votingProcedure_CommitteeVoter >$< maybeIdEncoder getCommitteeHashId
    , votingProcedure_Invalid >$< maybeIdEncoder getEventInfoId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: voting_anchor
Description: Acts as an anchor point for votes, ensuring they are securely recorded and linked to specific proposals.
-}
data VotingAnchor = VotingAnchor
  { votingAnchor_Id :: !VotingAnchorId
  , votingAnchor_Url :: !VoteUrl           -- sqltype=varchar
  , votingAnchor_DataHash :: !ByteString
  , votingAnchor_Type :: !AnchorType       -- sqltype=anchorType
  , votingAnchor_BlockId :: !BlockId       -- noreference
  } deriving (Eq, Show, Generic)
-- UniqueVotingAnchor  dataHash url type

instance HasDbInfo VotingAnchor

votingAnchorDecoder :: D.Row VotingAnchor
votingAnchorDecoder =
  VotingAnchor
    <$> idDecoder VotingAnchorId -- votingAnchor_Id
    <*> D.column (D.nonNullable voteUrlDecoder) -- votingAnchor_Url
    <*> D.column (D.nonNullable D.bytea) -- votingAnchor_DataHash
    <*> D.column (D.nonNullable anchorTypeDecoder) -- votingAnchor_Type
    <*> idDecoder BlockId -- votingAnchor_BlockId

votingAnchorEncoder :: E.Params VotingAnchor
votingAnchorEncoder =
  mconcat
    [ votingAnchor_Id >$< idEncoder getVotingAnchorId
    , votingAnchor_Url >$< E.param (E.nonNullable voteUrlEncoder)
    , votingAnchor_DataHash >$< E.param (E.nonNullable E.bytea)
    , votingAnchor_Type >$< E.param (E.nonNullable anchorTypeEncoder)
    , votingAnchor_BlockId >$< idEncoder getBlockId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: constitution
Description: Holds the on-chain constitution, which defines the rules and principles of the blockchain's governance system.
-}
data Constitution = Constitution
  { constitution_Id :: !ConstitutionId
  , constitution_GovActionProposalId :: !(Maybe GovActionProposalId) -- noreference
  , constitution_VotingAnchorId :: !VotingAnchorId                -- noreference
  , constitution_ScriptHash :: !(Maybe ByteString)                  -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

instance HasDbInfo Constitution

constitutionDecoder :: D.Row Constitution
constitutionDecoder =
  Constitution
    <$> idDecoder ConstitutionId -- constitution_Id
    <*> maybeIdDecoder GovActionProposalId -- constitution_GovActionProposalId
    <*> idDecoder VotingAnchorId -- constitution_VotingAnchorId
    <*> D.column (D.nullable D.bytea) -- constitution_ScriptHash

constitutionEncoder :: E.Params Constitution
constitutionEncoder =
  mconcat
    [ constitution_Id >$< idEncoder getConstitutionId
    , constitution_GovActionProposalId >$< maybeIdEncoder getGovActionProposalId
    , constitution_VotingAnchorId >$< idEncoder getVotingAnchorId
    , constitution_ScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee
Description: Contains information about the committee, including the quorum requirements and the proposal being considered.
-}
data Committee = Committee
  { committee_Id :: !CommitteeId
  , committee_GovActionProposalId :: !(Maybe GovActionProposalId) -- noreference
  , committee_QuorumNumerator :: !Word64
  , committee_QuorumDenominator :: !Word64
  } deriving (Eq, Show, Generic)

instance HasDbInfo Committee

committeeDecoder :: D.Row Committee
committeeDecoder =
  Committee
    <$> idDecoder CommitteeId -- committee_Id
    <*> maybeIdDecoder GovActionProposalId -- committee_GovActionProposalId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committee_QuorumNumerator
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committee_QuorumDenominator

committeeEncoder :: E.Params Committee
committeeEncoder =
  mconcat
    [ committee_Id >$< idEncoder getCommitteeId
    , committee_GovActionProposalId >$< maybeIdEncoder getGovActionProposalId
    , committee_QuorumNumerator >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , committee_QuorumDenominator >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_hash
Description: Stores hashes of committee records, which are used in governance processes.
-}
data CommitteeHash = CommitteeHash
  { committeeHash_Id :: !CommitteeHashId
  , committeeHash_Raw :: !ByteString    -- sqltype=hash28type
  , committeeHash_HasScript :: !Bool
  } deriving (Eq, Show, Generic)
-- UniqueCommitteeHash  raw hasScript

instance HasDbInfo CommitteeHash

committeeHashDecoder :: D.Row CommitteeHash
committeeHashDecoder =
  CommitteeHash
    <$> idDecoder CommitteeHashId -- committeeHash_Id
    <*> D.column (D.nonNullable D.bytea) -- committeeHash_Raw
    <*> D.column (D.nonNullable D.bool) -- committeeHash_HasScript

committeeHashEncoder :: E.Params CommitteeHash
committeeHashEncoder =
  mconcat
    [ committeeHash_Id >$< idEncoder getCommitteeHashId
    , committeeHash_Raw >$< E.param (E.nonNullable E.bytea)
    , committeeHash_HasScript >$< E.param (E.nonNullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_member
Description: Contains information about committee members.
-}
data CommitteeMember = CommitteeMember
  { committeeMember_Id :: !CommitteeMemberId
  , committeeMember_CommitteeId :: !CommitteeId          -- OnDeleteCascade -- here intentionally we use foreign keys
  , committeeMember_CommitteeHashId :: !CommitteeHashId  -- noreference
  , committeeMember_ExpirationEpoch :: !Word64           -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance HasDbInfo CommitteeMember

committeeMemberDecoder :: D.Row CommitteeMember
committeeMemberDecoder =
  CommitteeMember
    <$> idDecoder CommitteeMemberId -- committeeMember_Id
    <*> idDecoder CommitteeId -- committeeMember_CommitteeId
    <*> idDecoder CommitteeHashId -- committeeMember_CommitteeHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeMember_ExpirationEpoch

committeeMemberEncoder :: E.Params CommitteeMember
committeeMemberEncoder =
  mconcat
    [ committeeMember_Id >$< idEncoder getCommitteeMemberId
    , committeeMember_CommitteeId >$< idEncoder getCommitteeId
    , committeeMember_CommitteeHashId >$< idEncoder getCommitteeHashId
    , committeeMember_ExpirationEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_registration
Description: Contains information about the registration of committee members, including their public keys and other identifying information.
-}
data CommitteeRegistration = CommitteeRegistration
  { committeeRegistration_Id :: !CommitteeRegistrationId
  , committeeRegistration_TxId :: !TxId -- noreference
  , committeeRegistration_CertIndex :: !Word16
  , committeeRegistration_ColdKeyId :: !CommitteeHashId -- noreference
  , committeeRegistration_HotKeyId :: !CommitteeHashId  -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo CommitteeRegistration

committeeRegistrationDecoder :: D.Row CommitteeRegistration
committeeRegistrationDecoder =
  CommitteeRegistration
    <$> idDecoder CommitteeRegistrationId -- committeeRegistration_Id
    <*> idDecoder TxId -- committeeRegistration_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- committeeRegistration_CertIndex
    <*> idDecoder CommitteeHashId -- committeeRegistration_ColdKeyId
    <*> idDecoder CommitteeHashId -- committeeRegistration_HotKeyId

committeeRegistrationEncoder :: E.Params CommitteeRegistration
committeeRegistrationEncoder =
  mconcat
    [ committeeRegistration_Id >$< idEncoder getCommitteeRegistrationId
    , committeeRegistration_TxId >$< idEncoder getTxId
    , committeeRegistration_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeRegistration_ColdKeyId >$< idEncoder getCommitteeHashId
    , committeeRegistration_HotKeyId >$< idEncoder getCommitteeHashId
    ]

{-|
Table Name: committee_de_registration
Description: Contains information about the deregistration of committee members, including their public keys and other identifying information.
-}
data CommitteeDeRegistration = CommitteeDeRegistration
  { committeeDeRegistration_Id :: !CommitteeDeRegistrationId
  , committeeDeRegistration_TxId :: !TxId -- noreference
  , committeeDeRegistration_CertIndex :: !Word16
  , committeeDeRegistration_VotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , committeeDeRegistration_ColdKeyId :: !CommitteeHashId -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo CommitteeDeRegistration

committeeDeRegistrationDecoder :: D.Row CommitteeDeRegistration
committeeDeRegistrationDecoder =
  CommitteeDeRegistration
    <$> idDecoder CommitteeDeRegistrationId -- committeeDeRegistration_Id
    <*> idDecoder TxId -- committeeDeRegistration_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- committeeDeRegistration_CertIndex
    <*> maybeIdDecoder VotingAnchorId -- committeeDeRegistration_VotingAnchorId
    <*> idDecoder CommitteeHashId -- committeeDeRegistration_ColdKeyId

committeeDeRegistrationEncoder :: E.Params CommitteeDeRegistration
committeeDeRegistrationEncoder =
  mconcat
    [ committeeDeRegistration_Id >$< idEncoder getCommitteeDeRegistrationId
    , committeeDeRegistration_TxId >$< idEncoder getTxId
    , committeeDeRegistration_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeDeRegistration_VotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    , committeeDeRegistration_ColdKeyId >$< idEncoder getCommitteeHashId
    ]

{-|
Table Name: param_proposal
Description: Contains proposals for changes to the protocol parameters, including the proposed values and the expiration date.
-}
data ParamProposal = ParamProposal
  { paramProposal_Id :: !ParamProposalId
  , paramProposal_EpochNo :: !(Maybe Word64)                -- sqltype=word31type
  , paramProposal_Key :: !(Maybe ByteString)                -- sqltype=hash28type
  , paramProposal_MinFeeA :: !(Maybe DbWord64)              -- sqltype=word64type
  , paramProposal_MinFeeB :: !(Maybe DbWord64)              -- sqltype=word64type
  , paramProposal_MaxBlockSize :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposal_MaxTxSize :: !(Maybe DbWord64)            -- sqltype=word64type
  , paramProposal_MaxBhSize :: !(Maybe DbWord64)            -- sqltype=word64type
  , paramProposal_KeyDeposit :: !(Maybe DbLovelace)         -- sqltype=lovelace
  , paramProposal_PoolDeposit :: !(Maybe DbLovelace)        -- sqltype=lovelace
  , paramProposal_MaxEpoch :: !(Maybe DbWord64)             -- sqltype=word64type
  , paramProposal_OptimalPoolCount :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposal_Influence :: !(Maybe Double)
  , paramProposal_MonetaryExpandRate :: !(Maybe Double)
  , paramProposal_TreasuryGrowthRate :: !(Maybe Double)
  , paramProposal_Decentralisation :: !(Maybe Double)
  , paramProposal_Entropy :: !(Maybe ByteString)            -- sqltype=hash32type
  , paramProposal_ProtocolMajor :: !(Maybe Word16)          -- sqltype=word31type
  , paramProposal_ProtocolMinor :: !(Maybe Word16)          -- sqltype=word31type
  , paramProposal_MinUtxoValue :: !(Maybe DbLovelace)       -- sqltype=lovelace
  , paramProposal_MinPoolCost :: !(Maybe DbLovelace)        -- sqltype=lovelace

  , paramProposal_CostModelId :: !(Maybe CostModelId)       -- noreference
  , paramProposal_PriceMem :: !(Maybe Double)
  , paramProposal_PriceStep :: !(Maybe Double)
  , paramProposal_MaxTxExMem :: !(Maybe DbWord64)           -- sqltype=word64type
  , paramProposal_MaxTxExSteps :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposal_MaxBlockExMem :: !(Maybe DbWord64)        -- sqltype=word64type
  , paramProposal_MaxBlockExSteps :: !(Maybe DbWord64)      -- sqltype=word64type
  , paramProposal_MaxValSize :: !(Maybe DbWord64)           -- sqltype=word64type
  , paramProposal_CollateralPercent :: !(Maybe Word16)      -- sqltype=word31type
  , paramProposal_MaxCollateralInputs :: !(Maybe Word16)    -- sqltype=word31type
  , paramProposal_RegisteredTxId :: !TxId                   -- noreference
  , paramProposal_CoinsPerUtxoSize :: !(Maybe DbLovelace)   -- sqltype=lovelace

  , paramProposal_PvtMotionNoConfidence :: !(Maybe Double)
  , paramProposal_PvtCommitteeNormal :: !(Maybe Double)
  , paramProposal_PvtCommitteeNoConfidence :: !(Maybe Double)
  , paramProposal_PvtHardForkInitiation :: !(Maybe Double)
  , paramProposal_PvtppSecurityGroup :: !(Maybe Double)

  , paramProposal_DvtMotionNoConfidence :: !(Maybe Double)
  , paramProposal_DvtCommitteeNormal :: !(Maybe Double)
  , paramProposal_DvtCommitteeNoConfidence :: !(Maybe Double)
  , paramProposal_DvtUpdateToConstitution :: !(Maybe Double)
  , paramProposal_DvtHardForkInitiation :: !(Maybe Double)
  , paramProposal_DvtPPNetworkGroup :: !(Maybe Double)
  , paramProposal_DvtPPEconomicGroup :: !(Maybe Double)
  , paramProposal_DvtPPTechnicalGroup :: !(Maybe Double)
  , paramProposal_DvtPPGovGroup :: !(Maybe Double)
  , paramProposal_DvtTreasuryWithdrawal :: !(Maybe Double)

  , paramProposal_CommitteeMinSize :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposal_CommitteeMaxTermLength :: !(Maybe DbWord64) --
  , paramProposal_GovActionLifetime :: !(Maybe DbWord64)    -- sqltype=word64type
  , paramProposal_GovActionDeposit :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposal_DrepDeposit :: !(Maybe DbWord64)          -- sqltype=word64type
  , paramProposal_DrepActivity :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposal_MinFeeRefScriptCostPerByte :: !(Maybe Double)

  } deriving (Show, Eq, Generic)


instance HasDbInfo ParamProposal

paramProposalDecoder :: D.Row ParamProposal
paramProposalDecoder =
  ParamProposal
    <$> idDecoder ParamProposalId -- paramProposal_Id
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- paramProposal_EpochNo
    <*> D.column (D.nullable D.bytea) -- paramProposal_Key
    <*> maybeDbWord64Decoder -- paramProposal_MinFeeA
    <*> maybeDbWord64Decoder -- paramProposal_MinFeeB
    <*> maybeDbWord64Decoder -- paramProposal_MaxBlockSize
    <*> maybeDbWord64Decoder -- paramProposal_MaxTxSize
    <*> maybeDbWord64Decoder -- paramProposal_MaxBhSize
    <*> maybeDbLovelaceDecoder -- paramProposal_KeyDeposit
    <*> maybeDbLovelaceDecoder -- paramProposal_PoolDeposit
    <*> maybeDbWord64Decoder -- paramProposal_MaxEpoch
    <*> maybeDbWord64Decoder -- paramProposal_OptimalPoolCount
    <*> D.column (D.nullable D.float8) -- paramProposal_Influence
    <*> D.column (D.nullable D.float8) -- paramProposal_MonetaryExpandRate
    <*> D.column (D.nullable D.float8) -- paramProposal_TreasuryGrowthRate
    <*> D.column (D.nullable D.float8) -- paramProposal_Decentralisation
    <*> D.column (D.nullable D.bytea) -- paramProposal_Entropy
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposal_ProtocolMajor
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposal_ProtocolMinor
    <*> maybeDbLovelaceDecoder -- paramProposal_MinUtxoValue
    <*> maybeDbLovelaceDecoder -- paramProposal_MinPoolCost
    <*> maybeIdDecoder CostModelId -- paramProposal_CostModelId
    <*> D.column (D.nullable D.float8) -- paramProposal_PriceMem
    <*> D.column (D.nullable D.float8) -- paramProposal_PriceStep
    <*> maybeDbWord64Decoder -- paramProposal_MaxTxExMem
    <*> maybeDbWord64Decoder -- paramProposal_MaxTxExSteps
    <*> maybeDbWord64Decoder -- paramProposal_MaxBlockExMem
    <*> maybeDbWord64Decoder -- paramProposal_MaxBlockExSteps
    <*> maybeDbWord64Decoder -- paramProposal_MaxValSize
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposal_CollateralPercent
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposal_MaxCollateralInputs
    <*> idDecoder TxId -- paramProposal_RegisteredTxId
    <*> maybeDbLovelaceDecoder -- paramProposal_CoinsPerUtxoSize
    <*> D.column (D.nullable D.float8) -- paramProposal_PvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposal_PvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- paramProposal_PvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposal_PvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- paramProposal_PvtppSecurityGroup
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtMotionNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtCommitteeNormal
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtCommitteeNoConfidence
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtUpdateToConstitution
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtHardForkInitiation
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtPPNetworkGroup
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtPPEconomicGroup
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtPPTechnicalGroup
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtPPGovGroup
    <*> D.column (D.nullable D.float8) -- paramProposal_DvtTreasuryWithdrawal
    <*> maybeDbWord64Decoder -- paramProposal_CommitteeMinSize
    <*> maybeDbWord64Decoder -- paramProposal_CommitteeMaxTermLength
    <*> maybeDbWord64Decoder -- paramProposal_GovActionLifetime
    <*> maybeDbWord64Decoder -- paramProposal_GovActionDeposit
    <*> maybeDbWord64Decoder -- paramProposal_DrepDeposit
    <*> maybeDbWord64Decoder -- paramProposal_DrepActivity
    <*> D.column (D.nullable D.float8) -- paramProposal_MinFeeRefScriptCostPerByte

paramProposalEncoder :: E.Params ParamProposal
paramProposalEncoder =
  mconcat
    [ paramProposal_Id >$< idEncoder getParamProposalId
    , paramProposal_EpochNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , paramProposal_Key >$< E.param (E.nullable E.bytea)
    , paramProposal_MinFeeA >$< maybeDbWord64Encoder
    , paramProposal_MinFeeB >$< maybeDbWord64Encoder
    , paramProposal_MaxBlockSize >$< maybeDbWord64Encoder
    , paramProposal_MaxTxSize >$< maybeDbWord64Encoder
    , paramProposal_MaxBhSize >$< maybeDbWord64Encoder
    , paramProposal_KeyDeposit >$< maybeDbLovelaceEncoder
    , paramProposal_PoolDeposit >$< maybeDbLovelaceEncoder
    , paramProposal_MaxEpoch >$< maybeDbWord64Encoder
    , paramProposal_OptimalPoolCount >$< maybeDbWord64Encoder
    , paramProposal_Influence >$< E.param (E.nullable E.float8)
    , paramProposal_MonetaryExpandRate >$< E.param (E.nullable E.float8)
    , paramProposal_TreasuryGrowthRate >$< E.param (E.nullable E.float8)
    , paramProposal_Decentralisation >$< E.param (E.nullable E.float8)
    , paramProposal_Entropy >$< E.param (E.nullable E.bytea)
    , paramProposal_ProtocolMajor >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposal_ProtocolMinor >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposal_MinUtxoValue >$< maybeDbLovelaceEncoder
    , paramProposal_CoinsPerUtxoSize >$< maybeDbLovelaceEncoder
    , paramProposal_CostModelId >$< maybeIdEncoder getCostModelId
    , paramProposal_PriceMem >$< E.param (E.nullable E.float8)
    , paramProposal_PriceStep >$< E.param (E.nullable E.float8)
    , paramProposal_MaxTxExMem >$< maybeDbWord64Encoder
    , paramProposal_MaxTxExSteps >$< maybeDbWord64Encoder
    , paramProposal_MaxBlockExMem >$< maybeDbWord64Encoder
    , paramProposal_MaxBlockExSteps >$< maybeDbWord64Encoder
    , paramProposal_MaxValSize >$< maybeDbWord64Encoder
    , paramProposal_CollateralPercent >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposal_MaxCollateralInputs >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposal_RegisteredTxId >$< idEncoder getTxId
    , paramProposal_MinPoolCost >$< maybeDbLovelaceEncoder
    , paramProposal_PvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposal_PvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , paramProposal_PvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposal_PvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , paramProposal_PvtppSecurityGroup >$< E.param (E.nullable E.float8)
    , paramProposal_DvtMotionNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposal_DvtCommitteeNormal >$< E.param (E.nullable E.float8)
    , paramProposal_DvtCommitteeNoConfidence >$< E.param (E.nullable E.float8)
    , paramProposal_DvtUpdateToConstitution >$< E.param (E.nullable E.float8)
    , paramProposal_DvtHardForkInitiation >$< E.param (E.nullable E.float8)
    , paramProposal_DvtPPNetworkGroup >$< E.param (E.nullable E.float8)
    , paramProposal_DvtPPEconomicGroup >$< E.param (E.nullable E.float8)
    , paramProposal_DvtPPTechnicalGroup >$< E.param (E.nullable E.float8)
    , paramProposal_DvtPPGovGroup >$< E.param (E.nullable E.float8)
    , paramProposal_DvtTreasuryWithdrawal >$< E.param (E.nullable E.float8)
    , paramProposal_CommitteeMinSize >$< maybeDbWord64Encoder
    , paramProposal_CommitteeMaxTermLength >$< maybeDbWord64Encoder
    , paramProposal_GovActionLifetime >$< maybeDbWord64Encoder
    , paramProposal_GovActionDeposit >$< maybeDbWord64Encoder
    , paramProposal_DrepDeposit >$< maybeDbWord64Encoder
    , paramProposal_DrepActivity >$< maybeDbWord64Encoder
    , paramProposal_MinFeeRefScriptCostPerByte >$< E.param (E.nullable E.float8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: treasury_withdrawal
Description:
-}
data TreasuryWithdrawal = TreasuryWithdrawal
  { treasuryWithdrawal_Id :: !TreasuryWithdrawalId
  , treasuryWithdrawal_GovActionProposalId :: !GovActionProposalId -- noreference
  , treasuryWithdrawal_StakeAddressId :: !StakeAddressId          -- noreference
  , treasuryWithdrawal_Amount :: !DbLovelace                      -- sqltype=lovelace
  } deriving (Eq, Show, Generic)

instance HasDbInfo TreasuryWithdrawal

treasuryWithdrawalDecoder :: D.Row TreasuryWithdrawal
treasuryWithdrawalDecoder =
  TreasuryWithdrawal
    <$> idDecoder TreasuryWithdrawalId -- treasuryWithdrawal_Id
    <*> idDecoder GovActionProposalId -- treasuryWithdrawal_GovActionProposalId
    <*> idDecoder StakeAddressId -- treasuryWithdrawal_StakeAddressId
    <*> dbLovelaceDecoder -- treasuryWithdrawal_Amount

treasuryWithdrawalEncoder :: E.Params TreasuryWithdrawal
treasuryWithdrawalEncoder =
  mconcat
    [ treasuryWithdrawal_Id >$< idEncoder getTreasuryWithdrawalId
    , treasuryWithdrawal_GovActionProposalId >$< idEncoder getGovActionProposalId
    , treasuryWithdrawal_StakeAddressId >$< idEncoder getStakeAddressId
    , treasuryWithdrawal_Amount >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: event_info
Description: Contains information about events, including the epoch in which they occurred and the type of event.
-}
data EventInfo = EventInfo
  { eventInfo_Id :: !EventInfoId
  , eventInfo_TxId :: !(Maybe TxId)           -- noreference
  , eventInfo_Epoch :: !Word64              -- sqltype=word31type
  , eventInfo_Type :: !Text
  , eventInfo_Explanation :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance HasDbInfo EventInfo

eventInfoDecoder :: D.Row EventInfo
eventInfoDecoder =
  EventInfo
    <$> idDecoder EventInfoId -- eventInfo_Id
    <*> maybeIdDecoder TxId -- eventInfo_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- eventInfo_Epoch
    <*> D.column (D.nonNullable D.text) -- eventInfo_Type
    <*> D.column (D.nullable D.text) -- eventInfo_Explanation

eventInfoEncoder :: E.Params EventInfo
eventInfoEncoder =
  mconcat
    [ eventInfo_Id >$< idEncoder getEventInfoId
    , eventInfo_TxId >$< maybeIdEncoder getTxId
    , eventInfo_Epoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , eventInfo_Type >$< E.param (E.nonNullable E.text)
    , eventInfo_Explanation >$< E.param (E.nullable E.text)
    ]
