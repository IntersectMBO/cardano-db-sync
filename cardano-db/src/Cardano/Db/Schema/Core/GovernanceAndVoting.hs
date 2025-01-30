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
  maybeDbLovelaceDecoder
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
  { drepHashId :: !DrepHashId
  , drepHashRaw :: !(Maybe ByteString) -- sqltype=hash28type
  , drepHashView :: !Text
  , drepHashHasScript :: !Bool
  } deriving (Eq, Show, Generic)

drepHashDecoder :: D.Row DrepHash
drepHashDecoder =
  DrepHash
    <$> idDecoder DrepHashId -- drepHashId
    <*> D.column (D.nullable D.bytea) -- drepHashRaw
    <*> D.column (D.nonNullable D.text) -- drepHashView
    <*> D.column (D.nonNullable D.bool) -- drepHashHasScript

drepHashEncoder :: E.Params DrepHash
drepHashEncoder =
  mconcat
    [ drepHashId >$< idEncoder getDrepHashId
    , drepHashRaw >$< E.param (E.nullable E.bytea)
    , drepHashView >$< E.param (E.nonNullable E.text)
    , drepHashHasScript >$< E.param (E.nonNullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_registration
Description: Contains details about the registration of DReps, including their public keys and other identifying information.
-}
data DrepRegistration = DrepRegistration
  { drepRegistrationId :: !DrepRegistrationId
  , drepRegistrationTxId :: !TxId          -- noreference
  , drepRegistrationCertIndex :: !Word16
  , drepRegistrationDeposit :: !(Maybe Int64)
  , drepRegistrationVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , drepRegistrationDrepHashId :: !DrepHashId   -- noreference
  } deriving (Eq, Show, Generic)

drepRegistrationDecoder :: D.Row DrepRegistration
drepRegistrationDecoder =
  DrepRegistration
    <$> idDecoder DrepRegistrationId -- drepRegistrationId
    <*> idDecoder TxId -- drepRegistrationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- drepRegistrationCertIndex
    <*> D.column (D.nullable D.int8) -- drepRegistrationDeposit
    <*> maybeIdDecoder VotingAnchorId -- drepRegistrationVotingAnchorId
    <*> idDecoder DrepHashId -- drepRegistrationDrepHashId

drepRegistrationEncoder :: E.Params DrepRegistration
drepRegistrationEncoder =
  mconcat
    [ drepRegistrationId >$< idEncoder getDrepRegistrationId
    , drepRegistrationTxId >$< idEncoder getTxId
    , drepRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , drepRegistrationDeposit >$< E.param (E.nullable E.int8)
    , drepRegistrationVotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    , drepRegistrationDrepHashId >$< idEncoder getDrepHashId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: drep_distr
Description: Contains information about the distribution of DRep tokens, including the amount distributed and the epoch in which the distribution occurred.
-}
data DrepDistr = DrepDistr
  { drepDistrId :: !DrepDistrId
  , drepDistrHashId :: !DrepHashId         -- noreference
  , drepDistrAmount :: !Word64
  , drepDistrEpochNo :: !Word64            -- sqltype=word31type
  , drepDistrActiveUntil :: !(Maybe Word64)  -- sqltype=word31type
  } deriving (Eq, Show, Generic)

drepDistrDecoder :: D.Row DrepDistr
drepDistrDecoder =
  DrepDistr
    <$> idDecoder DrepDistrId -- drepDistrId
    <*> idDecoder DrepHashId -- drepDistrHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- drepDistrAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- drepDistrEpochNo
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- drepDistrActiveUntil

drepDistrEncoder :: E.Params DrepDistr
drepDistrEncoder =
  mconcat
    [ drepDistrId >$< idEncoder getDrepDistrId
    , drepDistrHashId >$< idEncoder getDrepHashId
    , drepDistrAmount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , drepDistrEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , drepDistrActiveUntil >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delegation_vote
Description: Tracks votes cast by stakeholders to delegate their voting rights to other entities within the governance framework.
-}
data DelegationVote = DelegationVote
  { delegationVoteId :: !DelegationVoteId
  , delegationVoteAddrId :: !StakeAddressId -- noreference
  , delegationVoteCertIndex :: !Word16
  , delegationVoteDrepHashId :: !DrepHashId -- noreference
  , delegationVoteTxId :: !TxId             -- noreference
  , delegationVoteRedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

delegationVoteDecoder :: D.Row DelegationVote
delegationVoteDecoder =
  DelegationVote
    <$> idDecoder DelegationVoteId -- delegationVoteId
    <*> idDecoder StakeAddressId -- delegationVoteAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- delegationVoteCertIndex
    <*> idDecoder DrepHashId -- delegationVoteDrepHashId
    <*> idDecoder TxId -- delegationVoteTxId
    <*> maybeIdDecoder RedeemerId -- delegationVoteRedeemerId

delegationVoteEncoder :: E.Params DelegationVote
delegationVoteEncoder =
  mconcat
    [ delegationVoteId >$< idEncoder getDelegationVoteId
    , delegationVoteAddrId >$< idEncoder getStakeAddressId
    , delegationVoteCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegationVoteDrepHashId >$< idEncoder getDrepHashId
    , delegationVoteTxId >$< idEncoder getTxId
    , delegationVoteRedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: gov_action_proposal
Description: Contains proposals for governance actions, including the type of action, the amount of the deposit, and the expiration date.
-}
data GovActionProposal = GovActionProposal
  { govActionProposalId :: !GovActionProposalId
  , govActionProposalTxId :: !TxId           -- noreference
  , govActionProposalIndex :: !Word64
  , govActionProposalPrevGovActionProposal :: !(Maybe GovActionProposalId) -- noreference
  , govActionProposalDeposit :: !DbLovelace  -- sqltype=lovelace
  , govActionProposalReturnAddress :: !StakeAddressId -- noreference
  , govActionProposalExpiration :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , govActionProposalType :: !GovActionType   -- sqltype=govactiontype
  , govActionProposalDescription :: !Text     -- sqltype=jsonb
  , govActionProposalParamProposal :: !(Maybe ParamProposalId) -- noreference
  , govActionProposalRatifiedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalEnactedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalDroppedEpoch :: !(Maybe Word64)  -- sqltype=word31type
  , govActionProposalExpiredEpoch :: !(Maybe Word64)  -- sqltype=word31type
  } deriving (Eq, Show, Generic)

govActionProposalDecoder :: D.Row GovActionProposal
govActionProposalDecoder =
  GovActionProposal
    <$> idDecoder GovActionProposalId -- govActionProposalId
    <*> idDecoder TxId -- govActionProposalTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- govActionProposalIndex
    <*> maybeIdDecoder GovActionProposalId -- govActionProposalPrevGovActionProposal
    <*> dbLovelaceDecoder -- govActionProposalDeposit
    <*> idDecoder StakeAddressId -- govActionProposalReturnAddress
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalExpiration
    <*> maybeIdDecoder VotingAnchorId -- govActionProposalVotingAnchorId
    <*> D.column (D.nonNullable govActionTypeDecoder) -- govActionProposalType
    <*> D.column (D.nonNullable D.text) -- govActionProposalDescription
    <*> maybeIdDecoder ParamProposalId -- govActionProposalParamProposal
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalRatifiedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalEnactedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalDroppedEpoch
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- govActionProposalExpiredEpoch

govActionProposalEncoder :: E.Params GovActionProposal
govActionProposalEncoder =
  mconcat
    [ govActionProposalId >$< idEncoder getGovActionProposalId
    , govActionProposalTxId >$< idEncoder getTxId
    , govActionProposalIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , govActionProposalPrevGovActionProposal >$< maybeIdEncoder getGovActionProposalId
    , govActionProposalDeposit >$< dbLovelaceEncoder
    , govActionProposalReturnAddress >$< idEncoder getStakeAddressId
    , govActionProposalExpiration >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalVotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    , govActionProposalType >$< E.param (E.nonNullable govActionTypeEncoder)
    , govActionProposalDescription >$< E.param (E.nonNullable E.text)
    , govActionProposalParamProposal >$< maybeIdEncoder getParamProposalId
    , govActionProposalRatifiedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalEnactedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalDroppedEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    , govActionProposalExpiredEpoch >$< E.param (E.nullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: voting_procedure
Description: Defines the procedures and rules governing the voting process, including quorum requirements and tallying mechanisms.
-}
data VotingProcedure = VotingProcedure
  { votingProcedureId :: !VotingProcedureId
  , votingProcedureTxId :: !TxId                    -- noreference
  , votingProcedureIndex :: !Word16
  , votingProcedureGovActionProposalId :: !GovActionProposalId -- noreference
  , votingProcedureVoterRole :: !VoterRole           -- sqltype=voterrole
  , votingProcedureCommitteeVoter :: !(Maybe CommitteeHashId) -- noreference
  , votingProcedureDrepVoter :: !(Maybe DrepHashId)    -- noreference
  , votingProcedurePoolVoter :: !(Maybe PoolHashId)    -- noreference
  , votingProcedureVote :: !Vote                     -- sqltype=vote
  , votingProcedureVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  , votingProcedureInvalid :: !(Maybe EventInfoId)    -- noreference
  } deriving (Eq, Show, Generic)

votingProcedureDecoder :: D.Row VotingProcedure
votingProcedureDecoder =
  VotingProcedure
    <$> idDecoder VotingProcedureId -- votingProcedureId
    <*> idDecoder TxId -- votingProcedureTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- votingProcedureIndex
    <*> idDecoder GovActionProposalId -- votingProcedureGovActionProposalId
    <*> D.column (D.nonNullable voterRoleDecoder) -- votingProcedureVoterRole
    <*> maybeIdDecoder CommitteeHashId -- votingProcedureCommitteeVoter
    <*> maybeIdDecoder DrepHashId -- votingProcedureDrepVoter
    <*> maybeIdDecoder PoolHashId -- votingProcedurePoolVoter
    <*> D.column (D.nonNullable voteDecoder) -- votingProcedureVote
    <*> maybeIdDecoder VotingAnchorId -- votingProcedureVotingAnchorId
    <*> maybeIdDecoder EventInfoId -- votingProcedureInvalid

votingProcedureEncoder :: E.Params VotingProcedure
votingProcedureEncoder =
  mconcat
    [ votingProcedureId >$< idEncoder getVotingProcedureId
    , votingProcedureTxId >$< idEncoder getTxId
    , votingProcedureIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , votingProcedureGovActionProposalId >$< idEncoder getGovActionProposalId
    , votingProcedureVoterRole >$< E.param (E.nonNullable voterRoleEncoder)
    , votingProcedureCommitteeVoter >$< maybeIdEncoder getCommitteeHashId
    , votingProcedureDrepVoter >$< maybeIdEncoder getDrepHashId
    , votingProcedurePoolVoter >$< maybeIdEncoder getPoolHashId
    , votingProcedureVote >$< E.param (E.nonNullable voteEncoder)
    , votingProcedureVotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    , votingProcedureInvalid >$< maybeIdEncoder getEventInfoId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: voting_anchor
Description: Acts as an anchor point for votes, ensuring they are securely recorded and linked to specific proposals.
-}
data VotingAnchor = VotingAnchor
  { votingAnchorId :: !VotingAnchorId
  , votingAnchorBlockId :: !BlockId       -- noreference
  , votingAnchorDataHash :: !ByteString
  , votingAnchorUrl :: !VoteUrl           -- sqltype=varchar
  , votingAnchorType :: !AnchorType       -- sqltype=anchorType
  } deriving (Eq, Show, Generic)
-- UniqueVotingAnchor  dataHash url type

votingAnchorDecoder :: D.Row VotingAnchor
votingAnchorDecoder =
  VotingAnchor
    <$> idDecoder VotingAnchorId -- votingAnchorId
    <*> idDecoder BlockId -- votingAnchorBlockId
    <*> D.column (D.nonNullable D.bytea) -- votingAnchorDataHash
    <*> D.column (D.nonNullable voteUrlDecoder) -- votingAnchorUrl
    <*> D.column (D.nonNullable anchorTypeDecoder) -- votingAnchorType

votingAnchorEncoder :: E.Params VotingAnchor
votingAnchorEncoder =
  mconcat
    [ votingAnchorId >$< idEncoder getVotingAnchorId
    , votingAnchorBlockId >$< idEncoder getBlockId
    , votingAnchorDataHash >$< E.param (E.nonNullable E.bytea)
    , votingAnchorUrl >$< E.param (E.nonNullable voteUrlEncoder)
    , votingAnchorType >$< E.param (E.nonNullable anchorTypeEncoder)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: constitution
Description: Holds the on-chain constitution, which defines the rules and principles of the blockchain's governance system.
-}
data Constitution = Constitution
  { constitutionId :: !ConstitutionId
  , constitutionGovActionProposalId :: !(Maybe GovActionProposalId) -- noreference
  , constitutionVotingAnchorId :: !VotingAnchorId                -- noreference
  , constitutionScriptHash :: !(Maybe ByteString)                  -- sqltype=hash28type
  } deriving (Eq, Show, Generic)

constitutionDecoder :: D.Row Constitution
constitutionDecoder =
  Constitution
    <$> idDecoder ConstitutionId -- constitutionId
    <*> maybeIdDecoder GovActionProposalId -- constitutionGovActionProposalId
    <*> idDecoder VotingAnchorId -- constitutionVotingAnchorId
    <*> D.column (D.nullable D.bytea) -- constitutionScriptHash

constitutionEncoder :: E.Params Constitution
constitutionEncoder =
  mconcat
    [ constitutionId >$< idEncoder getConstitutionId
    , constitutionGovActionProposalId >$< maybeIdEncoder getGovActionProposalId
    , constitutionVotingAnchorId >$< idEncoder getVotingAnchorId
    , constitutionScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee
Description: Contains information about the committee, including the quorum requirements and the proposal being considered.
-}
data Committee = Committee
  { committeeId :: !CommitteeId
  , committeeGovActionProposalId :: !(Maybe GovActionProposalId) -- noreference
  , committeeQuorumNumerator :: !Word64
  , committeeQuorumDenominator :: !Word64
  } deriving (Eq, Show, Generic)

committeeDecoder :: D.Row Committee
committeeDecoder =
  Committee
    <$> idDecoder CommitteeId -- committeeId
    <*> maybeIdDecoder GovActionProposalId -- committeeGovActionProposalId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeQuorumNumerator
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeQuorumDenominator

committeeEncoder :: E.Params Committee
committeeEncoder =
  mconcat
    [ committeeId >$< idEncoder getCommitteeId
    , committeeGovActionProposalId >$< maybeIdEncoder getGovActionProposalId
    , committeeQuorumNumerator >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , committeeQuorumDenominator >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_hash
Description: Stores hashes of committee records, which are used in governance processes.
-}
data CommitteeHash = CommitteeHash
  { committeeHashId :: !CommitteeHashId
  , committeeHashRaw :: !ByteString    -- sqltype=hash28type
  , committeeHashHasScript :: !Bool
  } deriving (Eq, Show, Generic)
-- UniqueCommitteeHash  raw hasScript

committeeHashDecoder :: D.Row CommitteeHash
committeeHashDecoder =
  CommitteeHash
    <$> idDecoder CommitteeHashId -- committeeHashId
    <*> D.column (D.nonNullable D.bytea) -- committeeHashRaw
    <*> D.column (D.nonNullable D.bool) -- committeeHashHasScript

committeeHashEncoder :: E.Params CommitteeHash
committeeHashEncoder =
  mconcat
    [ committeeHashId >$< idEncoder getCommitteeHashId
    , committeeHashRaw >$< E.param (E.nonNullable E.bytea)
    , committeeHashHasScript >$< E.param (E.nonNullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_member
Description: Contains information about committee members.
-}
data CommitteeMember = CommitteeMember
  { committeeMemberId :: !CommitteeMemberId
  , committeeMemberCommitteeId :: !CommitteeId          -- OnDeleteCascade -- here intentionally we use foreign keys
  , committeeMemberCommitteeHashId :: !CommitteeHashId  -- noreference
  , committeeMemberExpirationEpoch :: !Word64           -- sqltype=word31type
  } deriving (Eq, Show, Generic)

committeeMemberDecoder :: D.Row CommitteeMember
committeeMemberDecoder =
  CommitteeMember
    <$> idDecoder CommitteeMemberId -- committeeMemberId
    <*> idDecoder CommitteeId -- committeeMemberCommitteeId
    <*> idDecoder CommitteeHashId -- committeeMemberCommitteeHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeMemberExpirationEpoch

committeeMemberEncoder :: E.Params CommitteeMember
committeeMemberEncoder =
  mconcat
    [ committeeMemberId >$< idEncoder getCommitteeMemberId
    , committeeMemberCommitteeId >$< idEncoder getCommitteeId
    , committeeMemberCommitteeHashId >$< idEncoder getCommitteeHashId
    , committeeMemberExpirationEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: committee_registration
Description: Contains information about the registration of committee members, including their public keys and other identifying information.
-}
data CommitteeRegistration = CommitteeRegistration
  { committeeRegistrationId :: !CommitteeRegistrationId
  , committeeRegistrationTxId :: !TxId -- noreference
  , committeeRegistrationCertIndex :: !Word16
  , committeeRegistrationColdKeyId :: !CommitteeHashId -- noreference
  , committeeRegistrationHotKeyId :: !CommitteeHashId  -- noreference
  } deriving (Eq, Show, Generic)

committeeRegistrationDecoder :: D.Row CommitteeRegistration
committeeRegistrationDecoder =
  CommitteeRegistration
    <$> idDecoder CommitteeRegistrationId -- committeeRegistrationId
    <*> idDecoder TxId -- committeeRegistrationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- committeeRegistrationCertIndex
    <*> idDecoder CommitteeHashId -- committeeRegistrationColdKeyId
    <*> idDecoder CommitteeHashId -- committeeRegistrationHotKeyId

committeeRegistrationEncoder :: E.Params CommitteeRegistration
committeeRegistrationEncoder =
  mconcat
    [ committeeRegistrationId >$< idEncoder getCommitteeRegistrationId
    , committeeRegistrationTxId >$< idEncoder getTxId
    , committeeRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeRegistrationColdKeyId >$< idEncoder getCommitteeHashId
    , committeeRegistrationHotKeyId >$< idEncoder getCommitteeHashId
    ]

{-|
Table Name: committee_de_registration
Description: Contains information about the deregistration of committee members, including their public keys and other identifying information.
-}
data CommitteeDeRegistration = CommitteeDeRegistration
  { committeeDeRegistrationId :: !CommitteeDeRegistrationId
  , committeeDeRegistrationTxId :: !TxId -- noreference
  , committeeDeRegistrationCertIndex :: !Word16
  , committeeDeRegistrationColdKeyId :: !CommitteeHashId -- noreference
  , committeeDeRegistrationVotingAnchorId :: !(Maybe VotingAnchorId) -- noreference
  } deriving (Eq, Show, Generic)

committeeDeRegistrationDecoder :: D.Row CommitteeDeRegistration
committeeDeRegistrationDecoder =
  CommitteeDeRegistration
    <$> idDecoder CommitteeDeRegistrationId -- committeeDeRegistrationId
    <*> idDecoder TxId -- committeeDeRegistrationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- committeeDeRegistrationCertIndex
    <*> idDecoder CommitteeHashId -- committeeDeRegistrationColdKeyId
    <*> maybeIdDecoder VotingAnchorId -- committeeDeRegistrationVotingAnchorId

committeeDeRegistrationEncoder :: E.Params CommitteeDeRegistration
committeeDeRegistrationEncoder =
  mconcat
    [ committeeDeRegistrationId >$< idEncoder getCommitteeDeRegistrationId
    , committeeDeRegistrationTxId >$< idEncoder getTxId
    , committeeDeRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeDeRegistrationColdKeyId >$< idEncoder getCommitteeHashId
    , committeeDeRegistrationVotingAnchorId >$< maybeIdEncoder getVotingAnchorId
    ]

{-|
Table Name: param_proposal
Description: Contains proposals for changes to the protocol parameters, including the proposed values and the expiration date.
-}
data ParamProposal = ParamProposal
  { paramProposalId :: !ParamProposalId
  , paramProposalEpochNo :: !(Maybe Word64)                -- sqltype=word31type
  , paramProposalKey :: !(Maybe ByteString)                -- sqltype=hash28type
  , paramProposalMinFeeA :: !(Maybe DbWord64)              -- sqltype=word64type
  , paramProposalMinFeeB :: !(Maybe DbWord64)              -- sqltype=word64type
  , paramProposalMaxBlockSize :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposalMaxTxSize :: !(Maybe DbWord64)            -- sqltype=word64type
  , paramProposalMaxBhSize :: !(Maybe DbWord64)            -- sqltype=word64type
  , paramProposalKeyDeposit :: !(Maybe DbLovelace)         -- sqltype=lovelace
  , paramProposalPoolDeposit :: !(Maybe DbLovelace)        -- sqltype=lovelace
  , paramProposalMaxEpoch :: !(Maybe DbWord64)             -- sqltype=word64type
  , paramProposalOptimalPoolCount :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposalInfluence :: !(Maybe Double)
  , paramProposalMonetaryExpandRate :: !(Maybe Double)
  , paramProposalTreasuryGrowthRate :: !(Maybe Double)
  , paramProposalDecentralisation :: !(Maybe Double)
  , paramProposalEntropy :: !(Maybe ByteString)            -- sqltype=hash32type
  , paramProposalProtocolMajor :: !(Maybe Word16)          -- sqltype=word31type
  , paramProposalProtocolMinor :: !(Maybe Word16)          -- sqltype=word31type
  , paramProposalMinUtxoValue :: !(Maybe DbLovelace)       -- sqltype=lovelace

  , paramProposalMinPoolCost :: !(Maybe DbLovelace)        -- sqltype=lovelace
  , paramProposalCoinsPerUtxoSize :: !(Maybe DbLovelace)   -- sqltype=lovelace
  , paramProposalCostModelId :: !(Maybe CostModelId)       -- noreference
  , paramProposalPriceMem :: !(Maybe Double)
  , paramProposalPriceStep :: !(Maybe Double)
  , paramProposalMaxTxExMem :: !(Maybe DbWord64)           -- sqltype=word64type
  , paramProposalMaxTxExSteps :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposalMaxBlockExMem :: !(Maybe DbWord64)        -- sqltype=word64type
  , paramProposalMaxBlockExSteps :: !(Maybe DbWord64)      -- sqltype=word64type
  , paramProposalMaxValSize :: !(Maybe DbWord64)           -- sqltype=word64type
  , paramProposalCollateralPercent :: !(Maybe Word16)      -- sqltype=word31type
  , paramProposalMaxCollateralInputs :: !(Maybe Word16)    -- sqltype=word31type

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

  , paramProposalCommitteeMinSize :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposalCommitteeMaxTermLength :: !(Maybe DbWord64) --
  , paramProposalGovActionLifetime :: !(Maybe DbWord64)    -- sqltype=word64type
  , paramProposalGovActionDeposit :: !(Maybe DbWord64)     -- sqltype=word64type
  , paramProposalDrepDeposit :: !(Maybe DbWord64)          -- sqltype=word64type
  , paramProposalDrepActivity :: !(Maybe DbWord64)         -- sqltype=word64type
  , paramProposalMinFeeRefScriptCostPerByte :: !(Maybe Double)

  , paramProposalRegisteredTxId :: !TxId                 -- noreference
  } deriving (Show, Eq, Generic)

paramProposalDecoder :: D.Row ParamProposal
paramProposalDecoder =
  ParamProposal
    <$> idDecoder ParamProposalId -- paramProposalId
    <*> D.column (D.nullable $ fromIntegral <$> D.int8) -- paramProposalEpochNo
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
    <*> maybeDbLovelaceDecoder -- paramProposalCoinsPerUtxoSize
    <*> maybeIdDecoder CostModelId -- paramProposalCostModelId
    <*> D.column (D.nullable D.float8) -- paramProposalPriceMem
    <*> D.column (D.nullable D.float8) -- paramProposalPriceStep
    <*> maybeDbWord64Decoder -- paramProposalMaxTxExMem
    <*> maybeDbWord64Decoder -- paramProposalMaxTxExSteps
    <*> maybeDbWord64Decoder -- paramProposalMaxBlockExMem
    <*> maybeDbWord64Decoder -- paramProposalMaxBlockExSteps
    <*> maybeDbWord64Decoder -- paramProposalMaxValSize
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposalCollateralPercent
    <*> D.column (D.nullable $ fromIntegral <$> D.int2) -- paramProposalMaxCollateralInputs
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
    <*> idDecoder TxId -- paramProposalRegisteredTxId

paramProposalEncoder :: E.Params ParamProposal
paramProposalEncoder =
  mconcat
    [ paramProposalId >$< idEncoder getParamProposalId
    , paramProposalEpochNo >$< E.param (E.nullable $ fromIntegral >$< E.int8)
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
    , paramProposalMinPoolCost >$< maybeDbLovelaceEncoder
    , paramProposalCoinsPerUtxoSize >$< maybeDbLovelaceEncoder
    , paramProposalCostModelId >$< maybeIdEncoder getCostModelId
    , paramProposalPriceMem >$< E.param (E.nullable E.float8)
    , paramProposalPriceStep >$< E.param (E.nullable E.float8)
    , paramProposalMaxTxExMem >$< maybeDbWord64Encoder
    , paramProposalMaxTxExSteps >$< maybeDbWord64Encoder
    , paramProposalMaxBlockExMem >$< maybeDbWord64Encoder
    , paramProposalMaxBlockExSteps >$< maybeDbWord64Encoder
    , paramProposalMaxValSize >$< maybeDbWord64Encoder
    , paramProposalCollateralPercent >$< E.param (E.nullable $ fromIntegral >$< E.int2)
    , paramProposalMaxCollateralInputs >$< E.param (E.nullable $ fromIntegral >$< E.int2)
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
    , paramProposalRegisteredTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: treasury_withdrawal
Description:
-}
data TreasuryWithdrawal = TreasuryWithdrawal
  { treasuryWithdrawalId :: !TreasuryWithdrawalId
  , treasuryWithdrawalGovActionProposalId :: !GovActionProposalId -- noreference
  , treasuryWithdrawalStakeAddressId :: !StakeAddressId          -- noreference
  , treasuryWithdrawalAmount :: !DbLovelace                      -- sqltype=lovelace
  } deriving (Eq, Show, Generic)

treasuryWithdrawalDecoder :: D.Row TreasuryWithdrawal
treasuryWithdrawalDecoder =
  TreasuryWithdrawal
    <$> idDecoder TreasuryWithdrawalId -- treasuryWithdrawalId
    <*> idDecoder GovActionProposalId -- treasuryWithdrawalGovActionProposalId
    <*> idDecoder StakeAddressId -- treasuryWithdrawalStakeAddressId
    <*> dbLovelaceDecoder -- treasuryWithdrawalAmount

treasuryWithdrawalEncoder :: E.Params TreasuryWithdrawal
treasuryWithdrawalEncoder =
  mconcat
    [ treasuryWithdrawalId >$< idEncoder getTreasuryWithdrawalId
    , treasuryWithdrawalGovActionProposalId >$< idEncoder getGovActionProposalId
    , treasuryWithdrawalStakeAddressId >$< idEncoder getStakeAddressId
    , treasuryWithdrawalAmount >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: event_info
Description: Contains information about events, including the epoch in which they occurred and the type of event.
-}
data EventInfo = EventInfo
  { eventInfoId :: !EventInfoId
  , eventInfoTxId :: !(Maybe TxId)           -- noreference
  , eventInfoEpoch :: !Word64              -- sqltype=word31type
  , eventInfoType :: !Text
  , eventInfoExplanation :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

eventInfoDecoder :: D.Row EventInfo
eventInfoDecoder =
  EventInfo
    <$> idDecoder EventInfoId -- eventInfoId
    <*> maybeIdDecoder TxId -- eventInfoTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- eventInfoEpoch
    <*> D.column (D.nonNullable D.text) -- eventInfoType
    <*> D.column (D.nullable D.text) -- eventInfoExplanation

eventInfoEncoder :: E.Params EventInfo
eventInfoEncoder =
  mconcat
    [ eventInfoId >$< idEncoder getEventInfoId
    , eventInfoTxId >$< maybeIdEncoder getTxId
    , eventInfoEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , eventInfoType >$< E.param (E.nonNullable E.text)
    , eventInfoExplanation >$< E.param (E.nullable E.text)
    ]
