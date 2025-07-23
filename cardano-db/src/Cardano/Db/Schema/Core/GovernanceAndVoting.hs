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
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (
  AnchorType,
  DbLovelace,
  DbWord64,
  GovActionType,
  Vote,
  VoteUrl,
  VoterRole,
  anchorTypeEncoder,
  dbLovelaceBulkEncoder,
  dbLovelaceEncoder,
  govActionTypeEncoder,
  maybeDbLovelaceDecoder,
  maybeDbLovelaceEncoder,
  maybeDbWord64Decoder,
  maybeDbWord64Encoder,
  voteEncoder,
  voteUrlEncoder,
  voterRoleEncoder,
 )
import Contravariant.Extras (contrazip3, contrazip4)

-----------------------------------------------------------------------------------------------------------------------------------
-- GOVERNANCE AND VOTING
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: drep_hash
-- Description: Stores hashes of DRep (Decentralized Reputation) records, which are used in governance processes.
data DrepHash = DrepHash
  { drepHashRaw :: !(Maybe ByteString) -- sqltype=hash28type
  , drepHashView :: !Text
  , drepHashHasScript :: !Bool
  }
  deriving (Eq, Show, Generic)

type instance Key DrepHash = Id.DrepHashId
instance DbInfo DrepHash where
  uniqueFields _ = ["raw", "has_script"]

drepHashEncoder :: E.Params DrepHash
drepHashEncoder =
  mconcat
    [ drepHashRaw >$< E.param (E.nullable E.bytea)
    , drepHashView >$< E.param (E.nonNullable E.text)
    , drepHashHasScript >$< E.param (E.nonNullable E.bool)
    ]

-- |
-- Table Name: drep_registration
-- Description: Contains details about the registration of DReps, including their public keys and other identifying information.
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

drepRegistrationEncoder :: E.Params DrepRegistration
drepRegistrationEncoder =
  mconcat
    [ drepRegistrationTxId >$< Id.idEncoder Id.getTxId
    , drepRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , drepRegistrationDeposit >$< E.param (E.nullable E.int8)
    , drepRegistrationDrepHashId >$< Id.idEncoder Id.getDrepHashId
    , drepRegistrationVotingAnchorId >$< Id.maybeIdEncoder Id.getVotingAnchorId
    ]

-- |
-- Table Name: drep_distr
-- Description: Contains information about the distribution of DRep tokens, including the amount distributed and the epoch in which the distribution occurred.
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
  unnestParamTypes _ =
    [ ("hash_id", "bigint[]")
    , ("amount", "bigint[]")
    , ("epoch_no", "bigint[]")
    , ("active_until", "bigint[]")
    ]

drepDistrBulkEncoder :: E.Params ([Id.DrepHashId], [Word64], [Word64], [Maybe Word64])
drepDistrBulkEncoder =
  contrazip4
    (bulkEncoder $ E.nonNullable $ Id.getDrepHashId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nullable $ fromIntegral >$< E.int8)

-- |
-- Table Name: delegation_vote
-- Description: Tracks votes cast by stakeholders to delegate their voting rights to other entities within the governance framework.
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

delegationVoteEncoder :: E.Params DelegationVote
delegationVoteEncoder =
  mconcat
    [ delegationVoteAddrId >$< Id.idEncoder Id.getStakeAddressId
    , delegationVoteCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegationVoteDrepHashId >$< Id.idEncoder Id.getDrepHashId
    , delegationVoteTxId >$< Id.idEncoder Id.getTxId
    , delegationVoteRedeemerId >$< Id.maybeIdEncoder Id.getRedeemerId
    ]

-- |
-- Table Name: gov_action_proposal
-- Description: Contains proposals for governance actions, including the type of action, the amount of the deposit, and the expiration date.
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

instance DbInfo GovActionProposal where
  jsonbFields _ = ["description"]
  enumFields _ = [("type", "govactiontype")]

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

-- |
-- Table Name: voting_procedure
-- Description: Defines the procedures and rules governing the voting process, including quorum requirements and tallying mechanisms.
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

instance DbInfo VotingProcedure where
  enumFields _ = [("voter_role", "voterrole"), ("vote", "vote")]

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

-- |
-- Table Name: voting_anchor
-- Description: Acts as an anchor point for votes, ensuring they are securely recorded and linked to specific proposals.
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
  enumFields _ = [("type", "anchorType")]

votingAnchorEncoder :: E.Params VotingAnchor
votingAnchorEncoder =
  mconcat
    [ votingAnchorUrl >$< E.param (E.nonNullable voteUrlEncoder)
    , votingAnchorDataHash >$< E.param (E.nonNullable E.bytea)
    , votingAnchorType >$< E.param (E.nonNullable anchorTypeEncoder)
    , votingAnchorBlockId >$< Id.idEncoder Id.getBlockId
    ]

-- |
-- Table Name: constitution
-- Description: Holds the on-chain constitution, which defines the rules and principles of the blockchain's governance system.
data Constitution = Constitution
  { constitutionGovActionProposalId :: !(Maybe Id.GovActionProposalId) -- noreference
  , constitutionVotingAnchorId :: !Id.VotingAnchorId -- noreference
  , constitutionScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  }
  deriving (Eq, Show, Generic)

type instance Key Constitution = Id.ConstitutionId
instance DbInfo Constitution

constitutionEncoder :: E.Params Constitution
constitutionEncoder =
  mconcat
    [ constitutionGovActionProposalId >$< Id.maybeIdEncoder Id.getGovActionProposalId
    , constitutionVotingAnchorId >$< Id.idEncoder Id.getVotingAnchorId
    , constitutionScriptHash >$< E.param (E.nullable E.bytea)
    ]

-- |
-- Table Name: committee
-- Description: Contains information about the committee, including the quorum requirements and the proposal being considered.
data Committee = Committee
  { committeeGovActionProposalId :: !(Maybe Id.GovActionProposalId) -- noreference
  , committeeQuorumNumerator :: !Word64
  , committeeQuorumDenominator :: !Word64
  }
  deriving (Eq, Show, Generic)

type instance Key Committee = Id.CommitteeId
instance DbInfo Committee

committeeDecoder :: D.Row Committee
committeeDecoder =
  Committee
    <$> Id.maybeIdDecoder Id.GovActionProposalId -- committeeGovActionProposalId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeQuorumNumerator
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- committeeQuorumDenominator

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
  { committeeDeRegistrationTxId :: !Id.TxId -- noreference
  , committeeDeRegistrationCertIndex :: !Word16
  , committeeDeRegistrationVotingAnchorId :: !(Maybe Id.VotingAnchorId) -- noreference
  , committeeDeRegistrationColdKeyId :: !Id.CommitteeHashId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key CommitteeDeRegistration = Id.CommitteeDeRegistrationId
instance DbInfo CommitteeDeRegistration

committeeDeRegistrationEncoder :: E.Params CommitteeDeRegistration
committeeDeRegistrationEncoder =
  mconcat
    [ committeeDeRegistrationTxId >$< Id.idEncoder Id.getTxId
    , committeeDeRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , committeeDeRegistrationVotingAnchorId >$< Id.maybeIdEncoder Id.getVotingAnchorId
    , committeeDeRegistrationColdKeyId >$< Id.idEncoder Id.getCommitteeHashId
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

instance DbInfo TreasuryWithdrawal where
  unnestParamTypes _ =
    [ ("gov_action_proposal_id", "bigint[]")
    , ("stake_address_id", "bigint[]")
    , ("amount", "bigint[]")
    ]

treasuryWithdrawalBulkEncoder :: E.Params ([Id.GovActionProposalId], [Id.StakeAddressId], [DbLovelace])
treasuryWithdrawalBulkEncoder =
  contrazip3
    (bulkEncoder $ E.nonNullable $ Id.getGovActionProposalId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ Id.getStakeAddressId >$< E.int8)
    (bulkEncoder dbLovelaceBulkEncoder)

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
