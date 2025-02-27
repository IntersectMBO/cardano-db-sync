module Cardano.Db.Schema.Ids where

import Data.Int (Int64)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Data.Functor.Contravariant ((>$<))

-----------------------------------------------------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------------------------------------------------

{-|
  Helper function to create a decoder for an id column.
  The function takes a function that constructs the id type from an Int64.
-}
idDecoder :: (Int64 -> a) -> D.Row a
idDecoder f = D.column (D.nonNullable $ f <$> D.int8)

maybeIdDecoder :: (Int64 -> a) -> D.Row (Maybe a)
maybeIdDecoder f = D.column (D.nullable $ f <$> D.int8)

{-|
  Helper function to create an encoder for an id column.
  The function takes a function that extracts the Int64 from the id type.
-}
idEncoder :: (a -> Int64) -> E.Params a
idEncoder f = E.param $ E.nonNullable $ f >$< E.int8

idEncoderMany :: (a -> Int64) -> E.NullableOrNot E.Value  a
idEncoderMany f = E.nonNullable $ f >$< E.int8

maybeIdEncoder :: (a -> Int64) -> E.Params (Maybe a)
maybeIdEncoder f = E.param $ E.nullable $ f >$< E.int8

-----------------------------------------------------------------------------------------------------------------------------------
-- BASE TABLES
-----------------------------------------------------------------------------------------------------------------------------------
newtype BlockId = BlockId { getBlockId :: Int64 }
  deriving (Eq, Show, Ord)

newtype TxId = TxId { getTxId :: Int64 }
  deriving (Eq, Show, Ord)

newtype TxMetadataId = TxMetadataId { getTxMetadataId :: Int64 }
  deriving (Eq, Show, Ord)

newtype TxInId = TxInId { getTxInId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CollateralTxInId = CollateralTxInId { getCollateralTxInId :: Int64 }
  deriving (Eq, Show, Ord)

newtype AddressId = AddressId { getAddressId :: Int64 }
  deriving (Eq, Ord, Show)

newtype ReferenceTxInId = ReferenceTxInId { getReferenceTxInId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ReverseIndexId = ReverseIndexId { getReverseIndexId :: Int64 }
  deriving (Eq, Show, Ord)

newtype TxCborId = TxCborId { getTxCborId :: Int64 }
  deriving (Eq, Show, Ord)

newtype DatumId = DatumId { getDatumId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ScriptId = ScriptId { getScriptId :: Int64 }
  deriving (Eq, Show, Ord)

newtype RedeemerId = RedeemerId { getRedeemerId :: Int64 }
  deriving (Eq, Show, Ord)

newtype RedeemerDataId = RedeemerDataId { getRedeemerDataId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ExtraKeyWitnessId = ExtraKeyWitnessId { getExtraKeyWitnessId :: Int64 }
  deriving (Eq, Show, Ord)

newtype SlotLeaderId = SlotLeaderId { getSlotLeaderId :: Int64 }
  deriving (Eq, Show, Ord)

newtype SchemaVersionId = SchemaVersionId { getSchemaVersionId :: Int64 }
  deriving (Eq, Show, Ord)

newtype MetaId = MetaId { getMetaId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ExtraMigrationsId = ExtraMigrationsId { getExtraMigrationsId :: Int64 }
  deriving (Eq, Show, Ord)

newtype WithdrawalId = WithdrawalId { getWithdrawalId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
-- VARIANTS
-----------------------------------------------------------------------------------------------------------------------------------

-- | TxOut variants
newtype TxOutCoreId = TxOutCoreId { getTxOutCoreId :: Int64 }
  deriving (Eq, Ord, Show)

newtype TxOutAddressId = TxOutAddressId { getTxOutAddressId :: Int64 }
  deriving (Eq, Ord, Show)

newtype TxOutUtxoHdId = TxOutUtxoHdId { getTxOutUtxoHdId :: Int64 }
  deriving (Eq, Ord, Show)

newtype TxOutUtxoHdAddressId = TxOutUtxoHdAddressId { getTxOutUtxoHdAddressId :: Int64 }
  deriving (Eq, Ord, Show)

-- | CollateralTxOut variants
newtype CollateralTxOutCoreId = CollateralTxOutCoreId { getCollateralTxOutCoreId :: Int64 }
  deriving (Eq, Ord, Show)

newtype CollateralTxOutAddressId = CollateralTxOutAddressId { getCollateralTxOutAddressId :: Int64 }
  deriving (Eq, Ord, Show)

newtype CollateralTxOutUtxoHdId = CollateralTxOutUtxoHdId { getCollateralTxOutUtxoHdId :: Int64 }
  deriving (Eq, Ord, Show)

newtype CollateralTxOutUtxoHdAddressId = CollateralTxOutUtxoHdAddressId { getCollateralTxOutUtxoHdAddressId :: Int64 }
  deriving (Eq, Ord, Show)

-- | Multi-asset variants
newtype MaTxOutCoreId = MaTxOutCoreId { getMaTxOutCoreId :: Int64 }
  deriving (Eq, Ord, Show)

newtype MaTxOutAddressId = MaTxOutAddressId { getMaTxOutAddressId :: Int64 }
  deriving (Eq, Ord, Show)

newtype MaTxOutUtxoHdId = MaTxOutUtxoHdId { getMaTxOutUtxoHdId :: Int64 }
  deriving (Eq, Ord, Show)

newtype MaTxOutUtxoHdAddressId = MaTxOutUtxoHdAddressId { getMaTxOutUtxoHdAddressId :: Int64 }
  deriving (Eq, Ord, Show)


-----------------------------------------------------------------------------------------------------------------------------------
-- EPOCH AND PROTOCOL PARAMETER
-----------------------------------------------------------------------------------------------------------------------------------
newtype EpochId = EpochId { getEpochId :: Int64 }
  deriving (Eq, Show, Ord)

newtype EpochParamId = EpochParamId { getEpochParamId :: Int64 }
  deriving (Eq, Show, Ord)

newtype EpochStateId = EpochStateId { getEpochStateId :: Int64 }
  deriving (Eq, Show, Ord)

newtype EpochSyncTimeId = EpochSyncTimeId { getEpochSyncTimeId :: Int64 }
  deriving (Eq, Show, Ord)

newtype AdaPotsId = AdaPotsId { getAdaPotsId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PotTransferId = PotTransferId { getPotTransferId :: Int64 }
  deriving (Eq, Show, Ord)

newtype TreasuryId = TreasuryId { getTreasuryId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ReserveId = ReserveId { getReserveId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CostModelId = CostModelId { getCostModelId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
-- GOVERNANCE AND VOTING
-----------------------------------------------------------------------------------------------------------------------------------
newtype DrepHashId = DrepHashId { getDrepHashId :: Int64 }
  deriving (Eq, Show, Ord)

newtype DrepRegistrationId = DrepRegistrationId { getDrepRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

newtype DrepDistrId = DrepDistrId { getDrepDistrId :: Int64 }
  deriving (Eq, Show, Ord)

newtype DelegationVoteId = DelegationVoteId { getDelegationVoteId :: Int64 }
  deriving (Eq, Show, Ord)

newtype GovActionProposalId = GovActionProposalId { getGovActionProposalId :: Int64 }
  deriving (Eq, Show, Ord)

newtype VotingProcedureId = VotingProcedureId { getVotingProcedureId :: Int64 }
  deriving (Eq, Show, Ord)

newtype VotingAnchorId = VotingAnchorId { getVotingAnchorId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ConstitutionId = ConstitutionId { getConstitutionId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CommitteeId = CommitteeId { getCommitteeId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CommitteeHashId = CommitteeHashId { getCommitteeHashId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CommitteeMemberId = CommitteeMemberId { getCommitteeMemberId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CommitteeRegistrationId = CommitteeRegistrationId { getCommitteeRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

newtype CommitteeDeRegistrationId = CommitteeDeRegistrationId { getCommitteeDeRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ParamProposalId = ParamProposalId { getParamProposalId :: Int64 }
  deriving (Eq, Show, Ord)

newtype TreasuryWithdrawalId = TreasuryWithdrawalId { getTreasuryWithdrawalId :: Int64 }
  deriving (Eq, Show, Ord)

newtype EventInfoId = EventInfoId { getEventInfoId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
-- MULTI ASSETS
-----------------------------------------------------------------------------------------------------------------------------------
newtype MultiAssetId = MultiAssetId { getMultiAssetId :: Int64 }
  deriving (Eq, Show, Ord)

newtype MaTxMintId = MaTxMintId { getMaTxMintId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
-----------------------------------------------------------------------------------------------------------------------------------
newtype OffChainPoolDataId = OffChainPoolDataId { getOffChainPoolDataId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainPoolFetchErrorId = OffChainPoolFetchErrorId { getOffChainPoolFetchErrorId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteDataId = OffChainVoteDataId { getOffChainVoteDataId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteGovActionDataId = OffChainVoteGovActionDataId { getOffChainVoteGovActionDataId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteDrepDataId = OffChainVoteDrepDataId { getOffChainVoteDrepDataId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteAuthorId = OffChainVoteAuthorId { getOffChainVoteAuthorId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteReferenceId = OffChainVoteReferenceId { getOffChainVoteReferenceId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteExternalUpdateId = OffChainVoteExternalUpdateId { getOffChainVoteExternalUpdateId :: Int64 }
  deriving (Eq, Show, Ord)

newtype OffChainVoteFetchErrorId = OffChainVoteFetchErrorId { getOffChainVoteFetchErrorId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
-- POOLS
-----------------------------------------------------------------------------------------------------------------------------------

newtype PoolHashId = PoolHashId { getPoolHashId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PoolStatId = PoolStatId { getPoolStatId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PoolUpdateId = PoolUpdateId { getPoolUpdateId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PoolMetadataRefId = PoolMetadataRefId { getPoolMetadataRefId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PoolOwnerId = PoolOwnerId { getPoolOwnerId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PoolRetireId = PoolRetireId { getPoolRetireId :: Int64 }
  deriving (Eq, Show, Ord)

newtype PoolRelayId = PoolRelayId { getPoolRelayId :: Int64 }
  deriving (Eq, Show, Ord)

newtype DelistedPoolId = DelistedPoolId { getDelistedPoolId :: Int64 }
  deriving (Eq, Show, Ord)

newtype ReservedPoolTickerId = ReservedPoolTickerId { getReservedPoolTickerId :: Int64 }
  deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------------------------------------------------------------
-- | STAKE DELEGATION
-----------------------------------------------------------------------------------------------------------------------------------
newtype StakeAddressId = StakeAddressId { getStakeAddressId :: Int64 }
  deriving (Eq, Show, Ord)

newtype StakeRegistrationId = StakeRegistrationId { getStakeRegistrationId :: Int64 }
  deriving (Eq, Show, Ord)

newtype StakeDeregistrationId = StakeDeregistrationId { getStakeDeregistrationId :: Int64 }
  deriving (Eq, Show, Ord)

newtype DelegationId = DelegationId { getDelegationId :: Int64 }
  deriving (Eq, Show, Ord)

newtype RewardId = RewardId { getRewardId :: Int64 }
  deriving (Eq, Show, Ord)

newtype RewardRestId = RewardRestId { getRewardRestId :: Int64 }
  deriving (Eq, Show, Ord)

newtype EpochStakeId = EpochStakeId { getEpochStakeId :: Int64 }
  deriving (Eq, Show, Ord)

newtype EpochStakeProgressId = EpochStakeProgressId { getEpochStakeProgressId :: Int64 }
  deriving (Eq, Show, Ord)
