{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Schema.Core.StakeDelegation where

import Contravariant.Extras (contrazip4, contrazip5)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Types (textDecoder)
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Key)
import Cardano.Db.Types (
  DbLovelace (..),
  RewardSource,
  maybeDbLovelaceEncoder,
  rewardSourceEncoder,
 )

-----------------------------------------------------------------------------------------------------------------------------------

-- | STAKE DELEGATION
-- | These tables handle stake addresses, delegation, and reward

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: stake_address
-- Description: Contains information about stakeholder addresses.
data StakeAddress = StakeAddress -- Can be an address of a script hash
  { stakeAddressHashRaw :: !ByteString -- sqltype=addr29type
  , stakeAddressView :: !Text
  , stakeAddressScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  }
  deriving (Show, Eq, Generic)

type instance Key StakeAddress = StakeAddressId
instance DbInfo StakeAddress where
  uniqueFields _ = ["hash_raw"]

stakeAddressDecoder :: D.Row StakeAddress
stakeAddressDecoder =
  StakeAddress
    <$> D.column (D.nonNullable D.bytea) -- stakeAddressHashRaw
    <*> D.column (D.nonNullable textDecoder) -- stakeAddressView
    <*> D.column (D.nullable D.bytea) -- stakeAddressScriptHash

stakeAddressEncoder :: E.Params StakeAddress
stakeAddressEncoder =
  mconcat
    [ stakeAddressHashRaw >$< E.param (E.nonNullable E.bytea)
    , stakeAddressView >$< E.param (E.nonNullable E.text)
    , stakeAddressScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: stake_registration
-- Description: Contains information about stakeholder registrations.
data StakeRegistration = StakeRegistration
  { stakeRegistrationAddrId :: !StakeAddressId -- noreference
  , stakeRegistrationCertIndex :: !Word16
  , stakeRegistrationEpochNo :: !Word64 -- sqltype=word31type
  , stakeRegistrationTxId :: !TxId -- noreference
  , stakeRegistrationDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  }
  deriving (Eq, Show, Generic)

type instance Key StakeRegistration = StakeRegistrationId
instance DbInfo StakeRegistration

stakeRegistrationEncoder :: E.Params StakeRegistration
stakeRegistrationEncoder =
  mconcat
    [ stakeRegistrationAddrId >$< idEncoder getStakeAddressId
    , stakeRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , stakeRegistrationEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , stakeRegistrationTxId >$< idEncoder getTxId
    , stakeRegistrationDeposit >$< maybeDbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: stake_deregistration
-- Description: Contains information about stakeholder deregistrations.
data StakeDeregistration = StakeDeregistration
  { stakeDeregistrationAddrId :: !StakeAddressId -- noreference
  , stakeDeregistrationCertIndex :: !Word16
  , stakeDeregistrationEpochNo :: !Word64 -- sqltype=word31type
  , stakeDeregistrationTxId :: !TxId -- noreference
  , stakeDeregistrationRedeemerId :: !(Maybe RedeemerId) -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key StakeDeregistration = StakeDeregistrationId
instance DbInfo StakeDeregistration

stakeDeregistrationDecoder :: D.Row StakeDeregistration
stakeDeregistrationDecoder =
  StakeDeregistration
    <$> idDecoder StakeAddressId -- stakeDeregistrationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeDeregistrationCertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeDeregistrationEpochNo
    <*> idDecoder TxId -- stakeDeregistrationTxId
    <*> maybeIdDecoder RedeemerId -- stakeDeregistrationRedeemerId

stakeDeregistrationEncoder :: E.Params StakeDeregistration
stakeDeregistrationEncoder =
  mconcat
    [ stakeDeregistrationAddrId >$< idEncoder getStakeAddressId
    , stakeDeregistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , stakeDeregistrationEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , stakeDeregistrationTxId >$< idEncoder getTxId
    , stakeDeregistrationRedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: delegation
-- Description:Contains information about stakeholder delegations, including the stakeholder's address and the pool to which they are delegating.
data Delegation = Delegation
  { delegationAddrId :: !StakeAddressId -- noreference
  , delegationCertIndex :: !Word16
  , delegationPoolHashId :: !PoolHashId -- noreference
  , delegationActiveEpochNo :: !Word64
  , delegationTxId :: !TxId -- noreference
  , delegationSlotNo :: !Word64 -- sqltype=word63type
  , delegationRedeemerId :: !(Maybe RedeemerId) -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key Delegation = DelegationId
instance DbInfo Delegation

delegationDecoder :: D.Row Delegation
delegationDecoder =
  Delegation
    <$> idDecoder StakeAddressId -- delegationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- delegationCertIndex
    <*> idDecoder PoolHashId -- delegationPoolHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- delegationActiveEpochNo
    <*> idDecoder TxId -- delegationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- delegationSlotNo
    <*> maybeIdDecoder RedeemerId -- delegationRedeemerId

delegationEncoder :: E.Params Delegation
delegationEncoder =
  mconcat
    [ delegationAddrId >$< idEncoder getStakeAddressId
    , delegationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegationPoolHashId >$< idEncoder getPoolHashId
    , delegationActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , delegationTxId >$< idEncoder getTxId
    , delegationSlotNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , delegationRedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: reward
-- Description: Reward, Stake and Treasury need to be obtained from the ledger state.
--   The reward for each stake address and. This is not a balance, but a reward amount and the
--   epoch in which the reward was earned.
--   This table should never get rolled back.
data Reward = Reward
  { rewardAddrId :: !StakeAddressId -- noreference
  , rewardType :: !RewardSource -- sqltype=rewardtype
  , rewardAmount :: !DbLovelace -- sqltype=lovelace
  , rewardSpendableEpoch :: !Word64
  , rewardPoolId :: !PoolHashId -- noreference
  , rewardEarnedEpoch :: !Word64 -- generated="((CASE WHEN (type='refund') then spendable_epoch else (CASE WHEN spendable_epoch >= 2 then spendable_epoch-2 else 0 end) end) STORED)"
  }
  deriving (Show, Eq, Generic)

type instance Key Reward = RewardId

instance DbInfo Reward where
  enumFields _ = [("type", "rewardtype"), ("amount", "lovelace")]
  generatedFields _ = ["earned_epoch"]
  unnestParamTypes _ = [("addr_id", "bigint[]"), ("type", "text[]"), ("amount", "bigint[]"), ("spendable_epoch", "bigint[]"), ("pool_id", "bigint[]")]

rewardBulkEncoder :: E.Params ([StakeAddressId], [RewardSource], [DbLovelace], [Word64], [PoolHashId])
rewardBulkEncoder =
  contrazip5
    (bulkEncoder $ idBulkEncoder getStakeAddressId)
    (bulkEncoder $ E.nonNullable rewardSourceEncoder)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ idBulkEncoder getPoolHashId)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: reward_rest
-- Description: Contains information about the remaining reward for each stakeholder.
data RewardRest = RewardRest
  { rewardRestAddrId :: !StakeAddressId -- noreference
  , rewardRestType :: !RewardSource -- sqltype=rewardtype
  , rewardRestAmount :: !DbLovelace -- sqltype=lovelace
  , rewardRestSpendableEpoch :: !Word64
  , rewardRestEarnedEpoch :: !Word64 -- generated="(CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)"
  }
  deriving (Show, Eq, Generic)

type instance Key RewardRest = RewardRestId

instance DbInfo RewardRest where
  enumFields _ = [("type", "rewardtype"), ("amount", "lovelace")]
  generatedFields _ = ["earned_epoch"]
  unnestParamTypes _ =
    [ ("addr_id", "bigint[]")
    , ("type", "text[]")
    , ("amount", "bigint[]")
    , ("spendable_epoch", "bigint[]")
    ]

rewardRestBulkEncoder :: E.Params ([StakeAddressId], [RewardSource], [DbLovelace], [Word64])
rewardRestBulkEncoder =
  contrazip4
    (bulkEncoder $ idBulkEncoder getStakeAddressId)
    (bulkEncoder $ E.nonNullable rewardSourceEncoder)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: epoch_stake
-- Description: Contains information about the stake of each stakeholder in each epoch.
-- This table should never get rolled back
data EpochStake = EpochStake
  { epochStakeAddrId :: !StakeAddressId -- noreference
  , epochStakePoolId :: !PoolHashId -- noreference
  , epochStakeAmount :: !DbLovelace -- sqltype=lovelace
  , epochStakeEpochNo :: !Word64 -- sqltype=word31type
  }
  deriving (Show, Eq, Generic)

-- similar scenario as in Reward the constraint that was here is now set manually in
-- `applyAndInsertBlockMaybe` at a more optimal time.

type instance Key EpochStake = EpochStakeId

instance DbInfo EpochStake where
  uniqueFields _ = ["addr_id", "pool_id", "epoch_no"]
  unnestParamTypes _ =
    [ ("addr_id", "bigint[]")
    , ("pool_id", "bigint[]")
    , ("amount", "bigint[]")
    , ("epoch_no", "bigint[]")
    ]

epochStakeBulkEncoder :: E.Params ([StakeAddressId], [PoolHashId], [DbLovelace], [Word64])
epochStakeBulkEncoder =
  contrazip4
    (bulkEncoder $ idBulkEncoder getStakeAddressId)
    (bulkEncoder $ idBulkEncoder getPoolHashId)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: epoch_stake_progress
-- Description: Contains information about the progress of the epoch stake calculation.
data EpochStakeProgress = EpochStakeProgress
  { epochStakeProgressEpochNo :: !Word64 -- sqltype=word31type
  , epochStakeProgressCompleted :: !Bool
  }
  deriving (Show, Eq, Generic)

type instance Key EpochStakeProgress = EpochStakeProgressId

instance DbInfo EpochStakeProgress where
  uniqueFields _ = ["epoch_no"]
  unnestParamTypes _ =
    [ ("epoch_no", "bigint[]")
    , ("completed", "boolean[]")
    ]
