{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Schema.Core.StakeDeligation where

import Contravariant.Extras (contrazip5, contrazip2)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Statement.Function.Core (manyEncoder)
import Cardano.Db.Statement.Types (DbInfo(..), Key, Entity (..))
import Cardano.Db.Types (
  DbLovelace(..),
  RewardSource,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  maybeDbLovelaceDecoder,
  maybeDbLovelaceEncoder,
  rewardSourceDecoder,
  dbLovelaceEncoder,
  rewardSourceEncoder,
 )

-----------------------------------------------------------------------------------------------------------------------------------
-- | STAKE DELEGATION
-- | These tables handle stake addresses, delegation, and reward
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_address
Description: Contains information about stakeholder addresses.
-}
data StakeAddress = StakeAddress  -- Can be an address of a script hash
  { stakeAddressHashRaw :: !ByteString        -- sqltype=addr29type
  , stakeAddressView :: !Text
  , stakeAddressScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  } deriving (Show, Eq, Generic)

instance DbInfo StakeAddress where
  uniqueFields _ = ["hash_raw"]

type instance Key StakeAddress = StakeAddressId

entityNameStakeAddressDecoder :: D.Row (Entity StakeAddress)
entityNameStakeAddressDecoder =
  Entity
    <$> idDecoder StakeAddressId
    <*> stakeAddressDecoder

stakeAddressDecoder :: D.Row StakeAddress
stakeAddressDecoder =
  StakeAddress
    <$> D.column (D.nonNullable D.bytea) -- stakeAddressHashRaw
    <*> D.column (D.nonNullable D.text) -- stakeAddressView
    <*> D.column (D.nullable D.bytea) -- stakeAddressScriptHash

entityNameStakeAddressEncoder :: E.Params (Entity StakeAddress)
entityNameStakeAddressEncoder =
  mconcat
    [ entityKey >$< idEncoder getStakeAddressId
    , entityVal >$< stakeAddressEncoder
    ]

stakeAddressEncoder :: E.Params StakeAddress
stakeAddressEncoder =
  mconcat
    [ stakeAddressHashRaw >$< E.param (E.nonNullable E.bytea)
    , stakeAddressView >$< E.param (E.nonNullable E.text)
    , stakeAddressScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_registration
Description: Contains information about stakeholder registrations.
-}
data StakeRegistration = StakeRegistration
  { stakeRegistrationAddrId :: !StakeAddressId  -- noreference
  , stakeRegistrationCertIndex :: !Word16
  , stakeRegistrationEpochNo :: !Word64         -- sqltype=word31type
  , stakeRegistrationDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , stakeRegistrationTxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

instance DbInfo StakeRegistration

type instance Key StakeRegistration = StakeRegistrationId

entityNameStakeRegistrationDecoder :: D.Row (Entity StakeRegistration)
entityNameStakeRegistrationDecoder =
  Entity
    <$> idDecoder StakeRegistrationId
    <*> stakeRegistrationDecoder

stakeRegistrationDecoder :: D.Row StakeRegistration
stakeRegistrationDecoder =
  StakeRegistration
    <$> idDecoder StakeAddressId -- stakeRegistrationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeRegistrationCertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeRegistrationEpochNo
    <*> maybeDbLovelaceDecoder -- stakeRegistrationDeposit
    <*> idDecoder TxId -- stakeRegistrationTxId

entityNameStakeRegistrationEncoder :: E.Params (Entity StakeRegistration)
entityNameStakeRegistrationEncoder =
  mconcat
    [ entityKey >$< idEncoder getStakeRegistrationId
    , entityVal >$< stakeRegistrationEncoder
    ]

stakeRegistrationEncoder :: E.Params StakeRegistration
stakeRegistrationEncoder =
  mconcat
    [ stakeRegistrationAddrId >$< idEncoder getStakeAddressId
    , stakeRegistrationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , stakeRegistrationEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , stakeRegistrationDeposit >$< maybeDbLovelaceEncoder
    , stakeRegistrationTxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_deregistration
Description: Contains information about stakeholder deregistrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data StakeDeregistration = StakeDeregistration
  { stakeDeregistrationAddrId :: !StakeAddressId -- noreference
  , stakeDeregistrationCertIndex :: !Word16
  , stakeDeregistrationEpochNo :: !Word64       -- sqltype=word31type
  , stakeDeregistrationTxId :: !TxId            -- noreference
  , stakeDeregistrationRedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

instance DbInfo StakeDeregistration

type instance Key StakeDeregistration = StakeDeregistrationId

entityNameStakeDeregistrationDecoder :: D.Row (Entity StakeDeregistration)
entityNameStakeDeregistrationDecoder =
  Entity
    <$> idDecoder StakeDeregistrationId
    <*> stakeDeregistrationDecoder

stakeDeregistrationDecoder :: D.Row StakeDeregistration
stakeDeregistrationDecoder =
  StakeDeregistration
    <$> idDecoder StakeAddressId -- stakeDeregistrationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeDeregistrationCertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeDeregistrationEpochNo
    <*> idDecoder TxId -- stakeDeregistrationTxId
    <*> maybeIdDecoder RedeemerId -- stakeDeregistrationRedeemerId

entityNameStakeDeregistrationEncoder :: E.Params (Entity StakeDeregistration)
entityNameStakeDeregistrationEncoder =
  mconcat
    [ entityKey >$< idEncoder getStakeDeregistrationId
    , entityVal >$< stakeDeregistrationEncoder
    ]

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
{-|
Table Name: delegation
Description:Contains information about stakeholder delegations, including the stakeholder's address and the pool to which they are delegating.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Delegation = Delegation
  { delegationAddrId :: !StakeAddressId         -- noreference
  , delegationCertIndex :: !Word16
  , delegationPoolHashId :: !PoolHashId         -- noreference
  , delegationActiveEpochNo :: !Word64
  , delegationTxId :: !TxId                     -- noreference
  , delegationSlotNo :: !Word64                 -- sqltype=word63type
  , delegationRedeemerId :: !(Maybe RedeemerId)   -- noreference
  } deriving (Eq, Show, Generic)

instance DbInfo Delegation

type instance Key Delegation = DelegationId

entityNameDelegationDecoder :: D.Row (Entity Delegation)
entityNameDelegationDecoder =
  Entity
    <$> idDecoder DelegationId
    <*> delegationDecoder

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

entityNameDelegationEncoder :: E.Params (Entity Delegation)
entityNameDelegationEncoder =
  mconcat
    [ entityKey >$< idEncoder getDelegationId
    , entityVal >$< delegationEncoder
    ]

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
{-|
Table Name: reward
Description: Reward, Stake and Treasury need to be obtained from the ledger state.
  The reward for each stake address and. This is not a balance, but a reward amount and the
  epoch in which the reward was earned.
  This table should never get rolled back.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Reward = Reward
  { rewardAddrId :: !StakeAddressId   -- noreference
  , rewardType :: !RewardSource       -- sqltype=rewardtype
  , rewardAmount :: !DbLovelace       -- sqltype=lovelace
  , rewardEarnedEpoch :: !Word64      -- generated="((CASE WHEN (type='refund') then spendable_epoch else (CASE WHEN spendable_epoch >= 2 then spendable_epoch-2 else 0 end) end) STORED)"
  , rewardSpendableEpoch :: !Word64
  , rewardPoolId :: !PoolHashId       -- noreference
  } deriving (Show, Eq, Generic)

instance DbInfo Reward

type instance Key Reward = RewardId

entityNameRewardDecoder :: D.Row (Entity Reward)
entityNameRewardDecoder =
  Entity
    <$> idDecoder RewardId
    <*> rewardDecoder

rewardDecoder :: D.Row Reward
rewardDecoder =
  Reward
    <$> idDecoder StakeAddressId -- rewardAddrId
    <*> D.column (D.nonNullable rewardSourceDecoder) -- rewardType
    <*> dbLovelaceDecoder -- rewardAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardEarnedEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardSpendableEpoch
    <*> idDecoder PoolHashId -- rewardPoolId

entityNameRewardEncoder :: E.Params (Entity Reward)
entityNameRewardEncoder =
  mconcat
    [ entityKey >$< idEncoder getRewardId
    , entityVal >$< rewardEncoder
    ]

rewardEncoder :: E.Params Reward
rewardEncoder =
  mconcat
    [ rewardAddrId >$< idEncoder getStakeAddressId
    , rewardType >$< E.param (E.nonNullable rewardSourceEncoder)
    , rewardAmount >$< dbLovelaceEncoder
    , rewardEarnedEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , rewardSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , rewardPoolId >$< idEncoder getPoolHashId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reward_rest
Description: Contains information about the remaining reward for each stakeholder.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data RewardRest = RewardRest
  { rewardRestType :: !RewardSource     -- sqltype=rewardtype
  , rewardRestAmount :: !DbLovelace     -- sqltype=lovelace
  , rewardRestEarnedEpoch :: !Word64    -- generated="(CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)"
  , rewardRestSpendableEpoch :: !Word64
  } deriving (Show, Eq, Generic)

instance DbInfo RewardRest

type instance Key RewardRest = RewardRestId

entityNameRewardRestDecoder :: D.Row (Entity RewardRest)
entityNameRewardRestDecoder =
  Entity
    <$> idDecoder RewardRestId
    <*> rewardRestDecoder

rewardRestDecoder :: D.Row RewardRest
rewardRestDecoder =
  RewardRest
    <$> D.column (D.nonNullable rewardSourceDecoder) -- rewardRestType
    <*> dbLovelaceDecoder -- rewardRestAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRestEarnedEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRestSpendableEpoch

entityNameRewardRestEncoder :: E.Params (Entity RewardRest)
entityNameRewardRestEncoder =
  mconcat
    [ entityKey >$< idEncoder getRewardRestId
    , entityVal >$< rewardRestEncoder
    ]

rewardRestEncoder :: E.Params RewardRest
rewardRestEncoder =
  mconcat
    [ rewardRestType >$< E.param (E.nonNullable rewardSourceEncoder)
    , rewardRestAmount >$< dbLovelaceEncoder
    , rewardRestEarnedEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , rewardRestSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

rewardRestBulkEncoder :: E.Params ([StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64])
rewardRestBulkEncoder =
  contrazip5
    (manyEncoder $ idBulkEncoder getStakeAddressId)
    (manyEncoder $ E.nonNullable rewardSourceEncoder)
    (manyEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_stake
Description: Contains information about the stake of each stakeholder in each epoch.
  This table should never get rolled back
-}
-----------------------------------------------------------------------------------------------------------------------------------
data EpochStake = EpochStake
  { epochStakeAddrId :: !StakeAddressId -- noreference
  , epochStakePoolId :: !PoolHashId     -- noreference
  , epochStakeAmount :: !DbLovelace     -- sqltype=lovelace
  , epochStakeEpochNo :: !Word64        -- sqltype=word31type
  } deriving (Show, Eq, Generic)
-- similar scenario as in Reward the constraint that was here is now set manually in
-- `applyAndInsertBlockMaybe` at a more optimal time.

instance DbInfo EpochStake

type instance Key EpochStake = EpochStakeId

entityNameEpochStakeDecoder :: D.Row (Entity EpochStake)
entityNameEpochStakeDecoder =
  Entity
    <$> idDecoder EpochStakeId
    <*> epochStakeDecoder

epochStakeDecoder :: D.Row EpochStake
epochStakeDecoder =
  EpochStake
    <$> idDecoder StakeAddressId -- epochStakeAddrId
    <*> idDecoder PoolHashId -- epochStakePoolId
    <*> dbLovelaceDecoder -- epochStakeAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStakeEpochNo

entityNameEpochStakeEncoder :: E.Params (Entity EpochStake)
entityNameEpochStakeEncoder =
  mconcat
    [ entityKey >$< idEncoder getEpochStakeId
    , entityVal >$< epochStakeEncoder
    ]

epochStakeEncoder :: E.Params EpochStake
epochStakeEncoder =
  mconcat
    [ epochStakeAddrId >$< idEncoder getStakeAddressId
    , epochStakePoolId >$< idEncoder getPoolHashId
    , epochStakeAmount >$< dbLovelaceEncoder
    , epochStakeEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_stake_progress
Description: Contains information about the progress of the epoch stake calculation.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data EpochStakeProgress = EpochStakeProgress
  { epochStakeProgressEpochNo :: !Word64  -- sqltype=word31type
  , epochStakeProgressCompleted :: !Bool
  } deriving (Show, Eq, Generic)

instance DbInfo EpochStakeProgress where
  uniqueFields _ = ["epoch_no"]

type instance Key EpochStakeProgress = EpochStakeProgressId

entityNameEpochStakeProgressDecoder :: D.Row (Entity EpochStakeProgress)
entityNameEpochStakeProgressDecoder =
  Entity
    <$> idDecoder EpochStakeProgressId
    <*> epochStakeProgressDecoder

epochStakeProgressDecoder :: D.Row EpochStakeProgress
epochStakeProgressDecoder =
  EpochStakeProgress
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStakeProgressEpochNo
    <*> D.column (D.nonNullable D.bool) -- epochStakeProgressCompleted

entityNameEpochStakeProgressEncoder :: E.Params (Entity EpochStakeProgress)
entityNameEpochStakeProgressEncoder =
  mconcat
    [ entityKey >$< idEncoder getEpochStakeProgressId
    , entityVal >$< epochStakeProgressEncoder
    ]

epochStakeProgressEncoder :: E.Params EpochStakeProgress
epochStakeProgressEncoder =
  mconcat
    [ epochStakeProgressEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochStakeProgressCompleted >$< E.param (E.nonNullable E.bool)
    ]

epochStakeProgressBulkEncoder :: E.Params ([Word64], [Bool])
epochStakeProgressBulkEncoder =
  contrazip2
    (manyEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (manyEncoder $ E.nonNullable E.bool)
