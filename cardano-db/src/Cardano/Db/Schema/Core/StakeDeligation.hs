{-# LANGUAGE DeriveGeneric #-}

module Cardano.Db.Schema.Core.StakeDeligation where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Ids
import Cardano.Db.Types (
  DbLovelace(..),
  RewardSource,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  maybeDbLovelaceDecoder,
  maybeDbLovelaceEncoder,
  reward_SourceDecoder,
  dbLovelaceEncoder,
  reward_SourceEncoder, HasDbInfo,
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import Data.Functor.Contravariant
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E
import Contravariant.Extras (contrazip5)
import Cardano.Db.Statement.Helpers (manyEncoder)

-----------------------------------------------------------------------------------------------------------------------------------
-- | STAKE DELEGATION
-- | These tables handle stake addresses, delegation_, and reward_
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_address
Description: Contains information about stakeholder addresses.
-}
data StakeAddress = StakeAddress  -- Can be an address of a script hash
  { stakeAddress_Id :: !StakeAddressId -- noreference
  , stakeAddress_HashRaw :: !ByteString        -- sqltype=addr29type
  , stakeAddress_View :: !Text
  , stakeAddress_ScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  } deriving (Show, Eq, Generic)

instance HasDbInfo StakeAddress

stakeAddressDecoder :: D.Row StakeAddress
stakeAddressDecoder =
  StakeAddress
    <$> idDecoder StakeAddressId -- stakeAddress_Id
    <*> D.column (D.nonNullable D.bytea) -- stakeAddress_HashRaw
    <*> D.column (D.nonNullable D.text) -- stakeAddress_View
    <*> D.column (D.nullable D.bytea) -- stakeAddress_ScriptHash

stakeAddressEncoder :: E.Params StakeAddress
stakeAddressEncoder =
  mconcat
    [ stakeAddress_Id >$< idEncoder getStakeAddressId
    , stakeAddress_HashRaw >$< E.param (E.nonNullable E.bytea)
    , stakeAddress_View >$< E.param (E.nonNullable E.text)
    , stakeAddress_ScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_registration
Description: Contains information about stakeholder registrations.
-}
data StakeRegistration = StakeRegistration
  { stakeRegistration_Id :: !StakeRegistrationId
  , stakeRegistration_AddrId :: !StakeAddressId  -- noreference
  , stakeRegistration_CertIndex :: !Word16
  , stakeRegistration_EpochNo :: !Word64         -- sqltype=word31type
  , stakeRegistration_Deposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , stakeRegistration_TxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo StakeRegistration

stakeRegistrationDecoder :: D.Row StakeRegistration
stakeRegistrationDecoder =
  StakeRegistration
    <$> idDecoder StakeRegistrationId -- stakeRegistration_Id
    <*> idDecoder StakeAddressId -- stakeRegistration_AddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeRegistration_CertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeRegistration_EpochNo
    <*> maybeDbLovelaceDecoder -- stakeRegistration_Deposit
    <*> idDecoder TxId -- stakeRegistration_TxId

stakeRegistrationEncoder :: E.Params StakeRegistration
stakeRegistrationEncoder =
  mconcat
    [ stakeRegistration_Id >$< idEncoder getStakeRegistrationId
    , stakeRegistration_AddrId >$< idEncoder getStakeAddressId
    , stakeRegistration_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , stakeRegistration_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , stakeRegistration_Deposit >$< maybeDbLovelaceEncoder
    , stakeRegistration_TxId >$< idEncoder getTxId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_deregistration
Description: Contains information about stakeholder deregistrations.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data StakeDeregistration = StakeDeregistration
  { stakeDeregistration_Id :: !StakeDeregistrationId
  , stakeDeregistration_AddrId :: !StakeAddressId -- noreference
  , stakeDeregistration_CertIndex :: !Word16
  , stakeDeregistration_EpochNo :: !Word64       -- sqltype=word31type
  , stakeDeregistration_TxId :: !TxId            -- noreference
  , stakeDeregistration_RedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo StakeDeregistration

stakeDeregistrationDecoder :: D.Row StakeDeregistration
stakeDeregistrationDecoder =
  StakeDeregistration
    <$> idDecoder StakeDeregistrationId -- stakeDeregistration_Id
    <*> idDecoder StakeAddressId -- stakeDeregistration_AddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeDeregistration_CertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeDeregistration_EpochNo
    <*> idDecoder TxId -- stakeDeregistration_TxId
    <*> maybeIdDecoder RedeemerId -- stakeDeregistration_RedeemerId

stakeDeregistrationEncoder :: E.Params StakeDeregistration
stakeDeregistrationEncoder =
  mconcat
    [ stakeDeregistration_Id >$< idEncoder getStakeDeregistrationId
    , stakeDeregistration_AddrId >$< idEncoder getStakeAddressId
    , stakeDeregistration_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , stakeDeregistration_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , stakeDeregistration_TxId >$< idEncoder getTxId
    , stakeDeregistration_RedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: delegation
Description:Contains information about stakeholder delegations, including the stakeholder's address and the pool to which they are delegating.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Delegation = Delegation
  { delegation_Id :: !DelegationId
  , delegation_AddrId :: !StakeAddressId         -- noreference
  , delegation_CertIndex :: !Word16
  , delegation_PoolHashId :: !PoolHashId         -- noreference
  , delegation_ActiveEpochNo :: !Word64
  , delegation_TxId :: !TxId                     -- noreference
  , delegation_SlotNo :: !Word64                 -- sqltype=word63type
  , delegation_RedeemerId :: !(Maybe RedeemerId)   -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo Delegation

delegationDecoder :: D.Row Delegation
delegationDecoder =
  Delegation
    <$> idDecoder DelegationId -- delegation_Id
    <*> idDecoder StakeAddressId -- delegation_AddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- delegation_CertIndex
    <*> idDecoder PoolHashId -- delegation_PoolHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- delegation_ActiveEpochNo
    <*> idDecoder TxId -- delegation_TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- delegation_SlotNo
    <*> maybeIdDecoder RedeemerId -- delegation_RedeemerId

delegationEncoder :: E.Params Delegation
delegationEncoder =
  mconcat
    [ delegation_Id >$< idEncoder getDelegationId
    , delegation_AddrId >$< idEncoder getStakeAddressId
    , delegation_CertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegation_PoolHashId >$< idEncoder getPoolHashId
    , delegation_ActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , delegation_TxId >$< idEncoder getTxId
    , delegation_SlotNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , delegation_RedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reward
Description: Reward, Stake and Treasury need to be obtained from the ledger state.
  The reward_ for each stake address and. This is not a balance, but a reward amount and the
  epoch in which the reward was earned.
  This table should never get rolled back.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Reward = Reward
  { reward_Id :: !RewardId
  , reward_AddrId :: !StakeAddressId   -- noreference
  , reward_Type :: !RewardSource       -- sqltype=rewardtype
  , reward_Amount :: !DbLovelace       -- sqltype=lovelace
  , reward_EarnedEpoch :: !Word64      -- generated="((CASE WHEN (type='refund') then spendable_epoch else (CASE WHEN spendable_epoch >= 2 then spendable_epoch-2 else 0 end) end) STORED)"
  , reward_SpendableEpoch :: !Word64
  , reward_PoolId :: !PoolHashId       -- noreference
  } deriving (Show, Eq, Generic)

instance HasDbInfo Reward

rewardDecoder :: D.Row Reward
rewardDecoder =
  Reward
    <$> idDecoder RewardId -- reward_Id
    <*> idDecoder StakeAddressId -- reward_AddrId
    <*> D.column (D.nonNullable reward_SourceDecoder) -- reward_Type
    <*> dbLovelaceDecoder -- reward_Amount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- reward_EarnedEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- reward_SpendableEpoch
    <*> idDecoder PoolHashId -- reward_PoolId

rewardEncoder :: E.Params Reward
rewardEncoder =
  mconcat
    [ reward_Id >$< idEncoder getRewardId
    , reward_AddrId >$< idEncoder getStakeAddressId
    , reward_Type >$< E.param (E.nonNullable reward_SourceEncoder)
    , reward_Amount >$< dbLovelaceEncoder
    , reward_EarnedEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , reward_SpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , reward_PoolId >$< idEncoder getPoolHashId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: reward_rest
Description: Contains information about the remaining reward_ for each stakeholder.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data RewardRest = RewardRest
  { rewardRest_AddrId :: !StakeAddressId -- noreference
  , rewardRest_Type :: !RewardSource     -- sqltype=rewardtype
  , rewardRest_Amount :: !DbLovelace     -- sqltype=lovelace
  , rewardRest_EarnedEpoch :: !Word64    -- generated="(CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)"
  , rewardRest_SpendableEpoch :: !Word64
  } deriving (Show, Eq, Generic)

instance HasDbInfo RewardRest

rewardRestDecoder :: D.Row RewardRest
rewardRestDecoder =
  RewardRest
    <$> idDecoder StakeAddressId -- rewardRest_AddrId
    <*> D.column (D.nonNullable reward_SourceDecoder) -- rewardRest_Type
    <*> dbLovelaceDecoder -- rewardRest_Amount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRest_EarnedEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRest_SpendableEpoch

rewardRestEncoder :: E.Params RewardRest
rewardRestEncoder =
  mconcat
    [ rewardRest_AddrId >$< idEncoder getStakeAddressId
    , rewardRest_Type >$< E.param (E.nonNullable reward_SourceEncoder)
    , rewardRest_Amount >$< dbLovelaceEncoder
    , rewardRest_EarnedEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , rewardRest_SpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

rewardRestEncoderMany :: E.Params ([StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64])
rewardRestEncoderMany =
  contrazip5
    (manyEncoder $ idEncoderMany getStakeAddressId)
    (manyEncoder $ E.nonNullable reward_SourceEncoder)
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
  { epochStake_Id :: !EpochStakeId
  , epochStake_AddrId :: !StakeAddressId -- noreference
  , epochStake_PoolId :: !PoolHashId     -- noreference
  , epochStake_Amount :: !DbLovelace     -- sqltype=lovelace
  , epochStake_EpochNo :: !Word64        -- sqltype=word31type
  } deriving (Show, Eq, Generic)
-- similar scenario as in Reward the constraint that was here is now set manually in
-- `applyAndInsertBlockMaybe` at a more optimal time.

instance HasDbInfo EpochStake

epochStakeDecoder :: D.Row EpochStake
epochStakeDecoder =
  EpochStake
    <$> idDecoder EpochStakeId -- epochStake_Id
    <*> idDecoder StakeAddressId -- epochStake_AddrId
    <*> idDecoder PoolHashId -- epochStake_PoolId
    <*> dbLovelaceDecoder -- epochStake_Amount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStake_EpochNo

epochStakeEncoder :: E.Params EpochStake
epochStakeEncoder =
  mconcat
    [ epochStake_Id >$< idEncoder getEpochStakeId
    , epochStake_AddrId >$< idEncoder getStakeAddressId
    , epochStake_PoolId >$< idEncoder getPoolHashId
    , epochStake_Amount >$< dbLovelaceEncoder
    , epochStake_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_stake_progress
Description: Contains information about the progress of the epoch stake calculation.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data EpochStakeProgress = EpochStakeProgress
  { epochStakeProgress_Id :: !EpochStakeProgressId
  , epochStakeProgress_EpochNo :: !Word64  -- sqltype=word31type
  , epochStakeProgress_Completed :: !Bool
  -- UniqueEpochStakeProgress epochNo
  } deriving (Show, Eq, Generic)

instance HasDbInfo EpochStakeProgress

epochStakeProgressDecoder :: D.Row EpochStakeProgress
epochStakeProgressDecoder =
  EpochStakeProgress
    <$> idDecoder EpochStakeProgressId -- epochStakeProgress_Id
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStakeProgress_EpochNo
    <*> D.column (D.nonNullable D.bool) -- epochStakeProgress_Completed

epochStakeProgressEncoder :: E.Params EpochStakeProgress
epochStakeProgressEncoder =
  mconcat
    [ epochStakeProgress_Id >$< idEncoder getEpochStakeProgressId
    , epochStakeProgress_EpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochStakeProgress_Completed >$< E.param (E.nonNullable E.bool)
    ]
