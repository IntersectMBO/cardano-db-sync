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
  rewardSourceDecoder,
  rewardSourceEncoder,
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import Data.Functor.Contravariant
-- import Database.Persist.Class (Unique)
-- import Database.Persist.Documentation (deriveShowFields, document, (#), (--^))
-- import Database.Persist.EntityDef.Internal (EntityDef (..))
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E

-----------------------------------------------------------------------------------------------------------------------------------
-- | STAKE DELEGATION
-- | These tables handle stake addresses, delegation, and reward
-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_address
Description: Contains information about stakeholder addresses.
-}
data StakeAddress = StakeAddress  -- Can be an address of a script hash
  { stakeAddressId :: !StakeAddressId -- noreference
    , stakeAddressHashRaw :: !ByteString        -- sqltype=addr29type
  , stakeAddressView :: !Text
  , stakeAddressScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  } deriving (Show, Eq, Generic)

stakeAddressDecoder :: D.Row StakeAddress
stakeAddressDecoder =
  StakeAddress
    <$> idDecoder StakeAddressId -- stakeAddressId
    <*> D.column (D.nonNullable D.bytea) -- stakeAddressHashRaw
    <*> D.column (D.nonNullable D.text) -- stakeAddressView
    <*> D.column (D.nullable D.bytea) -- stakeAddressScriptHash

stakeAddressEncoder :: E.Params StakeAddress
stakeAddressEncoder =
  mconcat
    [ stakeAddressId >$< idEncoder getStakeAddressId
    , stakeAddressHashRaw >$< E.param (E.nonNullable E.bytea)
    , stakeAddressView >$< E.param (E.nonNullable E.text)
    , stakeAddressScriptHash >$< E.param (E.nullable E.bytea)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: stake_registration
Description: Contains information about stakeholder registrations.
-}
data StakeRegistration = StakeRegistration
  { stakeRegistrationId :: !StakeRegistrationId
  , stakeRegistrationAddrId :: !StakeAddressId  -- noreference
  , stakeRegistrationCertIndex :: !Word16
  , stakeRegistrationEpochNo :: !Word64         -- sqltype=word31type
  , stakeRegistrationDeposit :: !(Maybe DbLovelace) -- sqltype=lovelace
  , stakeRegistrationTxId :: !TxId              -- noreference
  } deriving (Eq, Show, Generic)

stakeRegistrationDecoder :: D.Row StakeRegistration
stakeRegistrationDecoder =
  StakeRegistration
    <$> idDecoder StakeRegistrationId -- stakeRegistrationId
    <*> idDecoder StakeAddressId -- stakeRegistrationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeRegistrationCertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeRegistrationEpochNo
    <*> maybeDbLovelaceDecoder -- stakeRegistrationDeposit
    <*> idDecoder TxId -- stakeRegistrationTxId

stakeRegistrationEncoder :: E.Params StakeRegistration
stakeRegistrationEncoder =
  mconcat
    [ stakeRegistrationId >$< idEncoder getStakeRegistrationId
    , stakeRegistrationAddrId >$< idEncoder getStakeAddressId
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
  { stakeDeregistrationId :: !StakeDeregistrationId
  , stakeDeregistrationAddrId :: !StakeAddressId -- noreference
  , stakeDeregistrationCertIndex :: !Word16
  , stakeDeregistrationEpochNo :: !Word64       -- sqltype=word31type
  , stakeDeregistrationTxId :: !TxId            -- noreference
  , stakeDeregistrationRedeemerId :: !(Maybe RedeemerId) -- noreference
  } deriving (Eq, Show, Generic)

stakeDeregistrationDecoder :: D.Row StakeDeregistration
stakeDeregistrationDecoder =
  StakeDeregistration
    <$> idDecoder StakeDeregistrationId -- stakeDeregistrationId
    <*> idDecoder StakeAddressId -- stakeDeregistrationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeDeregistrationCertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeDeregistrationEpochNo
    <*> idDecoder TxId -- stakeDeregistrationTxId
    <*> maybeIdDecoder RedeemerId -- stakeDeregistrationRedeemerId

stakeDeregistrationEncoder :: E.Params StakeDeregistration
stakeDeregistrationEncoder =
  mconcat
    [ stakeDeregistrationId >$< idEncoder getStakeDeregistrationId
    , stakeDeregistrationAddrId >$< idEncoder getStakeAddressId
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
  { delegationId :: !DelegationId
  , delegationAddrId :: !StakeAddressId         -- noreference
  , delegationCertIndex :: !Word16
  , delegationPoolHashId :: !PoolHashId         -- noreference
  , delegationActiveEpochNo :: !Word64
  , delegationTxId :: !TxId                     -- noreference
  , delegationSlotNo :: !Word64                 -- sqltype=word63type
  , delegationRedeemerId :: !(Maybe RedeemerId)   -- noreference
  } deriving (Eq, Show, Generic)

delegationDecoder :: D.Row Delegation
delegationDecoder =
  Delegation
    <$> idDecoder DelegationId -- delegationId
    <*> idDecoder StakeAddressId -- delegationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- delegationCertIndex
    <*> idDecoder PoolHashId -- delegationPoolHashId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- delegationActiveEpochNo
    <*> idDecoder TxId -- delegationTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- delegationSlotNo
    <*> maybeIdDecoder RedeemerId -- delegationRedeemerId

delegationEncoder :: E.Params Delegation
delegationEncoder =
  mconcat
    [ delegationId >$< idEncoder getDelegationId
    , delegationAddrId >$< idEncoder getStakeAddressId
    , delegationCertIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
    , delegationPoolHashId >$< idEncoder getPoolHashId
    , delegationActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , delegationTxId >$< idEncoder getTxId
    , delegationSlotNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , delegationRedeemerId >$< maybeIdEncoder getRedeemerId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description: Reward, Stake and Treasury need to be obtained from the ledger state.
  The reward for each stake address and. This is not a balance, but a reward amount and the
  epoch in which the reward was earned.
  This table should never get rolled back.
-}
-----------------------------------------------------------------------------------------------------------------------------------
data Reward = Reward
  { rewardId :: !RewardId
  , rewardAddrId :: !StakeAddressId   -- noreference
  , rewardType :: !RewardSource       -- sqltype=rewardtype
  , rewardAmount :: !DbLovelace       -- sqltype=lovelace
  , rewardEarnedEpoch :: !Word64      -- generated="((CASE WHEN (type='refund') then spendable_epoch else (CASE WHEN spendable_epoch >= 2 then spendable_epoch-2 else 0 end) end) STORED)"
  , rewardSpendableEpoch :: !Word64
  , rewardPoolId :: !PoolHashId       -- noreference
  } deriving (Show, Eq, Generic)

rewardDecoder :: D.Row Reward
rewardDecoder =
  Reward
    <$> idDecoder RewardId -- rewardId
    <*> idDecoder StakeAddressId -- rewardAddrId
    <*> D.column (D.nonNullable rewardSourceDecoder) -- rewardType
    <*> dbLovelaceDecoder -- rewardAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardEarnedEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardSpendableEpoch
    <*> idDecoder PoolHashId -- rewardPoolId

rewardEncoder :: E.Params Reward
rewardEncoder =
  mconcat
    [ rewardId >$< idEncoder getRewardId
    , rewardAddrId >$< idEncoder getStakeAddressId
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
  { rewardRestId :: !RewardRestId
  , rewardRestAddrId :: !StakeAddressId -- noreference
  , rewardRestType :: !RewardSource     -- sqltype=rewardtype
  , rewardRestAmount :: !DbLovelace     -- sqltype=lovelace
  , rewardRestEarnedEpoch :: !Word64    -- generated="(CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)"
  , rewardRestSpendableEpoch :: !Word64
  } deriving (Show, Eq, Generic)

rewardRestDecoder :: D.Row RewardRest
rewardRestDecoder =
  RewardRest
    <$> idDecoder RewardRestId -- rewardRestId
    <*> idDecoder StakeAddressId -- rewardRestAddrId
    <*> D.column (D.nonNullable rewardSourceDecoder) -- rewardRestType
    <*> dbLovelaceDecoder -- rewardRestAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRestEarnedEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRestSpendableEpoch

rewardRestEncoder :: E.Params RewardRest
rewardRestEncoder =
  mconcat
    [ rewardRestId >$< idEncoder getRewardRestId
    , rewardRestAddrId >$< idEncoder getStakeAddressId
    , rewardRestType >$< E.param (E.nonNullable rewardSourceEncoder)
    , rewardRestAmount >$< dbLovelaceEncoder
    , rewardRestEarnedEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , rewardRestSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: epoch_stake
Description: Contains information about the stake of each stakeholder in each epoch.
  This table should never get rolled back
-}
-----------------------------------------------------------------------------------------------------------------------------------
data EpochStake = EpochStake
  { epochStakeId :: !EpochStakeId
  , epochStakeAddrId :: !StakeAddressId -- noreference
  , epochStakePoolId :: !PoolHashId     -- noreference
  , epochStakeAmount :: !DbLovelace     -- sqltype=lovelace
  , epochStakeEpochNo :: !Word64        -- sqltype=word31type
  } deriving (Show, Eq, Generic)
-- similar scenario as in Reward the constraint that was here is now set manually in
-- `applyAndInsertBlockMaybe` at a more optimal time.

epochStakeDecoder :: D.Row EpochStake
epochStakeDecoder =
  EpochStake
    <$> idDecoder EpochStakeId -- epochStakeId
    <*> idDecoder StakeAddressId -- epochStakeAddrId
    <*> idDecoder PoolHashId -- epochStakePoolId
    <*> dbLovelaceDecoder -- epochStakeAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStakeEpochNo

epochStakeEncoder :: E.Params EpochStake
epochStakeEncoder =
  mconcat
    [ epochStakeId >$< idEncoder getEpochStakeId
    , epochStakeAddrId >$< idEncoder getStakeAddressId
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
  { epochStakeProgressId :: !EpochStakeProgressId
  , epochStakeProgressEpochNo :: !Word64  -- sqltype=word31type
  , epochStakeProgressCompleted :: !Bool
  -- UniqueEpochStakeProgress epochNo
  } deriving (Show, Eq, Generic)

epochStakeProgressDecoder :: D.Row EpochStakeProgress
epochStakeProgressDecoder =
  EpochStakeProgress
    <$> idDecoder EpochStakeProgressId -- epochStakeProgressId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStakeProgressEpochNo
    <*> D.column (D.nonNullable D.bool) -- epochStakeProgressCompleted

epochStakeProgressEncoder :: E.Params EpochStakeProgress
epochStakeProgressEncoder =
  mconcat
    [ epochStakeProgressId >$< idEncoder getEpochStakeProgressId
    , epochStakeProgressEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , epochStakeProgressCompleted >$< E.param (E.nonNullable E.bool)
    ]
