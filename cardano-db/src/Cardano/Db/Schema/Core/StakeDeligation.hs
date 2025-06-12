{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Schema.Core.StakeDeligation where

import Contravariant.Extras (contrazip2, contrazip4, contrazip5)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Types (textDecoder)
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (
  DbLovelace (..),
  RewardSource,
  dbLovelaceDecoder,
  dbLovelaceEncoder,
  maybeDbLovelaceDecoder,
  maybeDbLovelaceEncoder,
  rewardSourceDecoder,
  rewardSourceEncoder,
 )

-----------------------------------------------------------------------------------------------------------------------------------

-- | STAKE DELEGATION
-- | These tables handle stake addresses, delegation, and reward

-----------------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: stake_address
-- Description: Contains information about stakeholder addresses.
-----------------------------------------------------------------------------------------------------------------------------------
data StakeAddress = StakeAddress -- Can be an address of a script hash
  { stakeAddressHashRaw :: !ByteString -- sqltype=addr29type
  , stakeAddressView :: !Text
  , stakeAddressScriptHash :: !(Maybe ByteString) -- sqltype=hash28type
  }
  deriving (Show, Eq, Generic)

type instance Key StakeAddress = StakeAddressId
instance DbInfo StakeAddress where
  uniqueFields _ = ["hash_raw"]

entityStakeAddressDecoder :: D.Row (Entity StakeAddress)
entityStakeAddressDecoder =
  Entity
    <$> idDecoder StakeAddressId
    <*> stakeAddressDecoder

stakeAddressDecoder :: D.Row StakeAddress
stakeAddressDecoder =
  StakeAddress
    <$> D.column (D.nonNullable D.bytea) -- stakeAddressHashRaw
    <*> D.column (D.nonNullable textDecoder) -- stakeAddressView
    <*> D.column (D.nullable D.bytea) -- stakeAddressScriptHash

entityStakeAddressEncoder :: E.Params (Entity StakeAddress)
entityStakeAddressEncoder =
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
-- Table Name: stake_registration
-- Description: Contains information about stakeholder registrations.
-----------------------------------------------------------------------------------------------------------------------------------
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

entityStakeRegistrationDecoder :: D.Row (Entity StakeRegistration)
entityStakeRegistrationDecoder =
  Entity
    <$> idDecoder StakeRegistrationId
    <*> stakeRegistrationDecoder

stakeRegistrationDecoder :: D.Row StakeRegistration
stakeRegistrationDecoder =
  StakeRegistration
    <$> idDecoder StakeAddressId -- stakeRegistrationAddrId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int2) -- stakeRegistrationCertIndex
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- stakeRegistrationEpochNo
    <*> idDecoder TxId -- stakeRegistrationTxId
    <*> maybeDbLovelaceDecoder -- stakeRegistrationDeposit

entityStakeRegistrationEncoder :: E.Params (Entity StakeRegistration)
entityStakeRegistrationEncoder =
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
    , stakeRegistrationTxId >$< idEncoder getTxId
    , stakeRegistrationDeposit >$< maybeDbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: stake_deregistration
-- Description: Contains information about stakeholder deregistrations.
-----------------------------------------------------------------------------------------------------------------------------------
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

entityStakeDeregistrationDecoder :: D.Row (Entity StakeDeregistration)
entityStakeDeregistrationDecoder =
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

entityStakeDeregistrationEncoder :: E.Params (Entity StakeDeregistration)
entityStakeDeregistrationEncoder =
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
-- Table Name: delegation
-- Description:Contains information about stakeholder delegations, including the stakeholder's address and the pool to which they are delegating.
-----------------------------------------------------------------------------------------------------------------------------------
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

entityDelegationDecoder :: D.Row (Entity Delegation)
entityDelegationDecoder =
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

entityDelegationEncoder :: E.Params (Entity Delegation)
entityDelegationEncoder =
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
-- Table Name: reward
-- Description: Reward, Stake and Treasury need to be obtained from the ledger state.
--   The reward for each stake address and. This is not a balance, but a reward amount and the
--   epoch in which the reward was earned.
--   This table should never get rolled back.
-----------------------------------------------------------------------------------------------------------------------------------
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

rewardDecoder :: D.Row Reward
rewardDecoder =
  Reward
    <$> idDecoder StakeAddressId -- addr_id
    <*> D.column (D.nonNullable rewardSourceDecoder) -- type
    <*> dbLovelaceDecoder -- amount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- spendable_epoch
    <*> idDecoder PoolHashId -- pool_id
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- earned_epoch (generated)

rewardEncoder :: E.Params Reward
rewardEncoder =
  mconcat
    [ rewardAddrId >$< idEncoder getStakeAddressId
    , rewardType >$< E.param (E.nonNullable rewardSourceEncoder)
    , rewardAmount >$< dbLovelaceEncoder
    , rewardSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , rewardPoolId >$< idEncoder getPoolHashId
    ]

rewardBulkEncoder :: E.Params ([StakeAddressId], [RewardSource], [DbLovelace], [Word64], [PoolHashId])
rewardBulkEncoder =
  contrazip5
    (bulkEncoder $ idBulkEncoder getStakeAddressId)
    (bulkEncoder $ E.nonNullable rewardSourceEncoder)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ idBulkEncoder getPoolHashId)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: reward_rest
-- Description: Contains information about the remaining reward for each stakeholder.
-----------------------------------------------------------------------------------------------------------------------------------
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

entityRewardRestDecoder :: D.Row (Entity RewardRest)
entityRewardRestDecoder =
  Entity
    <$> idDecoder RewardRestId
    <*> rewardRestDecoder

rewardRestDecoder :: D.Row RewardRest
rewardRestDecoder =
  RewardRest
    <$> idDecoder StakeAddressId -- rewardRestAddrId
    <*> D.column (D.nonNullable rewardSourceDecoder) -- rewardRestType
    <*> dbLovelaceDecoder -- rewardRestAmount
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRestSpendableEpoch
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- rewardRestEarnedEpoch

entityRewardRestEncoder :: E.Params (Entity RewardRest)
entityRewardRestEncoder =
  mconcat
    [ entityKey >$< idEncoder getRewardRestId
    , entityVal >$< rewardRestEncoder
    ]

rewardRestEncoder :: E.Params RewardRest
rewardRestEncoder =
  mconcat
    [ rewardRestType >$< E.param (E.nonNullable rewardSourceEncoder)
    , rewardRestAmount >$< dbLovelaceEncoder
    , rewardRestSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

rewardRestBulkEncoder :: E.Params ([StakeAddressId], [RewardSource], [DbLovelace], [Word64])
rewardRestBulkEncoder =
  contrazip4
    (bulkEncoder $ idBulkEncoder getStakeAddressId)
    (bulkEncoder $ E.nonNullable rewardSourceEncoder)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: epoch_stake
-- Description: Contains information about the stake of each stakeholder in each epoch.
--   This table should never get rolled back
-----------------------------------------------------------------------------------------------------------------------------------
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
  bulkUniqueFields _ = ["addr_id", "pool_id", "epoch_no"]
  unnestParamTypes _ =
    [ ("addr_id", "bigint[]")
    , ("pool_id", "bigint[]")
    , ("amount", "bigint[]")
    , ("epoch_no", "bigint[]")
    ]

entityEpochStakeDecoder :: D.Row (Entity EpochStake)
entityEpochStakeDecoder =
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

entityEpochStakeEncoder :: E.Params (Entity EpochStake)
entityEpochStakeEncoder =
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

epochStakeBulkEncoder :: E.Params ([StakeAddressId], [PoolHashId], [DbLovelace], [Word64])
epochStakeBulkEncoder =
  contrazip4
    (bulkEncoder $ idBulkEncoder getStakeAddressId)
    (bulkEncoder $ idBulkEncoder getPoolHashId)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: epoch_stake_progress
-- Description: Contains information about the progress of the epoch stake calculation.
-----------------------------------------------------------------------------------------------------------------------------------
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

entityEpochStakeProgressDecoder :: D.Row (Entity EpochStakeProgress)
entityEpochStakeProgressDecoder =
  Entity
    <$> idDecoder EpochStakeProgressId
    <*> epochStakeProgressDecoder

epochStakeProgressDecoder :: D.Row EpochStakeProgress
epochStakeProgressDecoder =
  EpochStakeProgress
    <$> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- epochStakeProgressEpochNo
    <*> D.column (D.nonNullable D.bool) -- epochStakeProgressCompleted

entityEpochStakeProgressEncoder :: E.Params (Entity EpochStakeProgress)
entityEpochStakeProgressEncoder =
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
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nonNullable E.bool)
