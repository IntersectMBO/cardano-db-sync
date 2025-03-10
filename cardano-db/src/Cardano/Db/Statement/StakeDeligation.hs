{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.StakeDeligation where

import Data.Word (Word64)

import Cardano.Db (DelegationId)
import qualified Cardano.Db.Schema.Core.StakeDeligation as S
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction, DbLovelace, DbTransMode (..), RewardSource)
import Cardano.Prelude (MonadIO)

--------------------------------------------------------------------------------

-- | Deligation

--------------------------------------------------------------------------------
insertDelegationStmt :: HsqlS.Statement S.Delegation (Entity S.Delegation)
insertDelegationStmt =
  insert
    S.delegationEncoder
    (WithResult $ HsqlD.singleRow S.entityDelegationDecoder)

insertDelegation :: MonadIO m => S.Delegation -> DbAction m Id.DelegationId
insertDelegation delegation = do
  entity <- runDbSession (mkCallInfo "insertDelegation") $ HsqlS.statement delegation insertDelegationStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | EpochStake

--------------------------------------------------------------------------------
bulkInsertEpochStakeProgress :: MonadIO m => [S.EpochStakeProgress] -> DbAction m ()
bulkInsertEpochStakeProgress esps =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertEpochStakeProgress" $
      bulkInsertNoReturn
        extractEpochStakeProgress
        S.epochStakeProgressBulkEncoder
        esps
  where
    extractEpochStakeProgress :: [S.EpochStakeProgress] -> ([Id.StakeAddressId], [Word64], [Word64], [Word64], [Word64], [Word64])
    extractEpochStakeProgress xs =
      ( map epochStakeProgressAddrId xs
      , map epochStakeProgressEpochNo xs
      , map epochStakeProgressAmount xs
      , map epochStakeProgressDelegatedAmount xs
      , map epochStakeProgressPoolReward xs
      , map epochStakeProgressReserve xs
      )

--------------------------------------------------------------------------------

-- | Reward

--------------------------------------------------------------------------------

bulkInsertRewards :: MonadIO m => [Reward] -> DbAction m ()
bulkInsertRewards rewards =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertRewards" $
      bulkInsertNoReturn
        extractReward
        rewardBulkEncoder
        rewards
  where
    extractReward :: [Reward] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64])
    extractReward xs =
      ( map rewardAddrId xs
      , map rewardType xs
      , map rewardAmount xs
      , map rewardEarnedEpoch xs
      )

--------------------------------------------------------------------------------

-- | RewardRest

--------------------------------------------------------------------------------
bulkInsertRewardRests :: MonadIO m => [RewardRest] -> DbAction m ()
bulkInsertRewardRests rewardRests =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertRewardRests" $
      bulkInsertNoReturn
        extractRewardRest
        rewardRestBulkEncoder
        rewardRests
  where
    extractRewardRest :: [RewardRest] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64])
    extractRewardRest xs =
      ( map rewardRestAddrId xs
      , map rewardRestType xs
      , map rewardRestAmount xs
      , map rewardRestEarnedEpoch xs
      , map rewardRestSpendableEpoch xs
      )

--------------------------------------------------------------------------------

-- | StakeAddress

--------------------------------------------------------------------------------
insertStakeAddress :: MonadIO m => StakeAddress -> DbAction m Id.StakeAddressId
insertStakeAddress stakeAddress =
  runDbT TransWrite $
    mkDbTransaction "insertStakeAddress" $
      insertUnique
        stakeAddressdecoder
        (WithResult (HsqlD.singleRow $ idDecoder Id.StakeAddressId))
        stakeAddress

insertStakeDeregistration :: MonadIO m => StakeDeregistration -> DbAction m Id.StakeDeregistrationId
insertStakeDeregistration stakeDeregistration =
  runDbT TransWrite $
    mkDbTransaction "insertStakeDeregistration" $
      insertUnique
        stakeDeregistrationDecoder
        (WithResult (HsqlD.singleRow $ idDecoder Id.StakeDeregistrationId))
        stakeDeregistration

insertStakeRegistrationStmt :: HsqlS.Statement StakeRegistration (Entity StakeRegistration)
insertStakeRegistrationStmt =
  insert
    stakeRegistrationEncoder
    (WithResult $ HsqlD.singleRow stakeRegistrationDecoder)

insertStakeRegistration :: MonadIO m => StakeRegistration -> DbAction m Id.StakeRegistrationId
insertStakeRegistration stakeRegistration = do
  entity <- runDbSession (mkCallInfo "insertStakeRegistration") $ HsqlS.statement stakeRegistration insertStakeRegistrationStmt
  pure $ entityKey entity

bulkInsertEpochStakeProgress :: MonadIO m => [SEnP.EpochStakeProgress] -> DbAction m ()
bulkInsertEpochStakeProgress epochStakeProgress =
  runDbT TransWrite $
    mkDbTransaction "bulkInsertEpochStakeProgress" $
      bulkInsertCheckUnique
        SEnP.epochStakeProgressBulkEncoder
        NoResult
        epochStakeProgress

-- These tables handle stake addresses, delegation, and reward

-- delegation
-- epoch_stake
-- epoch_stake_progress
-- reward
-- reward_rest
-- stake_address
-- stake_deregistration
-- stake_registration
