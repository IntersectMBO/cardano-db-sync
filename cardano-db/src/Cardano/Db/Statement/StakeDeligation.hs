{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.StakeDeligation where

import Data.Word (Word64)
import qualified Hasql.Transaction as HsqlT

import Cardano.Db.Schema.Core.StakeDeligation (RewardRest(..), rewardRestEncoderMany, Delegation)
import Cardano.Db.Schema.Ids (StakeAddressId)
import Cardano.Db.Types (DbAction, DbLovelace, DbTransMode (..), RewardSource)
import Cardano.Prelude (MonadIO)
import Cardano.Db (DelegationId)

--------------------------------------------------------------------------------
-- | Deligation
--------------------------------------------------------------------------------
insertDelegation :: MonadIO m => Delegation -> DbAction m DelegationId
insertDelegation delegation =
  runDbT TransWrite $ mkDbTransaction "insertDelegation" $
    insert
      delegationEncoder
      (WithResult (HsqlD.singleRow $ idDecoder DelegationId))
      delegation

--------------------------------------------------------------------------------
-- | RewardRest
--------------------------------------------------------------------------------
insertManyRewardRests :: MonadIO m => [RewardRest] -> DbAction m ()
insertManyRewardRests rewardRests =
  runDbT TransWrite $ mkDbTransaction "insertManyRewardRests" $
    bulkInsertNoReturn
      extractRewardRest
      rewardRestEncoderMany
      rewardRests
  where
    extractRewardRest :: [RewardRest] -> ([StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64])
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
insertStakeAddress :: MonadIO m => StakeAddress -> DbAction m StakeAddressId
insertStakeAddress stakeAddress = runDbT TransWrite $ mkDbTransaction "insertStakeAddress" $
  insertUnique
    stakeAddressdecoder
    (WithResult (HsqlD.singleRow $ idDecoder StakeAddressId))
    stakeAddress
 fs
insertStakeDeregistration :: MonadIO m => StakeDeregistration -> DbAction m StakeDeregistrationId
insertStakeDeregistration stakeDeregistration = runDbT TransWrite $ mkDbTransaction "insertStakeDeregistration" $
  insertUnique
    stakeDeregistrationDecoder
    (WithResult (HsqlD.singleRow $ idDecoder StakeDeregistrationId))
    stakeDeregistration

insertStakeRegistration :: MonadIO m => StakeRegistration -> DbAction m StakeRegistrationId
insertStakeRegistration stakeRegistration = runDbT TransWrite $ mkDbTransaction "insertStakeRegistration" $
  insert
    stakeRegistrationDecoder
    (WithResult (HsqlD.singleRow $ idDecoder StakeRegistrationId))
    stakeRegistration

insertManyEpochStakeProgress :: MonadIO m => [SEnP.EpochStakeProgress] -> DbAction m ()
insertManyEpochStakeProgress epochStakeProgress = runDbT TransWrite $ mkDbTransaction "insertManyEpochStakeProgress" $
  insertManyCheckUnique
    SEnP.epochStakeProgressEncoderMany
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
