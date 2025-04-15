{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.StakeDeligation where

import Data.Word (Word64)

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.StakeDeligation as SS
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk, insertCheckUnique)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction, DbCallInfo (..), DbLovelace, RewardSource)
import Cardano.Prelude (ByteString, MonadError (..), MonadIO)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

--------------------------------------------------------------------------------
-- Deligation
--------------------------------------------------------------------------------
insertDelegationStmt :: HsqlStmt.Statement SS.Delegation (Entity SS.Delegation)
insertDelegationStmt =
  insert
    SS.delegationEncoder
    (WithResult $ HsqlD.singleRow SS.entityDelegationDecoder)

insertDelegation :: MonadIO m => SS.Delegation -> DbAction m Id.DelegationId
insertDelegation delegation = do
  entity <- runDbSession (mkCallInfo "insertDelegation") $ HsqlSes.statement delegation insertDelegationStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- EpochStake
--------------------------------------------------------------------------------
insertBulkEpochStakeStmt :: HsqlStmt.Statement [SS.EpochStake] ()
insertBulkEpochStakeStmt =
  insertBulk
    extractEpochStake
    SS.epochStakeBulkEncoder
    NoResultBulk
  where
    extractEpochStake :: [SS.EpochStake] -> ([Id.StakeAddressId], [Id.PoolHashId], [DbLovelace], [Word64])
    extractEpochStake xs =
      ( map SS.epochStakeAddrId xs
      , map SS.epochStakePoolId xs
      , map SS.epochStakeAmount xs
      , map SS.epochStakeEpochNo xs
      )

insertBulkEpochStake :: MonadIO m => [SS.EpochStake] -> DbAction m ()
insertBulkEpochStake epochStakes =
  runDbSession (mkCallInfo "insertBulkEpochStake") $
    HsqlSes.statement epochStakes insertBulkEpochStakeStmt

--------------------------------------------------------------------------------
-- EpochProgress
--------------------------------------------------------------------------------
insertBulkEpochStakeProgressStmt :: HsqlStmt.Statement [SS.EpochStakeProgress] ()
insertBulkEpochStakeProgressStmt =
  insertBulk
    extractEpochStakeProgress
    SS.epochStakeProgressBulkEncoder
    NoResultBulk
  where
    extractEpochStakeProgress :: [SS.EpochStakeProgress] -> ([Word64], [Bool])
    extractEpochStakeProgress xs =
      ( map SS.epochStakeProgressEpochNo xs
      , map SS.epochStakeProgressCompleted xs
      )

insertBulkEpochStakeProgress :: MonadIO m => [SS.EpochStakeProgress] -> DbAction m ()
insertBulkEpochStakeProgress epochStakeProgresses =
  runDbSession (mkCallInfo "insertBulkEpochStakeProgress") $
    HsqlSes.statement epochStakeProgresses insertBulkEpochStakeProgressStmt

--------------------------------------------------------------------------------
-- Reward
--------------------------------------------------------------------------------
insertBulkRewardsStmt :: HsqlStmt.Statement [SS.Reward] ()
insertBulkRewardsStmt =
  insertBulk
    extractReward
    SS.rewardBulkEncoder
    NoResultBulk
  where
    extractReward :: [SS.Reward] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64], [Id.PoolHashId])
    extractReward xs =
      ( map SS.rewardAddrId xs
      , map SS.rewardType xs
      , map SS.rewardAmount xs
      , map SS.rewardEarnedEpoch xs
      , map SS.rewardSpendableEpoch xs
      , map SS.rewardPoolId xs
      )

insertBulkRewards :: MonadIO m => [SS.Reward] -> DbAction m ()
insertBulkRewards rewards =
  runDbSession (mkCallInfo "insertBulkRewards") $
    HsqlSes.statement rewards insertBulkRewardsStmt

--------------------------------------------------------------------------------
-- RewardRest
--------------------------------------------------------------------------------
insertBulkRewardRestsStmt :: HsqlStmt.Statement [SS.RewardRest] ()
insertBulkRewardRestsStmt =
  insertBulk
    extractRewardRest
    SS.rewardRestBulkEncoder
    NoResultBulk
  where
    extractRewardRest :: [SS.RewardRest] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64])
    extractRewardRest xs =
      ( map SS.rewardRestAddrId xs
      , map SS.rewardRestType xs
      , map SS.rewardRestAmount xs
      , map SS.rewardRestEarnedEpoch xs
      , map SS.rewardRestSpendableEpoch xs
      )

insertBulkRewardRests :: MonadIO m => [SS.RewardRest] -> DbAction m ()
insertBulkRewardRests rewardRests =
  runDbSession (mkCallInfo "insertBulkRewardRests") $
    HsqlSes.statement rewardRests insertBulkRewardRestsStmt

--------------------------------------------------------------------------------
-- StakeAddress
--------------------------------------------------------------------------------
insertStakeAddressStmt :: HsqlStmt.Statement SS.StakeAddress (Entity SS.StakeAddress)
insertStakeAddressStmt =
  insertCheckUnique
    SS.stakeAddressEncoder
    (WithResult $ HsqlD.singleRow SS.entityStakeAddressDecoder)

insertStakeAddress :: MonadIO m => SS.StakeAddress -> DbAction m Id.StakeAddressId
insertStakeAddress stakeAddress =
  runDbSession (mkCallInfo "insertStakeAddress") $ do
    entity <-
      HsqlSes.statement stakeAddress insertStakeAddressStmt
    pure $ entityKey entity

insertStakeDeregistrationStmt :: HsqlStmt.Statement SS.StakeDeregistration (Entity SS.StakeDeregistration)
insertStakeDeregistrationStmt =
  insertCheckUnique
    SS.stakeDeregistrationEncoder
    (WithResult $ HsqlD.singleRow SS.entityStakeDeregistrationDecoder)

insertStakeDeregistration :: MonadIO m => SS.StakeDeregistration -> DbAction m Id.StakeDeregistrationId
insertStakeDeregistration stakeDeregistration =
  runDbSession (mkCallInfo "insertStakeDeregistration") $ do
    entity <-
      HsqlSes.statement stakeDeregistration insertStakeDeregistrationStmt
    pure $ entityKey entity

insertStakeRegistrationStmt :: HsqlStmt.Statement SS.StakeRegistration (Entity SS.StakeRegistration)
insertStakeRegistrationStmt =
  insert
    SS.stakeRegistrationEncoder
    (WithResult $ HsqlD.singleRow SS.entityStakeRegistrationDecoder)

insertStakeRegistration :: MonadIO m => SS.StakeRegistration -> DbAction m Id.StakeRegistrationId
insertStakeRegistration stakeRegistration = do
  entity <-
    runDbSession (mkCallInfo "insertStakeRegistration") $
      HsqlSes.statement stakeRegistration insertStakeRegistrationStmt
  pure $ entityKey entity

-- | Queries
queryStakeAddressStmt :: HsqlStmt.Statement ByteString (Maybe Id.StakeAddressId)
queryStakeAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.StakeAddressId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM stake_address"
          , " WHERE hash_raw = $1"
          ]

queryStakeAddress :: MonadIO m => ByteString -> (ByteString -> Text.Text) -> DbAction m Id.StakeAddressId
queryStakeAddress addr toText = do
  result <- runDbSession callInfo $ HsqlSes.statement addr queryStakeAddressStmt
  case result of
    Just res -> pure res
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "queryStakeAddress"
    errorMsg = "StakeAddress " <> toText addr <> " not found"

-- These tables handle stake addresses, delegation, and reward

-- delegation
-- epoch_stake
-- epoch_stake_progress
-- reward
-- reward_rest
-- stake_address
-- stake_deregistration
-- stake_registration
