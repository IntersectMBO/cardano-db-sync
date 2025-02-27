{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.StakeDeligation where

import Data.Word (Word64)
import qualified Hasql.Transaction as HsqlT

import Cardano.Db.Schema.Core.StakeDeligation (RewardRest(..), rewardRestEncoderMany)
import Cardano.Db.Schema.Ids (StakeAddressId)
import Cardano.Db.Statement.Helpers (runDbT, mkDbTransaction, bulkInsertNoReturn)
import Cardano.Db.Types (DbAction, DbLovelace, DbTxMode (..), RewardSource)
import Cardano.Prelude (MonadIO)

insertManyRewardRests :: MonadIO m => [RewardRest] -> DbAction m ()
insertManyRewardRests rewardRests =
  runDbT Write $ mkDbTransaction "insertManyRewardRests" $ insertManyRewardRestsStm rewardRests

insertManyRewardRestsStm :: [RewardRest] -> HsqlT.Transaction ()
insertManyRewardRestsStm rewardRests =
  bulkInsertNoReturn
    "reward_rest"
    ["addr_id", "type", "amount", "spendable_epoch", "earned_epoch"]
    ["int8[]", "rewardtype[]", "numeric[]", "int8[]", "int8[]"]
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

-- These tables handle stake addresses, delegation, and reward

-- stake_address
-- stake_registration
-- stake_deregistration
-- delegation
-- reward
-- epoch_stake
-- epoch_stake_progress
-- reward_rest
