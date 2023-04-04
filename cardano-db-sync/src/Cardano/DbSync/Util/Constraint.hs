{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Util.Constraint (
  epochStakeConstraintName,
  rewardConstraintName,
) where

import Database.Persist.Names (ConstraintNameDB (..))

epochStakeConstraintName :: ConstraintNameDB
epochStakeConstraintName = ConstraintNameDB "unique_epoch_stake"

rewardConstraintName :: ConstraintNameDB
rewardConstraintName = ConstraintNameDB "unique_reward"
