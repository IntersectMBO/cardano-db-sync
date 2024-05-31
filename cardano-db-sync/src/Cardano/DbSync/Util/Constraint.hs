{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Util.Constraint (
  constraintNameEpochStake,
  constraintNameReward,
  dbConstraintNamesExists,
  addConstraintsIfNotExist,
  addStakeConstraintsIfNotExist,
  addRewardConstraintsIfNotExist,
  addRewardTableConstraint,
  addEpochStakeTableConstraint,
) where

import Cardano.BM.Trace (logInfo)
import Cardano.Db (ManualDbConstraints (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, MonadAppDB (..), SyncEnv (..), askTrace)
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Database.Persist.EntityDef.Internal (EntityDef (..))
import Database.Persist.Names (ConstraintNameDB (..), EntityNameDB (..), FieldNameDB (..))
import Database.Persist.Postgresql (PersistEntity (..), SqlBackend)

constraintNameEpochStake :: ConstraintNameDB
constraintNameEpochStake = ConstraintNameDB "unique_epoch_stake"

constraintNameReward :: ConstraintNameDB
constraintNameReward = ConstraintNameDB "unique_reward"

-- We manually create unique constraints to improve insert speeds when syncing
-- This function checks if those constraints have already been created
dbConstraintNamesExists :: MonadIO m => SqlBackend -> m ManualDbConstraints
dbConstraintNamesExists sqlBackend = do
  runReaderT queryRewardAndEpochStakeConstraints sqlBackend

queryRewardAndEpochStakeConstraints ::
  MonadIO m =>
  ReaderT SqlBackend m ManualDbConstraints
queryRewardAndEpochStakeConstraints = do
  resEpochStake <- DB.queryHasConstraint constraintNameEpochStake
  resReward <- DB.queryHasConstraint constraintNameReward
  pure $
    ManualDbConstraints
      { dbConstraintRewards = resReward
      , dbConstraintEpochStake = resEpochStake
      }

addConstraintsIfNotExist :: App ()
addConstraintsIfNotExist = do
  addStakeConstraintsIfNotExist
  addRewardConstraintsIfNotExist

addStakeConstraintsIfNotExist :: App ()
addStakeConstraintsIfNotExist = do
  syncEnv <- ask
  mdbc <- liftIO . readTVarIO $ envDbConstraints syncEnv
  unless (dbConstraintEpochStake mdbc) addEpochStakeTableConstraint
  liftIO
    . atomically
    $ writeTVar (envDbConstraints syncEnv) (mdbc {dbConstraintEpochStake = True})

addRewardConstraintsIfNotExist :: App ()
addRewardConstraintsIfNotExist = do
  syncEnv <- ask
  mdbc <- liftIO . readTVarIO $ envDbConstraints syncEnv
  unless (dbConstraintRewards mdbc) addRewardTableConstraint
  liftIO
    . atomically
    $ writeTVar (envDbConstraints syncEnv) (mdbc {dbConstraintRewards = True})

addRewardTableConstraint :: App ()
addRewardTableConstraint = do
  let entityD = entityDef $ Proxy @DB.Reward
  dbQueryToApp $
    DB.alterTable
      entityD
      ( DB.AddUniqueConstraint
          constraintNameReward
          [ FieldNameDB "addr_id"
          , FieldNameDB "type"
          , FieldNameDB "earned_epoch"
          , FieldNameDB "pool_id"
          ]
      )
  logNewConstraint entityD (unConstraintNameDB constraintNameReward)

addEpochStakeTableConstraint :: App ()
addEpochStakeTableConstraint = do
  let entityD = entityDef $ Proxy @DB.EpochStake
  dbQueryToApp $
    DB.alterTable
      entityD
      ( DB.AddUniqueConstraint
          constraintNameEpochStake
          [ FieldNameDB "epoch_no"
          , FieldNameDB "addr_id"
          , FieldNameDB "pool_id"
          ]
      )
  logNewConstraint entityD (unConstraintNameDB constraintNameEpochStake)

logNewConstraint ::
  EntityDef ->
  Text ->
  App ()
logNewConstraint table constraintName = do
  trce <- askTrace
  liftIO $
    logInfo trce $
      "The table "
        <> unEntityNameDB (entityDB table)
        <> " was given a new unique constraint called "
        <> constraintName
