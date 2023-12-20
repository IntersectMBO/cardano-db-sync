{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logInfo)
import Cardano.Db (ManualDbConstraints (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.Prelude (MonadIO (..), Proxy (..), ReaderT (runReaderT), atomically)
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Control.Monad (unless)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
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

addConstraintsIfNotExist ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  ReaderT SqlBackend m ()
addConstraintsIfNotExist syncEnv trce = do
  addStakeConstraintsIfNotExist syncEnv trce
  addRewardConstraintsIfNotExist syncEnv trce

addStakeConstraintsIfNotExist ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  ReaderT SqlBackend m ()
addStakeConstraintsIfNotExist syncEnv trce = do
  mdbc <- liftIO . readTVarIO $ envDbConstraints syncEnv
  unless (dbConstraintEpochStake mdbc) (addEpochStakeTableConstraint trce)
  liftIO
    . atomically
    $ writeTVar (envDbConstraints syncEnv) (mdbc {dbConstraintEpochStake = True})

addRewardConstraintsIfNotExist ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  ReaderT SqlBackend m ()
addRewardConstraintsIfNotExist syncEnv trce = do
  mdbc <- liftIO . readTVarIO $ envDbConstraints syncEnv
  unless (dbConstraintRewards mdbc) (addRewardTableConstraint trce)
  liftIO
    . atomically
    $ writeTVar (envDbConstraints syncEnv) (mdbc {dbConstraintRewards = True})

addRewardTableConstraint ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  ReaderT SqlBackend m ()
addRewardTableConstraint trce = do
  let entityD = entityDef $ Proxy @DB.Reward
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
  liftIO $ logNewConstraint trce entityD (unConstraintNameDB constraintNameReward)

addEpochStakeTableConstraint ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  ReaderT SqlBackend m ()
addEpochStakeTableConstraint trce = do
  let entityD = entityDef $ Proxy @DB.EpochStake
  DB.alterTable
    entityD
    ( DB.AddUniqueConstraint
        constraintNameEpochStake
        [ FieldNameDB "epoch_no"
        , FieldNameDB "addr_id"
        , FieldNameDB "pool_id"
        ]
    )
  liftIO $ logNewConstraint trce entityD (unConstraintNameDB constraintNameEpochStake)

logNewConstraint ::
  Trace IO Text ->
  EntityDef ->
  Text ->
  IO ()
logNewConstraint trce table constraintName =
  logInfo trce $
    "The table "
      <> unEntityNameDB (entityDB table)
      <> " was given a new unique constraint called "
      <> constraintName
