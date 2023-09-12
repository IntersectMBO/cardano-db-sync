{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Util.Constraint (
  constraintNameEpochStake,
  constraintNameReward,
  dbConstraintNamesExists,
  alterConstraints,
  addRewardTableConstraint,
  addEpochStakeTableConstraint,
) where

import Cardano.Db (ManualDbConstraints (..))
import qualified Cardano.Db as DB
import Cardano.Prelude (MonadIO, Proxy (..), ReaderT (runReaderT))
import Control.Monad (when)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Persist.Names (ConstraintNameDB (..), FieldNameDB (..))
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

alterConstraints ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  ManualDbConstraints ->
  ReaderT SqlBackend m ()
alterConstraints ManualDbConstraints {..} = do
  when dbConstraintRewards addRewardTableConstraint
  when dbConstraintEpochStake addEpochStakeTableConstraint

addRewardTableConstraint ::
  forall m. (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m ()
addRewardTableConstraint = do
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

addEpochStakeTableConstraint ::
  forall m. (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m ()
addEpochStakeTableConstraint = do
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
