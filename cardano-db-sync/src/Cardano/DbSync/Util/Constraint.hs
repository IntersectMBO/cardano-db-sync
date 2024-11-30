{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Util.Constraint (
  constraintNameEpochStake,
  constraintNameReward,
  dbConstraintNamesExists,
  queryIsJsonbInSchema,
  addConstraintsIfNotExist,
  addStakeConstraintsIfNotExist,
  addRewardConstraintsIfNotExist,
  addRewardTableConstraint,
  addEpochStakeTableConstraint,
) where

import Cardano.BM.Data.Trace (Trace)
import qualified Cardano.BM.Tracing as BM
import Cardano.Db (ManualDbConstraints (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Functions (getSeverity)
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Util.Logging (LogContext (..), initLogCtx, logInfoCtx)
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

queryIsJsonbInSchema :: MonadIO m => SqlBackend -> m Bool
queryIsJsonbInSchema sqlBackend = do
  runReaderT DB.queryJsonbInSchemaExists sqlBackend

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
  unless (dbConstraintEpochStake mdbc) (addEpochStakeTableConstraint syncEnv trce)
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
  severity <- liftIO $ getSeverity syncEnv
  unless (dbConstraintRewards mdbc) (addRewardTableConstraint trce severity)
  liftIO
    . atomically
    $ writeTVar (envDbConstraints syncEnv) (mdbc {dbConstraintRewards = True})

addRewardTableConstraint ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  BM.Severity ->
  ReaderT SqlBackend m ()
addRewardTableConstraint trce severity = do
  let entityD = entityDef $ Proxy @DB.Reward
      logCtx = initLogCtx severity "addRewardTableConstraint" "Cardano.DbSync.Util"
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
  liftIO $ logNewConstraint trce logCtx entityD (unConstraintNameDB constraintNameReward)

addEpochStakeTableConstraint ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  ReaderT SqlBackend m ()
addEpochStakeTableConstraint syncEnv trce = do
  severity <- liftIO $ getSeverity syncEnv
  let entityD = entityDef $ Proxy @DB.EpochStake
      logCtx = initLogCtx severity "addEpochStakeTableConstraint" "Cardano.DbSync.Util"
  DB.alterTable
    entityD
    ( DB.AddUniqueConstraint
        constraintNameEpochStake
        [ FieldNameDB "epoch_no"
        , FieldNameDB "addr_id"
        , FieldNameDB "pool_id"
        ]
    )
  liftIO $ logNewConstraint trce logCtx entityD (unConstraintNameDB constraintNameEpochStake)

logNewConstraint ::
  Trace IO Text ->
  LogContext ->
  EntityDef ->
  Text ->
  IO ()
logNewConstraint trce logCtx table constraintName =
  logInfoCtx trce $
    logCtx
      { lcMessage =
          "The table "
            <> unEntityNameDB (entityDB table)
            <> " was given a new unique constraint called "
            <> constraintName
      }
