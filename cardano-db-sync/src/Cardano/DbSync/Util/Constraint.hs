-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Util.Constraint where
--   constraintNameEpochStake,
--   constraintNameReward,
--   dbConstraintNamesExists,
--   queryIsJsonbInSchema,
--   addConstraintsIfNotExist,
--   addStakeConstraintsIfNotExist,
--   addRewardConstraintsIfNotExist,
--   addRewardTableConstraint,
--   addEpochStakeTableConstraint,
-- ) where

-- import Cardano.BM.Data.Trace (Trace)
-- import Cardano.BM.Trace (logInfo)
-- import Cardano.Db (ManualDbConstraints (..))
-- import qualified Cardano.Db as DB
-- import Cardano.DbSync.Api.Types (SyncEnv (..))
-- import Cardano.Prelude (MonadIO (..), Proxy (..), ReaderT (runReaderT), atomically)
-- import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
-- import Control.Monad (unless)
-- import Control.Monad.Trans.Control (MonadBaseControl)
-- import Data.Text (Text)
-- import Database.Persist.EntityDef.Internal (EntityDef (..))
-- import Database.Persist.Names (ConstraintNameDB (..), EntityNameDB (..), FieldNameDB (..))
-- import Database.Persist.Postgresql (PersistEntity (..), SqlBackend)

-- import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
-- import Control.Monad (unless)
-- import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified DB.Constraint as DB
-- import Data.Proxy (Proxy (..))
-- import qualified Data.Text as Text

-- import qualified App.Types.DB as AppDB (EpochStake, Reward)
-- import DB.Core (DbEvent, DbInfo, tableName, validateColumn)

-- -- | Tracks which manual constraints exist in the database
-- data ManualDbConstraints = ManualDbConstraints
--   { dbConstraintRewards :: !Bool
--   , dbConstraintEpochStake :: !Bool
--   }

-- -- | Constraint name for EpochStake table
-- constraintNameEpochStake :: DB.ConstraintNameDB
-- constraintNameEpochStake = DB.ConstraintNameDB "unique_epoch_stake"

-- -- | Constraint name for Reward table
-- constraintNameReward :: DB.ConstraintNameDB
-- constraintNameReward = DB.ConstraintNameDB "unique_reward"

-- -- | Function to query which constraints exist
-- queryRewardAndEpochStakeConstraints :: MonadIO m => DbEvent m ManualDbConstraints
-- queryRewardAndEpochStakeConstraints = do
--   resEpochStake <- DB.queryHasConstraint constraintNameEpochStake
--   resReward <- DB.queryHasConstraint constraintNameReward
--   pure $
--     ManualDbConstraints
--       { dbConstraintRewards = resReward
--       , dbConstraintEpochStake = resEpochStake
--       }

-- -- | Check if jsonb type exists in the schema
-- -- This is a placeholder - implement according to your needs
-- queryIsJsonbInSchema :: MonadIO m => DbEvent m Bool
-- queryIsJsonbInSchema = pure True -- Implement with actual check

-- -- | Generic function to create unique constraints for any DbInfo type
-- addUniqueConstraint ::
--   forall a m.
--   (DbInfo a, MonadIO m) =>
--   -- | Constraint name
--   DB.ConstraintNameDB ->
--   -- | Column names to include in constraint
--   [Text.Text] ->
--   -- | Logger parameter
--   Text.Text ->
--   DbEvent m ()
-- addUniqueConstraint constraintName columnsList logger = do
--   let tbl = tableName (Proxy @a)
--       -- Validate each column name against the DbInfo
--       fields = map (DB.FieldNameDB . validateColumn @a) columnsList
--   DB.alterTableAddConstraint tbl constraintName fields

-- -- Logging would be implemented here

-- -- | Add constraints for EpochStake table
-- addEpochStakeTableConstraint ::
--   MonadIO m =>
--   -- | Logger parameter
--   Text.Text ->
--   DbEvent m ()
-- addEpochStakeTableConstraint logger =
--   addUniqueConstraint @AppDB.EpochStake
--     constraintNameEpochStake
--     ["epoch_no", "addr_id", "pool_id"]
--     logger

-- -- | Add constraints for Reward table
-- addRewardTableConstraint ::
--   MonadIO m =>
--   -- | Logger parameter
--   Text.Text ->
--   DbEvent m ()
-- addRewardTableConstraint logger =
--   addUniqueConstraint @AppDB.Reward
--     constraintNameReward
--     ["addr_id", "type", "earned_epoch", "pool_id"]
--     logger

-- -- | Add all constraints if needed
-- addConstraintsIfNotExist ::
--   MonadIO m =>
--   -- | TVar for tracking constraint state
--   TVar ManualDbConstraints ->
--   -- | Logger parameter
--   Text.Text ->
--   DbEvent m ()
-- addConstraintsIfNotExist envDbConstraints logger = do
--   addStakeConstraintsIfNotExist envDbConstraints logger
--   addRewardConstraintsIfNotExist envDbConstraints logger

-- -- | Add EpochStake constraints if not exist
-- addStakeConstraintsIfNotExist ::
--   MonadIO m =>
--   TVar ManualDbConstraints ->
--   Text.Text ->
--   DbEvent m ()
-- addStakeConstraintsIfNotExist envDbConstraints logger = do
--   mdbc <- liftIO $ readTVarIO envDbConstraints
--   unless (dbConstraintEpochStake mdbc) $ do
--     addEpochStakeTableConstraint logger
--     liftIO . atomically $
--       writeTVar envDbConstraints (mdbc {dbConstraintEpochStake = True})

-- -- | Add Reward constraints if not exist
-- addRewardConstraintsIfNotExist ::
--   MonadIO m =>
--   TVar ManualDbConstraints ->
--   Text.Text ->
--   DbEvent m ()
-- addRewardConstraintsIfNotExist envDbConstraints logger = do
--   mdbc <- liftIO $ readTVarIO envDbConstraints
--   unless (dbConstraintRewards mdbc) $ do
--     addRewardTableConstraint logger
--     liftIO . atomically $
--       writeTVar envDbConstraints (mdbc {dbConstraintRewards = True})
