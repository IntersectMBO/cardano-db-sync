module Cardano.DbSync.Util.Constraint where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Db (ManualDbConstraints (..))
import qualified Cardano.Db as DB
import Cardano.Prelude (MonadIO (..), atomically)
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Control.Monad (unless)
import Data.Text (Text)

import Cardano.DbSync.Api.Types (SyncEnv (..))

-- | Add all constraints if needed
addConstraintsIfNotExist ::
  MonadIO m =>
  -- | TVar for tracking constraint state
  SyncEnv ->
  Trace IO Text ->
  DB.DbAction m ()
addConstraintsIfNotExist syncEnv trce = do
  addStakeConstraintsIfNotExist syncEnv trce
  addRewardConstraintsIfNotExist syncEnv trce

-- | Add EpochStake constraints if not exist
addStakeConstraintsIfNotExist ::
  MonadIO m =>
  SyncEnv ->
  Trace IO Text ->
  DB.DbAction m ()
addStakeConstraintsIfNotExist syncEnv trce = do
  let eDbConstraints = envDbConstraints syncEnv
  mdbc <- liftIO $ readTVarIO eDbConstraints
  unless (dbConstraintEpochStake mdbc) $ do
    DB.addEpochStakeTableConstraint trce
    liftIO . atomically $
      writeTVar eDbConstraints (mdbc {dbConstraintEpochStake = True})

-- | Add Reward constraints if not exist
addRewardConstraintsIfNotExist ::
  MonadIO m =>
  SyncEnv ->
  Trace IO Text ->
  DB.DbAction m ()
addRewardConstraintsIfNotExist syncEnv trce = do
  let eDbConstraints = envDbConstraints syncEnv
  mdbc <- liftIO $ readTVarIO eDbConstraints
  unless (dbConstraintRewards mdbc) $ do
    DB.addRewardTableConstraint trce
    liftIO . atomically $
      writeTVar eDbConstraints (mdbc {dbConstraintRewards = True})
