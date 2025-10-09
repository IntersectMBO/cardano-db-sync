{-# LANGUAGE FlexibleContexts #-}

module Cardano.DbSync.Util.Constraint where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Prelude (ExceptT, MonadIO (..), atomically, lift)
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Control.Monad (unless)
import Data.Text (Text)

import Cardano.Db (ManualDbConstraints (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Error (SyncNodeError)

-- | Add all constraints if needed
addConstraintsIfNotExist ::
  -- | TVar for tracking constraint state
  SyncEnv ->
  Trace IO Text ->
  ExceptT SyncNodeError DB.DbM ()
addConstraintsIfNotExist syncEnv trce = do
  addStakeConstraintsIfNotExist syncEnv trce
  addRewardConstraintsIfNotExist syncEnv trce

-- | Add EpochStake constraints if not exist
addStakeConstraintsIfNotExist ::
  SyncEnv ->
  Trace IO Text ->
  ExceptT SyncNodeError DB.DbM ()
addStakeConstraintsIfNotExist syncEnv trce = do
  let eDbConstraints = envDbConstraints syncEnv
  mdbc <- liftIO $ readTVarIO eDbConstraints
  unless (dbConstraintEpochStake mdbc) $ do
    lift $ DB.addEpochStakeTableConstraint trce
    liftIO . atomically $
      writeTVar eDbConstraints (mdbc {dbConstraintEpochStake = True})

-- | Add Reward constraints if not exist
addRewardConstraintsIfNotExist ::
  SyncEnv ->
  Trace IO Text ->
  ExceptT SyncNodeError DB.DbM ()
addRewardConstraintsIfNotExist syncEnv trce = do
  let eDbConstraints = envDbConstraints syncEnv
  mdbc <- liftIO $ readTVarIO eDbConstraints
  unless (dbConstraintRewards mdbc) $ do
    lift $ DB.addRewardTableConstraint trce
    liftIO . atomically $
      writeTVar eDbConstraints (mdbc {dbConstraintRewards = True})
