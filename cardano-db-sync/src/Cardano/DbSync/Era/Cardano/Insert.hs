{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.DbSync.Era.Cardano.Insert
  ( insertEpochSyncTime
  ) where

import qualified Cardano.Db as Db

import           Cardano.Prelude hiding (STM, atomically)

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Db (SyncState)

import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, STM, StrictTVar, atomically,
                   readTVar, writeTVar)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import           Database.Persist.Sql (SqlBackend)


-- If `db-sync` is started in epoch `N`, the number of seconds to sync that epoch will be recorded
-- as `Nothing`.

insertEpochSyncTime
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> SyncState -> StrictTVar IO (Maybe UTCTime) -> ReaderT SqlBackend m ()
insertEpochSyncTime epochNo syncState estvar = do
  now <- liftIO Time.getCurrentTime
  mlast <- liftIO . atomically $ swapTVar estvar (Just now)
  void . Db.insertEpochSyncTime $
          Db.EpochSyncTime
            { Db.epochSyncTimeNo = unEpochNo epochNo - 1
            , Db.epochSyncTimeSeconds = realToFrac . Time.diffUTCTime now <$> mlast
            , Db.epochSyncTimeState = syncState
            }


swapTVar :: MonadSTM m => StrictTVar m a -> a -> STM m a
swapTVar tvar !new = do
  old <- readTVar tvar
  writeTVar tvar new
  pure old
