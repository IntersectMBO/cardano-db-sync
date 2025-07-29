{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Cardano.Util (
  insertEpochSyncTime,
  initEpochStatistics,
  resetEpochStatistics,
  unChainHash,
) where

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar, newTVarIO, writeTVar)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Network.Block (ChainHash (..))

import Cardano.Db (DbAction, SyncState)
import qualified Cardano.Db as Db
import Cardano.DbSync.Api.Types (EpochStatistics (..), SyncEnv (..))
import Cardano.DbSync.Cache.Types (initCacheStatistics)

-- If `db-sync` is started in epoch `N`, the number of seconds to sync that epoch will be recorded
-- as `Nothing`.
insertEpochSyncTime ::
  MonadIO m =>
  EpochNo ->
  SyncState ->
  EpochStatistics ->
  UTCTime ->
  DbAction m ()
insertEpochSyncTime epochNo syncState epochStats endTime = do
  void . Db.insertEpochSyncTime $
    Db.EpochSyncTime
      { Db.epochSyncTimeNo = unEpochNo epochNo - 1
      , Db.epochSyncTimeSeconds = ceiling (realToFrac (Time.diffUTCTime endTime (elsStartTime epochStats)) :: Double)
      , Db.epochSyncTimeState = syncState
      }

initEpochStatistics :: MonadIO m => m (StrictTVar IO EpochStatistics)
initEpochStatistics = do
  curTime <- liftIO Time.getCurrentTime
  liftIO $
    newTVarIO $
      EpochStatistics
        { elsStartTime = curTime
        , elsCaches = initCacheStatistics
        , elsUnicodeNull = Map.empty
        }

resetEpochStatistics :: MonadIO m => SyncEnv -> m ()
resetEpochStatistics syncEnv = liftIO $ do
  curTime <- getCurrentTime
  let newEpochStatsValue =
        EpochStatistics
          { elsStartTime = curTime
          , elsCaches = initCacheStatistics
          , elsUnicodeNull = Map.empty
          }
  atomically $ writeTVar (envEpochStatistics syncEnv) newEpochStatsValue

unChainHash :: ChainHash (CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    GenesisHash -> "genesis"
    BlockHash bh -> SBS.fromShort (Consensus.getOneEraHash bh)
