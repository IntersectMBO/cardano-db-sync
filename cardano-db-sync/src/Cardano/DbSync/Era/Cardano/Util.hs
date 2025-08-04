{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Cardano.Util (
  insertEpochSyncTime,
  initEpochStatistics,
  resetEpochStatistics,
  unChainHash,
) where

import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar, newTVarIO, writeTVar)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Network.Block (ChainHash (..))

import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types (EpochStatistics (..), SyncEnv (..))
import Cardano.DbSync.Cache.Types (initCacheStatistics)
import Cardano.DbSync.Error (SyncNodeError)

-- If `db-sync` is started in epoch `N`, the number of seconds to sync that epoch will be recorded
-- as `Nothing`.
insertEpochSyncTime ::
  EpochNo ->
  DB.SyncState ->
  EpochStatistics ->
  UTCTime ->
  ExceptT SyncNodeError DB.DbM ()
insertEpochSyncTime epochNo syncState epochStats endTime = do
  void
    . lift
    $ DB.insertEpochSyncTime
    $ DB.EpochSyncTime
      { DB.epochSyncTimeNo = unEpochNo epochNo - 1
      , DB.epochSyncTimeSeconds = ceiling (realToFrac (Time.diffUTCTime endTime (elsStartTime epochStats)) :: Double)
      , DB.epochSyncTimeState = syncState
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
