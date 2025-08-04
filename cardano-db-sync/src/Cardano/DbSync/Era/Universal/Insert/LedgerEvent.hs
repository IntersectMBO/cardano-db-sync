{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cardano.DbSync.Era.Universal.Insert.LedgerEvent (
  insertNewEpochLedgerEvents,
) where

import Cardano.BM.Trace (logInfo)

import qualified Cardano.Db as DB
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))

import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (EpochStatistics (..), SyncEnv (..), UnicodeNullSource, formatUnicodeNullSource)
import Cardano.DbSync.Cache.Types (textShowCacheStats)
import Cardano.DbSync.Era.Cardano.Util (insertEpochSyncTime, resetEpochStatistics)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Adjust (adjustEpochRewards)
import Cardano.DbSync.Era.Universal.Epoch (insertPoolDepositRefunds, insertProposalRefunds, insertRewardRests, insertRewards)
import Cardano.DbSync.Era.Universal.Insert.GovAction
import Cardano.DbSync.Era.Universal.Validate (validateEpochRewards)
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types

import Cardano.DbSync.Error (SyncNodeError)
import Cardano.DbSync.Metrics (setDbEpochSyncDuration, setDbEpochSyncNumber)
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO)
import Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Text.Printf (printf)

--------------------------------------------------------------------------------------------
-- Insert LedgerEvents
--------------------------------------------------------------------------------------------
insertNewEpochLedgerEvents ::
  SyncEnv ->
  EpochNo ->
  [LedgerEvent] ->
  ExceptT SyncNodeError DB.DbM ()
insertNewEpochLedgerEvents syncEnv currentEpochNo@(EpochNo curEpoch) =
  mapM_ handler
  where
    metricSetters = envMetricSetters syncEnv
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    ntw = getNetwork syncEnv

    subFromCurrentEpoch :: Word64 -> EpochNo
    subFromCurrentEpoch m =
      if unEpochNo currentEpochNo >= m
        then EpochNo $ unEpochNo currentEpochNo - m
        else EpochNo 0

    toSyncState :: SyncState -> DB.SyncState
    toSyncState SyncLagging = DB.SyncLagging
    toSyncState SyncFollowing = DB.SyncFollowing

    handler ::
      LedgerEvent ->
      ExceptT SyncNodeError DB.DbM ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          databaseCacheSize <- lift DB.queryStatementCacheSize
          liftIO . logInfo tracer $ "Database Statement Cache size is " <> textShow databaseCacheSize
          currentTime <- liftIO getCurrentTime
          -- Get current epoch statistics
          epochStats <- liftIO $ readTVarIO (envEpochStatistics syncEnv)
          -- Insert the epoch sync time into the database
          insertEpochSyncTime en (toSyncState ss) epochStats currentTime
          -- Text of the epoch sync time
          let epochDurationText = formatEpochDuration (elsStartTime epochStats) currentTime

          -- Format statistics
          cacheStatsText <- liftIO $ textShowCacheStats (elsCaches epochStats) cache
          let unicodeStats = formatUnicodeNullStats (elsUnicodeNull epochStats)

          -- add epoch metricI's to prometheus
          liftIO $ setDbEpochSyncDuration metricSetters (epochDurationSeconds (elsStartTime epochStats) currentTime)
          liftIO $ setDbEpochSyncNumber metricSetters (fromIntegral $ unEpochNo en - 1)

          -- Log comprehensive epoch statistics
          liftIO . logInfo tracer $
            mconcat
              [ "\n----------------------- Statistics for Epoch " <> textShow (unEpochNo en - 1) <> " -----------------------"
              , "\nThis epoch took: " <> epochDurationText <> " to process."
              , "\n\nNull Unicodes:"
              , "\n  " <> unicodeStats
              , cacheStatsText
              , "\n-----------------------------------------------------------------"
              ]

          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
          -- Reset epoch statistics for new epoch
          resetEpochStatistics syncEnv
        LedgerStartAtEpoch en -> do
          -- This is different from the previous case in that the db-sync started
          -- in this epoch, for example after a restart, instead of after an epoch boundary.
          liftIO . logInfo tracer $ "Starting at epoch " <> textShow (unEpochNo en)
          -- Reset epoch statistics for new epoch
          resetEpochStatistics syncEnv
        LedgerDeltaRewards _e rwd -> do
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards syncEnv ntw (subFromCurrentEpoch 2) currentEpochNo (Map.toList $ Generic.unRewards rwd)
          -- This event is only created when it's not empty, so we don't need to check for null here.
          liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Delta rewards"
        LedgerIncrementalRewards _ rwd -> do
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards syncEnv ntw (subFromCurrentEpoch 1) (EpochNo $ curEpoch + 1) rewards
        LedgerRestrainedRewards e rwd creds ->
          adjustEpochRewards syncEnv ntw e rwd creds
        LedgerTotalRewards _e rwd ->
          validateEpochRewards tracer ntw (subFromCurrentEpoch 2) currentEpochNo rwd
        LedgerAdaPots _ ->
          pure () -- These are handled separately by insertBlock
        LedgerGovInfo enacted dropped expired uncl -> do
          unless (Set.null uncl) $
            liftIO $
              logInfo tracer $
                "Found " <> textShow (Set.size uncl) <> " unclaimed proposal refunds"
          updateDropped syncEnv (EpochNo curEpoch) (garGovActionId <$> (dropped <> expired))
          let refunded = filter (\e -> Set.notMember (garGovActionId e) uncl) (enacted <> dropped <> expired)
          insertProposalRefunds syncEnv ntw (subFromCurrentEpoch 1) currentEpochNo refunded -- TODO: check if they are disjoint to avoid double entries.
          forM_ enacted $ \gar -> do
            gaId <- resolveGovActionProposal syncEnv (garGovActionId gar)
            void $ lift $ DB.updateGovActionEnacted gaId (unEpochNo currentEpochNo)
            whenJust (garMTreasury gar) $ \treasuryMap -> do
              let rewards = Map.mapKeys Ledger.raCredential $ Map.map (Set.singleton . mkTreasuryReward) treasuryMap
              insertRewardRests syncEnv ntw (subFromCurrentEpoch 1) currentEpochNo (Map.toList rewards)
        LedgerMirDist rwd -> do
          unless (Map.null rwd) $ do
            let rewards = Map.toList rwd
            insertRewardRests syncEnv ntw (subFromCurrentEpoch 1) currentEpochNo rewards
            liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Mir rewards"
        LedgerPoolReap en drs ->
          unless (Map.null $ Generic.unRewards drs) $ do
            insertPoolDepositRefunds syncEnv en drs
        LedgerDeposits {} -> pure ()

epochDurationSeconds :: UTCTime -> UTCTime -> Double
epochDurationSeconds startTime endTime =
  realToFrac (diffUTCTime endTime startTime)

formatEpochDuration :: UTCTime -> UTCTime -> Text
formatEpochDuration startTime endTime =
  let duration = diffUTCTime endTime startTime
      totalSeconds = floor duration :: Integer
      hours = totalSeconds `div` 3600
      minutes = (totalSeconds `mod` 3600) `div` 60
      seconds = totalSeconds `mod` 60
      milliseconds = floor ((duration - fromIntegral totalSeconds) * 100) :: Integer
   in Text.pack $ printf "%02d:%02d:%02d.%02d" hours minutes seconds milliseconds

formatUnicodeNullStats :: Map.Map UnicodeNullSource [DB.TxId] -> Text
formatUnicodeNullStats unicodeMap =
  if Map.null unicodeMap
    then "No Unicode NUL characters found in JSON parsing."
    else
      let header = "The following were recorded as null, due to a Unicode NUL character found when trying to parse the json:"
          formatEntry (source, txIds) = do
            let unwrappedTxIds = map DB.getTxId txIds
            "  " <> formatUnicodeNullSource source <> " - " <> textShow (length txIds) <> " - for txIds: " <> textShow unwrappedTxIds
          entries = Map.toList unicodeMap
       in header <> "\n" <> Text.intercalate "\n" (map formatEntry entries)
