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
import Cardano.DbSync.AppT (App, SyncEnv (..), askTrace, dbQueryToApp)
import Cardano.DbSync.Cache.Types (textShowStats)
import Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Adjust (adjustEpochRewards)
import Cardano.DbSync.Era.Universal.Epoch (insertPoolDepositRefunds, insertProposalRefunds, insertRewardRests, insertRewards)
import Cardano.DbSync.Era.Universal.Insert.GovAction
import Cardano.DbSync.Era.Universal.Validate (validateEpochRewards)
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Database.Persist.SqlBackend.Internal
import Database.Persist.SqlBackend.Internal.StatementCache

--------------------------------------------------------------------------------------------
-- Insert LedgerEvents
--------------------------------------------------------------------------------------------
insertNewEpochLedgerEvents ::
  EpochNo ->
  [LedgerEvent] ->
  App ()
insertNewEpochLedgerEvents currentEpochNo@(EpochNo curEpoch) =
  mapM_ handler
  where
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
      App ()
    handler ev = do
      syncEnv <- ask
      cache <- asks envCache
      tracer <- askTrace
      case ev of
        LedgerNewEpoch en ss -> do
          insertEpochSyncTime en (toSyncState ss) (envEpochSyncTime syncEnv)
          sqlBackend <- asks envBackend
          persistantCacheSize <- liftIO $ statementCacheSize $ connStmtMap sqlBackend
          liftIO . logInfo tracer $ "Persistant SQL Statement Cache size is " <> textShow persistantCacheSize
          stats <- liftIO $ textShowStats cache
          liftIO . logInfo tracer $ stats
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerStartAtEpoch en ->
          -- This is different from the previous case in that the db-sync started
          -- in this epoch, for example after a restart, instead of after an epoch boundary.
          liftIO . logInfo tracer $ "Starting at epoch " <> textShow (unEpochNo en)
        LedgerDeltaRewards _e rwd -> do
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards (subFromCurrentEpoch 2) currentEpochNo (Map.toList $ Generic.unRewards rwd)
          -- This event is only created when it's not empty, so we don't need to check for null here.
          liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Delta rewards"
        LedgerIncrementalRewards _ rwd -> do
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards (subFromCurrentEpoch 1) (EpochNo $ curEpoch + 1) rewards
        LedgerRestrainedRewards e rwd creds ->
          adjustEpochRewards e rwd creds
        LedgerTotalRewards _e rwd ->
          validateEpochRewards (subFromCurrentEpoch 2) currentEpochNo rwd
        LedgerAdaPots _ ->
          pure () -- These are handled separately by insertBlock
        LedgerGovInfo en ex uncl -> do
          unless (Set.null uncl) $
            liftIO $
              logInfo tracer $
                "Found " <> textShow (Set.size uncl) <> " unclaimed proposal refunds"
          updateDropped (EpochNo curEpoch) (garGovActionId <$> (en <> ex))
          let en' = filter (\e -> Set.notMember (garGovActionId e) uncl) en
              ex' = filter (\e -> Set.notMember (garGovActionId e) uncl) ex
          insertProposalRefunds (subFromCurrentEpoch 1) currentEpochNo (en' <> ex') -- TODO: check if they are disjoint to avoid double entries.
          forM_ en $ \gar -> whenJust (garMTreasury gar) $ \treasuryMap -> do
            gaId <- resolveGovActionProposal (garGovActionId gar)
            void $ dbQueryToApp $ DB.updateGovActionEnacted gaId (unEpochNo currentEpochNo)
            let rewards = Map.mapKeys Ledger.raCredential $ Map.map (Set.singleton . mkTreasuryReward) treasuryMap
            insertRewardRests (subFromCurrentEpoch 1) currentEpochNo (Map.toList rewards)
        LedgerMirDist rwd -> do
          unless (Map.null rwd) $ do
            let rewards = Map.toList rwd
            insertRewardRests (subFromCurrentEpoch 1) currentEpochNo rewards
            liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Mir rewards"
        LedgerPoolReap en drs ->
          unless (Map.null $ Generic.unRewards drs) $ do
            insertPoolDepositRefunds en drs
        LedgerDeposits {} -> pure ()
