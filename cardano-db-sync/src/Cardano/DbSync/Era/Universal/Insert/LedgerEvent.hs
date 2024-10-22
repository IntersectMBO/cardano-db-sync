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
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache.Types (textShowStats)
import Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Adjust (adjustEpochRewards)
import Cardano.DbSync.Era.Universal.Epoch (insertPoolDepositRefunds, insertProposalRefunds, insertRewardRests, insertRewards)
import Cardano.DbSync.Era.Universal.Insert.GovAction
import Cardano.DbSync.Era.Universal.Validate (validateEpochRewards)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Database.Persist.SqlBackend.Internal
import Database.Persist.SqlBackend.Internal.StatementCache

--------------------------------------------------------------------------------------------
-- Insert LedgerEvents
--------------------------------------------------------------------------------------------
insertNewEpochLedgerEvents ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  EpochNo ->
  [LedgerEvent] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertNewEpochLedgerEvents syncEnv currentEpochNo@(EpochNo curEpoch) =
  mapM_ handler
  where
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
      (MonadBaseControl IO m, MonadIO m) =>
      LedgerEvent ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $
            insertEpochSyncTime en (toSyncState ss) (envEpochSyncTime syncEnv)
          sqlBackend <- lift ask
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
          insertRewards syncEnv ntw (subFromCurrentEpoch 2) currentEpochNo cache (Map.toList $ Generic.unRewards rwd)
          -- This event is only created when it's not empty, so we don't need to check for null here.
          liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Delta rewards"
        LedgerIncrementalRewards _ rwd -> do
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards syncEnv ntw (subFromCurrentEpoch 1) (EpochNo $ curEpoch + 1) cache rewards
        LedgerRestrainedRewards e rwd creds ->
          lift $ adjustEpochRewards tracer ntw cache e rwd creds
        LedgerTotalRewards _e rwd ->
          lift $ validateEpochRewards tracer ntw (subFromCurrentEpoch 2) currentEpochNo rwd
        LedgerAdaPots _ ->
          pure () -- These are handled separately by insertBlock
        LedgerGovInfo enacted dropped expired uncl -> do
          unless (Set.null uncl) $
            liftIO $
              logInfo tracer $
                "Found " <> textShow (Set.size uncl) <> " unclaimed proposal refunds"
          updateDropped cache (EpochNo curEpoch) (garGovActionId <$> (dropped <> expired))
          let refunded = filter (\e -> Set.notMember (garGovActionId e) uncl) (enacted <> dropped <> expired)
          insertProposalRefunds tracer ntw (subFromCurrentEpoch 1) currentEpochNo cache refunded -- TODO: check if they are disjoint to avoid double entries.
          forM_ enacted $ \gar -> do
            gaId <- resolveGovActionProposal cache (garGovActionId gar)
            lift $ void $ DB.updateGovActionEnacted gaId (unEpochNo currentEpochNo)
            whenJust (garMTreasury gar) $ \treasuryMap -> do
              let rewards = Map.mapKeys Ledger.raCredential $ Map.map (Set.singleton . mkTreasuryReward) treasuryMap
              insertRewardRests tracer ntw (subFromCurrentEpoch 1) currentEpochNo cache (Map.toList rewards)
        LedgerMirDist rwd -> do
          unless (Map.null rwd) $ do
            let rewards = Map.toList rwd
            insertRewardRests tracer ntw (subFromCurrentEpoch 1) currentEpochNo cache rewards
            liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Mir rewards"
        LedgerPoolReap en drs ->
          unless (Map.null $ Generic.unRewards drs) $ do
            insertPoolDepositRefunds syncEnv en drs
        LedgerDeposits {} -> pure ()
