{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Fix.EpochStake where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, LedgerEnv (..), MonadAppDB (..), SyncEnv (..), askTrace)
import Cardano.DbSync.Era.Shelley.Generic.StakeDist hiding (getStakeSlice)
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict

migrateStakeDistr :: Strict.Maybe CardanoLedgerState -> App Bool
migrateStakeDistr mcls = do
  ledgerEnv <- asks envLedgerEnv
  trce <- askTrace
  case (ledgerEnv, mcls) of
    (HasLedger lenv, Strict.Just cls) -> do
      ems <- dbQueryToApp DB.queryAllExtraMigrations
      runWhen (not $ DB.isStakeDistrComplete ems) $ do
        liftIO $ logInfo trce "Starting Stake Distribution migration on table epoch_stake"
        let stakeSlice = getStakeSlice lenv cls True
        case stakeSlice of
          NoSlices ->
            liftIO $ logInsert trce 0
          Slice (StakeSlice _epochNo distr) isFinal -> do
            liftIO $ logInsert trce (Map.size distr)
            insertStakeSlice stakeSlice
            (mminEpoch, mmaxEpoch) <- dbQueryToApp DB.queryMinMaxEpochStake
            liftIO $ logMinMax trce mminEpoch mmaxEpoch
            case (mminEpoch, mmaxEpoch) of
              (Just minEpoch, Just maxEpoch) -> do
                when (maxEpoch > 0) $
                  dbQueryToApp $
                    DB.insertEpochStakeProgress (mkProgress True <$> [minEpoch .. (maxEpoch - 1)])
                dbQueryToApp $ DB.insertEpochStakeProgress [mkProgress isFinal maxEpoch]
              _other -> pure ()
        dbQueryToApp $ DB.insertExtraMigration DB.StakeDistrEnded
    _other -> pure False
  where
    mkProgress isCompleted e =
      DB.EpochStakeProgress
        { DB.epochStakeProgressEpochNo = e
        , DB.epochStakeProgressCompleted = isCompleted
        }

    logInsert :: Trace IO Text -> Int -> IO ()
    logInsert trce n
      | n == 0 = logInfo trce "No missing epoch_stake found"
      | n > 100000 = logWarning trce $ "Found " <> DB.textShow n <> " epoch_stake. This may take a while"
      | otherwise = logInfo trce $ "Found " <> DB.textShow n <> " epoch_stake"

    logMinMax trce mmin mmax =
      logInfo trce $
        mconcat
          [ "Min epoch_stake at "
          , DB.textShow mmin
          , " and max at "
          , DB.textShow mmax
          ]

    runWhen :: Monad m => Bool -> m () -> m Bool
    runWhen a action = do
      if a then action >> pure True else pure False
