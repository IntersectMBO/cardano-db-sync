{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Fix.EpochStake where

import Cardano.BM.Trace (logInfo, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Era.Shelley.Generic.StakeDist hiding (getStakeSlice)
import Cardano.DbSync.Era.Shelley.Insert.Epoch
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types
import Cardano.Prelude
import Control.Monad.Trans.Control
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import Database.Persist.Sql (SqlBackend)

migrateStakeDistr :: (MonadIO m, MonadBaseControl IO m) => SyncEnv -> Strict.Maybe CardanoLedgerState -> ExceptT SyncNodeError (ReaderT SqlBackend m) Bool
migrateStakeDistr env mcls =
  case (envLedgerEnv env, mcls) of
    (HasLedger lenv, Strict.Just cls) -> do
      ems <- lift DB.queryAllExtraMigrations
      runWhen (not $ DB.isStakeDistrComplete ems) $ do
        liftIO $ logInfo trce "Starting Stake Distribution migration on table epoch_stake"
        let stakeSlice = getStakeSlice lenv cls True
        case stakeSlice of
          NoSlices ->
            liftIO $ logInsert 0
          Slice (StakeSlice _epochNo distr) isFinal -> do
            liftIO $ logInsert (Map.size distr)
            insertStakeSlice env stakeSlice
            (mminEpoch, mmaxEpoch) <- lift DB.queryMinMaxEpochStake
            liftIO $ logMinMax mminEpoch mmaxEpoch
            case (mminEpoch, mmaxEpoch) of
              (Just minEpoch, Just maxEpoch) -> do
                when (maxEpoch > 0) $
                  lift $
                    DB.insertEpochStakeProgress (mkProgress True <$> [minEpoch .. (maxEpoch - 1)])
                lift $ DB.insertEpochStakeProgress [mkProgress isFinal maxEpoch]
              _ -> pure ()
        lift $ DB.insertExtraMigration DB.StakeDistrEnded
    _ -> pure False
  where
    trce = getTrace env
    mkProgress isCompleted e =
      DB.EpochStakeProgress
        { DB.epochStakeProgressEpochNo = e
        , DB.epochStakeProgressCompleted = isCompleted
        }

    logInsert :: Int -> IO ()
    logInsert n
      | n == 0 = logInfo trce "No missing epoch_stake found"
      | n > 100000 = logWarning trce $ "Found " <> DB.textShow n <> " epoch_stake. This may take a while"
      | otherwise = logInfo trce $ "Found " <> DB.textShow n <> " epoch_stake"

    logMinMax mmin mmax =
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
