{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Validate
  ( validateEpochRewardsBefore
  ) where

import           Cardano.Prelude hiding (from, on)

import           Cardano.BM.Trace (Trace, logError)

import qualified Cardano.Db as Db

import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import           Database.Esqueleto.Legacy (Value (..), from, select, sum_, val, where_, (==.),
                   (^.))

import           Database.Persist.Sql (SqlBackend)


validateEpochRewardsBefore
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo
    -> ReaderT SqlBackend m ()
validateEpochRewardsBefore tracer epochNo = do
  actual <- queryEpochRewardTotal epochNo
  unless (actual == 0) $ do
    mExpected <- queryEpochRewardTotalReceived epochNo
    case mExpected of
      Nothing ->
        liftIO . logError tracer $ mconcat
                    [ "validateEpochRewardsBefore: no expected total for rewards earned in epoch "
                    , textShow (unEpochNo epochNo)
                    ]
      Just expected ->
        when (actual /= expected) .
          liftIO .
            logError tracer $ mconcat
                [ "validateEpochRewardsBefore: rewards earned in epoch "
                , textShow (unEpochNo epochNo), " expected total of ", textShow expected
                , " ADA but got " , textShow actual, " ADA"
                ]

-- -------------------------------------------------------------------------------------------------

queryEpochRewardTotal
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m Db.Ada
queryEpochRewardTotal (EpochNo epochNo) = do
  res <- select . from $ \ rwd -> do
            where_ (rwd ^. Db.RewardEarnedEpoch ==. val epochNo)
            pure (sum_ $ rwd ^. Db.RewardAmount)
  pure $ Db.unValueSumAda (listToMaybe res)

queryEpochRewardTotalReceived
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m (Maybe Db.Ada)
queryEpochRewardTotalReceived (EpochNo epochNo) = do
  res <- select . from $ \ ertr -> do
            where_ (ertr ^. Db.EpochRewardTotalReceivedEarnedEpoch==. val epochNo)
            pure (ertr ^. Db.EpochRewardTotalReceivedAmount)
  pure $ Db.word64ToAda . Db.unDbLovelace . unValue <$> listToMaybe res
