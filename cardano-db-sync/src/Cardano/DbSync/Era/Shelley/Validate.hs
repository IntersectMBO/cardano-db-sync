{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Validate
  ( validateEpochRewardsAfter
  , validateEpochRewardsBefore
  ) where

import           Cardano.Prelude hiding (from, on)

import           Cardano.BM.Trace (Trace, logError, logInfo)

import           Cardano.Db (DbLovelace, RewardSource)
import qualified Cardano.Db as Db

import           Cardano.Ledger.BaseTypes (Network)
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), from, on, select, sum_, val,
                   where_, (==.), (^.))

import           Database.Persist.Sql (SqlBackend)


validateEpochRewardsAfter
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Network -> EpochNo -> Map (Ledger.StakeCredential c) Coin
    -> ReaderT SqlBackend m ()
validateEpochRewardsAfter tracer nw epochNo rmap = do
    actual <- queryEpochRewardTotal epochNo
    -- unless (actual == 0) $
    if actual /= expected
        then do
          liftIO . logError tracer $ mconcat
                      [ "validateEpochRewardsAfter: rewards earned in epoch "
                      , textShow (unEpochNo epochNo), " expected total of ", textShow expected
                      , " ADA but got " , textShow actual, " ADA"
                      ]
          logFullRewardMap epochNo (convertRewardMap nw rmap)
        else
          liftIO . logInfo tracer $ mconcat
                      [ "validateEpochRewardsAfter: total rewards that become spendable in epoch "
                      , textShow (2 + unEpochNo epochNo), " is ", textShow actual, " ADA"
                      ]
  where
    expected :: Db.Ada
    expected = Db.word64ToAda . fromIntegral . sum $ map unCoin (Map.elems rmap)

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
        liftIO $
          if actual /= expected
            then logError tracer $ mconcat
                    [ "validateEpochRewardsBefore: rewards earned in epoch "
                    , textShow (unEpochNo epochNo), ", expected total is ", textShow expected
                    , " ADA but actual is " , textShow actual, " ADA"
                    ]
            else logInfo tracer $ mconcat
                    [ "validateEpochRewardsBefore: rewards earned in epoch "
                    , textShow (unEpochNo epochNo), " of ", textShow expected, " ADA"
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

-- -------------------------------------------------------------------------------------------------

convertRewardMap :: Network -> Map (Ledger.StakeCredential c) Coin -> Map Generic.StakeCred Coin
convertRewardMap nw = Map.mapKeys (Generic.toStakeCred nw)


logFullRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> Map Generic.StakeCred Coin -> ReaderT SqlBackend m ()
logFullRewardMap epochNo ledgerMap = do
    dbMap <- queryRewardMap epochNo
    when (Map.size dbMap > 0 && Map.size ledgerMap > 0) $
      liftIO $ diffRewardMap dbMap ledgerMap


queryRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m (Map Generic.StakeCred (Set (RewardSource, DbLovelace)))
queryRewardMap (EpochNo epochNo) = do
    res <- select . from $ \ (rwd `InnerJoin` saddr) -> do
              on (rwd ^. Db.RewardAddrId ==. saddr ^. Db.StakeAddressId)
              where_ (rwd ^. Db.RewardEarnedEpoch ==. val epochNo)
              pure (saddr ^. Db.StakeAddressHashRaw, rwd ^. Db.RewardType, rwd ^.Db.RewardAmount)
    pure . Map.fromList . map collapse $ List.groupOn fst (map convert res)
  where
    convert :: (Value ByteString, Value RewardSource, Value DbLovelace) -> (Generic.StakeCred, (RewardSource, DbLovelace))
    convert (Value cred, Value source, Value amount) = (Generic.StakeCred cred, (source, amount))

    collapse :: [(Generic.StakeCred, (RewardSource, DbLovelace))] -> (Generic.StakeCred, Set (RewardSource, DbLovelace))
    collapse xs =
        case xs of
          [] -> panic "queryRewardMap.collapse"
          x:_ -> (fst x, Set.fromList $ map snd xs)

diffRewardMap :: Map Generic.StakeCred (Set (RewardSource, DbLovelace)) -> Map Generic.StakeCred Coin -> IO ()
diffRewardMap dbMap ledgerMap = do
    putStrLn $ "dbMap length: " ++ show (length $ Map.keys dbMap)
    putStrLn $ "ledgerMap length: " ++ show (length $ Map.keys ledgerMap)
    mapM_ print $ Map.toList diffMap
    when (Map.size diffMap > 0) $
      panic "diffMap"
  where
    keys :: [Generic.StakeCred]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map Generic.StakeCred (Set (RewardSource, DbLovelace), Coin)
    diffMap = List.foldl' create mempty keys

    create
        :: Map Generic.StakeCred (Set (RewardSource, DbLovelace), Coin) -> Generic.StakeCred
        -> Map Generic.StakeCred (Set (RewardSource, DbLovelace), Coin)
    create !acc addr =
        case (Map.lookup addr dbMap, Map.lookup addr ledgerMap) of
          (Just s, Just coin) ->
                if fromIntegral (sum . map (Db.unDbLovelace . snd) $ Set.toList s) == unCoin coin
                  then acc
                  else Map.insert addr (s, coin) acc
          (Nothing, Just coin) -> Map.insert addr (mempty, coin) acc
          (Just s, Nothing) -> Map.insert addr (s, Coin (-1)) acc
          (Nothing, Nothing) -> acc
