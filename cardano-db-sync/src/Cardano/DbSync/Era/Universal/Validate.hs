{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Validate (
  validateEpochRewards,
) where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Shelley.API (Network)
import qualified Cardano.Ledger.Shelley.Rewards as Ledger
import Cardano.Prelude hiding (from, on)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Err (error)

validateEpochRewards ::
  MonadIO m =>
  Trace IO Text ->
  Network ->
  EpochNo ->
  EpochNo ->
  Map StakeCred (Set Ledger.Reward) ->
  DB.DbAction m ()
validateEpochRewards tracer network _earnedEpochNo spendableEpochNo rmap = do
  actualCount <- DB.queryNormalEpochRewardCount (unEpochNo spendableEpochNo)
  if actualCount /= expectedCount
    then do
      liftIO . logWarning tracer $
        mconcat
          [ "validateEpochRewards: rewards spendable in epoch "
          , textShow (unEpochNo spendableEpochNo)
          , " expected total of "
          , textShow expectedCount
          , " but got "
          , textShow actualCount
          ]
      logFullRewardMap tracer spendableEpochNo network (convertPoolRewards rmap)
    else do
      liftIO . logInfo tracer $
        mconcat
          [ "Validate Epoch Rewards: total rewards that become spendable in epoch "
          , textShow (unEpochNo spendableEpochNo)
          , " are "
          , textShow actualCount
          ]
  where
    expectedCount :: Word64
    expectedCount = fromIntegral . sum $ map Set.size (Map.elems rmap)

logFullRewardMap ::
  MonadIO m =>
  Trace IO Text ->
  EpochNo ->
  Network ->
  Generic.Rewards ->
  DB.DbAction m ()
logFullRewardMap tracer epochNo network ledgerMap = do
  dbMap <- queryRewardMap epochNo
  when (Map.size dbMap > 0 && Map.size (Generic.unRewards ledgerMap) > 0) $
    liftIO $
      diffRewardMap tracer network dbMap (Map.mapKeys (Generic.stakingCredHash network) $ Map.map convert $ Generic.unRewards ledgerMap)
  where
    convert :: Set Generic.Reward -> [(DB.RewardSource, Coin)]
    convert = map (\rwd -> (Generic.rewardSource rwd, Generic.rewardAmount rwd)) . Set.toList

queryRewardMap :: MonadIO m => EpochNo -> DB.DbAction m (Map ByteString [(DB.RewardSource, DB.DbLovelace)])
queryRewardMap (EpochNo epochNo) = do
  results <- DB.queryRewardMapData epochNo
  pure $ processRewardMapData results

processRewardMapData :: [(ByteString, DB.RewardSource, DB.DbLovelace)] -> Map ByteString [(DB.RewardSource, DB.DbLovelace)]
processRewardMapData results =
  Map.fromList . map collapse $ List.groupOn fst (map convert results)
  where
    convert :: (ByteString, DB.RewardSource, DB.DbLovelace) -> (ByteString, (DB.RewardSource, DB.DbLovelace))
    convert (cred, source, amount) = (cred, (source, amount))

    collapse :: [(ByteString, (DB.RewardSource, DB.DbLovelace))] -> (ByteString, [(DB.RewardSource, DB.DbLovelace)])
    collapse xs =
      case xs of
        [] -> error "processRewardMapData.collapse: Unexpected empty list"
        x : _ -> (fst x, List.sort $ map snd xs)

diffRewardMap ::
  Trace IO Text ->
  Network ->
  Map ByteString [(DB.RewardSource, DB.DbLovelace)] ->
  Map ByteString [(DB.RewardSource, Coin)] ->
  IO ()
diffRewardMap tracer _nw dbMap ledgerMap = do
  when (Map.size diffMap > 0) $ do
    logError tracer "diffRewardMap:"
    mapM_ (logError tracer . render) $ Map.toList diffMap
  where
    keys :: [ByteString]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map ByteString ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Coin)])
    diffMap = List.foldl' mkDiff mempty keys

    mkDiff ::
      Map ByteString ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Coin)]) ->
      ByteString ->
      Map ByteString ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Coin)])
    mkDiff !acc addr =
      case (Map.lookup addr dbMap, Map.lookup addr ledgerMap) of
        (Just xs, Just ys) ->
          if fromIntegral (sum $ map (DB.unDbLovelace . snd) xs) == sum (map (unCoin . snd) ys)
            then acc
            else Map.insert addr (xs, ys) acc
        (Nothing, Just ys) -> Map.insert addr ([], ys) acc
        (Just xs, Nothing) -> Map.insert addr (xs, []) acc
        (Nothing, Nothing) -> acc

    render :: (ByteString, ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Coin)])) -> Text
    render (cred, (xs, ys)) = mconcat ["  ", show cred, ": ", show xs, " /= ", show ys]
