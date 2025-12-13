{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Validate (
  validateEpochStake,
  validateEpochRewards,
) where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import Cardano.Ledger.Shelley.API (Network)
import qualified Cardano.Ledger.Shelley.Rewards as Ledger
import Cardano.Prelude hiding (from, on)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Err (error)

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Error (SyncNodeError)
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Constraint
import qualified Data.Strict.Maybe as Strict

validateEpochStake ::
  SyncEnv ->
  ApplyResult ->
  Bool ->
  ExceptT SyncNodeError DB.DbM ()
validateEpochStake syncEnv applyRes firstCall = case apOldLedger applyRes of
  Strict.Just lstate | Just (expectedCount, epoch) <- Generic.countEpochStake (clsState lstate) -> do
    actualCount <- lift $ DB.queryNormalEpochStakeCount (unEpochNo epoch)
    if actualCount /= expectedCount
      then do
        liftIO
          . logWarning tracer
          $ mconcat
            [ "validateEpochStake: epoch stake in epoch "
            , textShow (unEpochNo epoch)
            , " expected total of "
            , textShow expectedCount
            , " but got "
            , textShow actualCount
            ]
        let slice = Generic.fullEpochStake (clsState lstate)
        addStakeConstraintsIfNotExist syncEnv tracer
        insertStakeSlice syncEnv slice
        when firstCall $ validateEpochStake syncEnv applyRes False
      else
        liftIO $
          logInfo tracer $
            mconcat
              [ "Validate Epoch Stake: total entries in epoch "
              , textShow (unEpochNo epoch)
              , " are "
              , textShow actualCount
              ]
  _ -> pure ()
  where
    tracer = getTrace syncEnv

validateEpochRewards ::
  Trace IO Text ->
  Network ->
  EpochNo ->
  EpochNo ->
  Map StakeCred (Set Ledger.Reward) ->
  ExceptT SyncNodeError DB.DbM ()
validateEpochRewards tracer network _earnedEpochNo spendableEpochNo rmap = do
  actualCount <- lift $ DB.queryNormalEpochRewardCount (unEpochNo spendableEpochNo)
  if actualCount /= expectedCount
    then do
      liftIO
        . logWarning tracer
        $ mconcat
          [ "validateEpochRewards: rewards spendable in epoch "
          , textShow (unEpochNo spendableEpochNo)
          , " expected total of "
          , textShow expectedCount
          , " but got "
          , textShow actualCount
          ]
      logFullRewardMap tracer spendableEpochNo network (convertPoolRewards rmap)
    else do
      liftIO
        . logInfo tracer
        $ mconcat
          [ "Validate Epoch Rewards: total rewards that become spendable in epoch "
          , textShow (unEpochNo spendableEpochNo)
          , " are "
          , textShow actualCount
          ]
  where
    expectedCount :: Word64
    expectedCount = fromIntegral . sum $ map Set.size (Map.elems rmap)

logFullRewardMap ::
  Trace IO Text ->
  EpochNo ->
  Network ->
  Generic.Rewards ->
  ExceptT SyncNodeError DB.DbM ()
logFullRewardMap tracer epochNo network ledgerMap = do
  dbMap <- queryRewardMap epochNo
  when (Map.size dbMap > 0 && Map.size (Generic.unRewards ledgerMap) > 0) $
    liftIO $
      diffRewardMap tracer network dbMap (Map.mapKeys (Generic.stakingCredHash network) $ Map.map convert $ Generic.unRewards ledgerMap)
  where
    convert :: Set Generic.Reward -> [(DB.RewardSource, Word64)]
    convert = map (\rwd -> (Generic.rewardSource rwd, Generic.rewardAmount rwd)) . Set.toList

queryRewardMap :: EpochNo -> ExceptT SyncNodeError DB.DbM (Map ByteString [(DB.RewardSource, DB.DbLovelace)])
queryRewardMap (EpochNo epochNo) = do
  results <- lift $ DB.queryRewardMapData epochNo
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
  Map ByteString [(DB.RewardSource, Word64)] ->
  IO ()
diffRewardMap tracer _nw dbMap ledgerMap = do
  when (Map.size diffMap > 0) $ do
    logError tracer "diffRewardMap:"
    mapM_ (logError tracer . render) $ Map.toList diffMap
  where
    keys :: [ByteString]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map ByteString ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Word64)])
    diffMap = List.foldl' mkDiff mempty keys

    mkDiff ::
      Map ByteString ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Word64)]) ->
      ByteString ->
      Map ByteString ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Word64)])
    mkDiff !acc addr =
      case (Map.lookup addr dbMap, Map.lookup addr ledgerMap) of
        (Just xs, Just ys) ->
          if fromIntegral (sum $ map (DB.unDbLovelace . snd) xs) == sum (map snd ys)
            then acc
            else Map.insert addr (xs, ys) acc
        (Nothing, Just ys) -> Map.insert addr ([], ys) acc
        (Just xs, Nothing) -> Map.insert addr (xs, []) acc
        (Nothing, Nothing) -> acc

    render :: (ByteString, ([(DB.RewardSource, DB.DbLovelace)], [(DB.RewardSource, Word64)])) -> Text
    render (cred, (xs, ys)) = mconcat ["  ", show cred, ": ", show xs, " /= ", show ys]
