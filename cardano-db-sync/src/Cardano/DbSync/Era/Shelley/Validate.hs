{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Validate
  ( validateEpochRewards
  ) where

import           Cardano.Prelude hiding (from, on)

import           Cardano.BM.Trace (Trace, logError, logInfo, logWarning)

import           Cardano.Db (DbLovelace, RewardSource)
import qualified Cardano.Db as Db
import           Cardano.DbSync.Era.Shelley.ValidateWithdrawal (validateRewardWithdrawals)

import           Cardano.Ledger.Coin (Coin (..))

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), countRows, desc, from, not_,
                   on, orderBy, select, sum_, val, where_, (==.), (^.))

import           Database.Persist.Sql (SqlBackend)


validateEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Generic.Rewards
    -> ReaderT SqlBackend m ()
validateEpochRewards tracer rmap = do
    actual <- queryEpochRewardTotal (Generic.rwdEpoch rmap)
    if actual /= expected
        then do
          liftIO . logWarning tracer $ mconcat
                      [ "validateEpochRewards: rewards spendable in epoch "
                      , textShow (unEpochNo $ Generic.rwdEpoch rmap), " expected total of "
                      , textShow expected , " ADA but got " , textShow actual, " ADA"
                      ]
          logFullRewardMap tracer rmap
        else
          liftIO . logInfo tracer $ mconcat
                      [ "validateEpochRewards: total rewards that become spendable in epoch "
                      , textShow (unEpochNo $ Generic.rwdEpoch rmap), " is ", textShow actual
                      , " ADA"
                      ]
    validateRewardWithdrawals tracer (Generic.rwdEpoch rmap)
  where
    expected :: Db.Ada
    expected =
      Db.word64ToAda . fromIntegral . sum
        $ map (unCoin . Set.foldl' foldf (Coin 0)) (Map.elems $ Generic.rwdRewards rmap)

    foldf :: Coin -> Generic.Reward -> Coin
    foldf coin rwd = plusCoin coin (Generic.rewardAmount rwd)

-- -------------------------------------------------------------------------------------------------

queryEpochRewardTotal
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m Db.Ada
queryEpochRewardTotal (EpochNo epochNo) = do
  res <- select . from $ \ rwd -> do
            where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
            -- For ... reasons ... pool deposit refunds are put into the rewards account
            -- but are not considered part of the total rewards for an epoh.
            where_ (not_ $ rwd ^. Db.RewardType ==. val Db.RwdDepositRefund)
            pure (sum_ $ rwd ^. Db.RewardAmount)
  pure $ Db.unValueSumAda (listToMaybe res)

-- -------------------------------------------------------------------------------------------------

logFullRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Generic.Rewards -> ReaderT SqlBackend m ()
logFullRewardMap tracer ledgerMap = do
    dbMap <- queryRewardMap $ Generic.rwdEpoch ledgerMap
    when (Map.size dbMap > 0 && Map.size (Generic.rwdRewards ledgerMap) > 0) $
      diffRewardMap tracer (Generic.rwdEpoch ledgerMap) dbMap (Map.map convert $ Generic.rwdRewards ledgerMap)
  where
    convert :: Set Generic.Reward -> [(RewardSource, Coin)]
    convert = map (\rwd -> (Generic.rewardSource rwd, Generic.rewardAmount rwd)) . Set.toList

queryRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m (Map Generic.StakeCred [(RewardSource, DbLovelace)])
queryRewardMap (EpochNo epochNo) = do
    res <- select . from $ \ (rwd `InnerJoin` saddr) -> do
              on (rwd ^. Db.RewardAddrId ==. saddr ^. Db.StakeAddressId)
              where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
              -- Need this orderBy so that the `groupOn` below works correctly.
              orderBy [desc (saddr ^. Db.StakeAddressHashRaw)]
              pure (saddr ^. Db.StakeAddressHashRaw, rwd ^. Db.RewardType, rwd ^. Db.RewardAmount)
    pure . Map.fromList . map collapse $ List.groupOn fst (map convert res)
  where
    convert :: (Value ByteString, Value RewardSource, Value DbLovelace) -> (Generic.StakeCred, (RewardSource, DbLovelace))
    convert (Value cred, Value source, Value amount) = (Generic.StakeCred cred, (source, amount))

    collapse :: [(Generic.StakeCred, (RewardSource, DbLovelace))] -> (Generic.StakeCred, [(RewardSource, DbLovelace)])
    collapse xs =
      case xs of
        [] -> panic "queryRewardMap.collapse: Unexpected empty list" -- Impossible
        x:_ -> (fst x, List.sort $ map snd xs)

diffRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred [(RewardSource, DbLovelace)]
    -> Map Generic.StakeCred [(RewardSource, Coin)]
    -> ReaderT SqlBackend m ()
diffRewardMap tracer epochNo dbMap ledgerMap = do
    when (Map.size diffMap > 0) $ do
      reportCount tracer epochNo diffMap
      liftIO . logError tracer $ Text.unlines
            [ "Rewards differ between ledger and db-sync."
            , "Please report at https://github.com/input-output-hk/cardano-db-sync/issues."
            ]
  where
    keys :: [Generic.StakeCred]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map Generic.StakeCred ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])
    diffMap = List.foldl' mkDiff mempty keys

    mkDiff
        :: Map Generic.StakeCred ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])
        -> Generic.StakeCred
        -> Map Generic.StakeCred ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])
    mkDiff !acc addr =
        case (Map.lookup addr dbMap, Map.lookup addr ledgerMap) of
          (Just s, Just xs) ->
                if fromIntegral (sum $ map (Db.unDbLovelace . snd) s) == sum (map (unCoin . snd) xs)
                  then acc
                  else Map.insert addr (s, xs) acc
          (Nothing, Just xs) -> Map.insert addr ([], xs) acc
          (Just s, Nothing) -> Map.insert addr (s, []) acc
          (Nothing, Nothing) -> acc

reportCount
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])
    -> ReaderT SqlBackend m ()
reportCount tracer epochNo diffMap =
    mapM_ report $ Map.toList diffMap
  where
    report
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred,  ([(RewardSource, DbLovelace)], [(RewardSource, Coin)]))
        -> ReaderT SqlBackend m ()
    report (cred, (xs, _coin)) = do
      count <- queryRewardEntries epochNo cred
      unless (count == length xs) $
        liftIO . logError tracer $
          mconcat [ "reportCount: ", textShow count, " == ", textShow (length xs), " ???" ]

queryRewardEntries
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> Generic.StakeCred -> ReaderT SqlBackend m Int
queryRewardEntries (EpochNo epochNo) (Generic.StakeCred cred) = do
  res <- select . from $ \ (rwd `InnerJoin` saddr) -> do
            on (rwd ^. Db.RewardAddrId ==. saddr ^. Db.StakeAddressId)
            where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
            where_ (saddr ^. Db.StakeAddressHashRaw ==. val cred)
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)
