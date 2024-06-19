{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Validate (
  validateEpochRewards,
) where

import Cardano.BM.Trace (logError, logInfo, logWarning)
import Cardano.Db (DbLovelace, RewardSource)
import qualified Cardano.Db as Db
import Cardano.DbSync.AppT (App, MonadAppDB (..), askNetwork, askTrace)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types
import Cardano.DbSync.Util (textShow)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Shelley.Rewards as Ledger
import Cardano.Prelude hiding (from, on)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Database.Esqueleto.Experimental (
  InnerJoin (InnerJoin),
  SqlBackend,
  Value (Value),
  desc,
  from,
  not_,
  on,
  orderBy,
  select,
  table,
  val,
  where_,
  (:&) ((:&)),
  (==.),
  (^.),
 )
import GHC.Err (error)

{- HLINT ignore "Fuse on/on" -}
{- HLINT ignore "Reduce duplication" -}

validateEpochRewards ::
  EpochNo ->
  EpochNo ->
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  App ()
validateEpochRewards _earnedEpochNo spendableEpochNo rmap = do
  tracer <- askTrace
  actualCount <- dbQueryToApp $ Db.queryNormalEpochRewardCount (unEpochNo spendableEpochNo)
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
      logFullRewardMap spendableEpochNo (convertPoolRewards rmap)
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
  EpochNo ->
  Generic.Rewards ->
  App ()
logFullRewardMap epochNo ledgerMap = do
  network <- askNetwork
  dbMap <- dbQueryToApp $ queryRewardMap epochNo
  when (Map.size dbMap > 0 && Map.size (Generic.unRewards ledgerMap) > 0) $
    diffRewardMap dbMap (Map.mapKeys (Generic.stakingCredHash network) $ Map.map convert $ Generic.unRewards ledgerMap)
  where
    convert :: Set Generic.Reward -> [(RewardSource, Coin)]
    convert = map (\rwd -> (Generic.rewardSource rwd, Generic.rewardAmount rwd)) . Set.toList

diffRewardMap ::
  Map ByteString [(RewardSource, DbLovelace)] ->
  Map ByteString [(RewardSource, Coin)] ->
  App ()
diffRewardMap dbMap ledgerMap = do
  tracer <- askTrace
  when (Map.size diffMap > 0) $ do
    liftIO $ logError tracer "diffRewardMap:"
    mapM_ (liftIO . logError tracer . render) $ Map.toList diffMap
  where
    keys :: [ByteString]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map ByteString ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])
    diffMap = List.foldl' mkDiff mempty keys

    mkDiff ::
      Map ByteString ([(RewardSource, DbLovelace)], [(RewardSource, Coin)]) ->
      ByteString ->
      Map ByteString ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])
    mkDiff !acc addr =
      case (Map.lookup addr dbMap, Map.lookup addr ledgerMap) of
        (Just xs, Just ys) ->
          if fromIntegral (sum $ map (Db.unDbLovelace . snd) xs) == sum (map (unCoin . snd) ys)
            then acc
            else Map.insert addr (xs, ys) acc
        (Nothing, Just ys) -> Map.insert addr ([], ys) acc
        (Just xs, Nothing) -> Map.insert addr (xs, []) acc
        (Nothing, Nothing) -> acc

    render :: (ByteString, ([(RewardSource, DbLovelace)], [(RewardSource, Coin)])) -> Text
    render (cred, (xs, ys)) = mconcat ["  ", show cred, ": ", show xs, " /= ", show ys]

------------------------------------------------------------------------------------------
-- Querries
------------------------------------------------------------------------------------------

queryRewardMap ::
  (MonadBaseControl IO m, MonadIO m) =>
  EpochNo ->
  ReaderT SqlBackend m (Map ByteString [(RewardSource, DbLovelace)])
queryRewardMap (EpochNo epochNo) = do
  res <- select $ do
    (rwd :& saddr) <-
      from
        $ table @Db.Reward
          `InnerJoin` table @Db.StakeAddress
        `on` ( \(rwd :& saddr) ->
                rwd ^. Db.RewardAddrId ==. saddr ^. Db.StakeAddressId
             )
    where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
    where_ (not_ $ rwd ^. Db.RewardType ==. val Db.RwdDepositRefund)
    where_ (not_ $ rwd ^. Db.RewardType ==. val Db.RwdTreasury)
    where_ (not_ $ rwd ^. Db.RewardType ==. val Db.RwdReserves)
    orderBy [desc (saddr ^. Db.StakeAddressHashRaw)]
    pure (saddr ^. Db.StakeAddressHashRaw, rwd ^. Db.RewardType, rwd ^. Db.RewardAmount)

  pure . Map.fromList . map collapse $ List.groupOn fst (map convert res)
  where
    convert :: (Value ByteString, Value RewardSource, Value DbLovelace) -> (ByteString, (RewardSource, DbLovelace))
    convert (Value cred, Value source, Value amount) = (cred, (source, amount))

    collapse :: [(ByteString, (RewardSource, DbLovelace))] -> (ByteString, [(RewardSource, DbLovelace)])
    collapse xs =
      case xs of
        [] -> error "queryRewardMap.collapse: Unexpected empty list" -- Impossible
        x : _ -> (fst x, List.sort $ map snd xs)
