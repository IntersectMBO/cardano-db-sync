{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Report.StakeReward.Latest (
  reportEpochStakeRewards,
  reportLatestStakeRewards,
) where

import Cardano.Db
import Cardano.DbTool.Report.Display
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (
  SqlBackend,
  Value (..),
  asc,
  desc,
  from,
  innerJoin,
  limit,
  max_,
  on,
  orderBy,
  select,
  table,
  unSqlBackendKey,
  val,
  where_,
  (<=.),
  (==.),
  (^.),
  type (:&) ((:&)),
 )
import Text.Printf (printf)

{- HLINT ignore "Fuse on/on" -}

reportEpochStakeRewards :: Word64 -> [Text] -> IO ()
reportEpochStakeRewards epochNum saddr = do
  xs <- catMaybes <$> runDbNoLoggingEnv (mapM (queryEpochStakeRewards epochNum) saddr)
  renderRewards xs

reportLatestStakeRewards :: [Text] -> IO ()
reportLatestStakeRewards saddr = do
  xs <- catMaybes <$> runDbNoLoggingEnv (mapM queryLatestStakeRewards saddr)
  renderRewards xs

-- -------------------------------------------------------------------------------------------------

data EpochReward = EpochReward
  { erAddressId :: !StakeAddressId
  , erEpochNo :: !Word64
  , erDate :: !UTCTime
  , erAddress :: !Text
  , erPoolId :: !Word64
  , erPoolTicker :: !Text
  , erReward :: !Ada
  , erDelegated :: !Ada
  , erPercent :: !Double
  }

queryEpochStakeRewards :: MonadIO m => Word64 -> Text -> ReaderT SqlBackend m (Maybe EpochReward)
queryEpochStakeRewards epochNum address = do
  mdel <- queryDelegation address epochNum
  maybe (pure Nothing) ((fmap . fmap) Just (queryReward epochNum address)) mdel

queryLatestStakeRewards :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe EpochReward)
queryLatestStakeRewards address = do
  epochNum <- queryLatestMemberRewardEpochNo
  mdel <- queryDelegation address epochNum
  maybe (pure Nothing) ((fmap . fmap) Just (queryReward epochNum address)) mdel
  where
    -- Find the latest epoch where member rewards have been distributed.
    -- Can't use the Reward table for this because that table may have been partially
    -- populated for the next epcoh.
    queryLatestMemberRewardEpochNo :: MonadIO m => ReaderT SqlBackend m Word64
    queryLatestMemberRewardEpochNo = do
      res <- select $ do
        blk <- from $ table @Block
        where_ (isJust $ blk ^. BlockEpochNo)
        pure $ max_ (blk ^. BlockEpochNo)
      pure $ maybe 0 (pred . pred) (join $ unValue =<< listToMaybe res)

queryDelegation ::
  MonadIO m =>
  Text ->
  Word64 ->
  ReaderT SqlBackend m (Maybe (StakeAddressId, UTCTime, DbLovelace, PoolHashId))
queryDelegation address epochNum = do
  res <- select $ do
    (ep :& es :& saddr) <-
      from
        $ table @Epoch
          `innerJoin` table @EpochStake
        `on` (\(ep :& es) -> ep ^. EpochNo ==. es ^. EpochStakeEpochNo)
          `innerJoin` table @StakeAddress
        `on` (\(_ep :& es :& saddr) -> saddr ^. StakeAddressId ==. es ^. EpochStakeAddrId)

    where_ (saddr ^. StakeAddressView ==. val address)
    where_ (es ^. EpochStakeEpochNo <=. val epochNum)
    orderBy [desc (es ^. EpochStakeEpochNo)]
    limit 1
    pure
      ( es ^. EpochStakeAddrId
      , ep ^. EpochEndTime
      , es ^. EpochStakeAmount
      , es ^. EpochStakePoolId
      )
  pure $ fmap unValue4 (listToMaybe res)

queryReward ::
  MonadIO m =>
  Word64 ->
  Text ->
  (StakeAddressId, UTCTime, DbLovelace, PoolHashId) ->
  ReaderT SqlBackend m EpochReward
queryReward en address (saId, date, DbLovelace delegated, poolId) = do
  res <- select $ do
    (ep :& reward :& saddr) <-
      from
        $ table @Epoch
          `innerJoin` table @Reward
        `on` (\(ep :& reward) -> ep ^. EpochNo ==. reward ^. RewardEarnedEpoch)
          `innerJoin` table @StakeAddress
        `on` (\(_ep :& reward :& saddr) -> saddr ^. StakeAddressId ==. reward ^. RewardAddrId)
    where_ (ep ^. EpochNo ==. val en)
    where_ (saddr ^. StakeAddressId ==. val saId)
    orderBy [asc (ep ^. EpochNo)]
    pure (reward ^. RewardAmount)
  mtn <- select $ do
    pod <- from $ table @PoolOfflineData
    where_ (pod ^. PoolOfflineDataPoolId ==. val poolId)
    -- Use the `id` column as a proxy for time where larger `id` means later time.
    orderBy [desc (pod ^. PoolOfflineDataId)]
    pure (pod ^. PoolOfflineDataTickerName)

  let reward = maybe 0 (unDbLovelace . unValue) (listToMaybe res)
  pure $
    EpochReward
      { erAddressId = saId
      , erPoolId = fromIntegral $ unSqlBackendKey (unPoolHashKey poolId)
      , erPoolTicker = maybe "???" unValue (listToMaybe mtn)
      , erEpochNo = en
      , erDate = date
      , erAddress = address
      , erReward = word64ToAda reward
      , erDelegated = word64ToAda delegated
      , erPercent = rewardPercent reward (if delegated == 0 then Nothing else Just delegated)
      }

renderRewards :: [EpochReward] -> IO ()
renderRewards xs = do
  putStrLn " epoch |                       stake_address                         |   delegated    | pool_id | ticker |    reward    | RoS (%pa)"
  putStrLn "-------+-------------------------------------------------------------+----------------+-------- +--------+--------------+-----------"
  mapM_ renderReward (List.sortOn (Down . erDelegated) xs)
  putStrLn ""
  where
    renderReward :: EpochReward -> IO ()
    renderReward er =
      Text.putStrLn $
        mconcat
          [ leftPad 6 (textShow $ erEpochNo er)
          , separator
          , erAddress er
          , separator
          , leftPad 14 (renderAda (erDelegated er))
          , separator
          , leftPad 7 (textShow $ erPoolId er)
          , separator
          , rightPad 6 (erPoolTicker er)
          , separator
          , leftPad 12 (specialRenderAda (erReward er))
          , separator
          , Text.pack (if erPercent er == 0.0 then "   0.0" else printf "%8.3f" (erPercent er))
          ]

    specialRenderAda :: Ada -> Text
    specialRenderAda ada = if ada == 0 then "0.0     " else renderAda ada

rewardPercent :: Word64 -> Maybe Word64 -> Double
rewardPercent reward mDelegated =
  case mDelegated of
    Nothing -> 0.0
    Just deleg -> 100.0 * 365.25 / 5.0 * fromIntegral reward / fromIntegral deleg
