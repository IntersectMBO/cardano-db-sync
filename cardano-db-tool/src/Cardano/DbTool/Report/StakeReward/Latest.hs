{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.StakeReward.Latest (
  reportEpochStakeRewards,
  reportLatestStakeRewards,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (fromMaybe, textShow)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Text.Printf (printf)

reportEpochStakeRewards :: Word64 -> [Text] -> IO ()
reportEpochStakeRewards epochNum saddr = do
  xs <- catMaybes <$> DB.runDbStandaloneSilent (mapM (queryEpochStakeRewards epochNum) saddr)
  renderRewards xs

reportLatestStakeRewards :: [Text] -> IO ()
reportLatestStakeRewards saddr = do
  xs <- catMaybes <$> DB.runDbStandaloneSilent (mapM queryLatestStakeRewards saddr)
  renderRewards xs

data EpochReward = EpochReward
  { erAddressId :: !DB.StakeAddressId
  , erEpochNo :: !Word64
  , erDate :: !UTCTime
  , erAddress :: !Text
  , erPoolId :: !Word64
  , erPoolTicker :: !Text
  , erReward :: !DB.Ada
  , erDelegated :: !DB.Ada
  , erPercent :: !Double
  }

queryEpochStakeRewards :: Word64 -> Text -> DB.DbM (Maybe EpochReward)
queryEpochStakeRewards epochNum address = do
  mdel <- DB.queryDelegationForEpoch address epochNum
  case mdel of
    Nothing -> pure Nothing
    Just delegation -> Just <$> queryReward epochNum address delegation

queryLatestStakeRewards :: Text -> DB.DbM (Maybe EpochReward)
queryLatestStakeRewards address = do
  epochNum <- DB.queryLatestMemberRewardEpochNo
  mdel <- DB.queryDelegationForEpoch address epochNum
  case mdel of
    Nothing -> pure Nothing
    Just delegation -> Just <$> queryReward epochNum address delegation

queryReward ::
  Word64 ->
  Text ->
  (DB.StakeAddressId, UTCTime, DB.DbLovelace, DB.PoolHashId) ->
  DB.DbM EpochReward
queryReward en address (saId, date, DB.DbLovelace delegated, poolId) = do
  mRewardAmount <- DB.queryRewardAmount en saId
  mPoolTicker <- DB.queryPoolTicker poolId

  let reward = maybe 0 DB.unDbLovelace mRewardAmount
      poolTicker = fromMaybe "???" mPoolTicker

  pure $
    EpochReward
      { erAddressId = saId
      , erPoolId = fromIntegral $ DB.getPoolHashId poolId
      , erPoolTicker = poolTicker
      , erEpochNo = en
      , erDate = date
      , erAddress = address
      , erReward = DB.word64ToAda reward
      , erDelegated = DB.word64ToAda delegated
      , erPercent = rewardPercent reward (if delegated == 0 then Nothing else Just delegated)
      }

renderRewards :: [EpochReward] -> IO ()
renderRewards xs = do
  mapM_ Text.putStrLn (renderTable cols (map toRow (List.sortOn (Down . erDelegated) xs)))
  putStrLn ""
  where
    cols :: [(Align, Text)]
    cols =
      [ (AlignRight, "epoch")
      , (AlignLeft, "stake_address")
      , (AlignRight, "delegated")
      , (AlignRight, "pool_id")
      , (AlignLeft, "ticker")
      , (AlignRight, "reward")
      , (AlignRight, "RoS (%pa)")
      ]

    toRow :: EpochReward -> [Text]
    toRow er =
      [ textShow (erEpochNo er)
      , erAddress er
      , DB.renderAda (erDelegated er)
      , textShow (erPoolId er)
      , erPoolTicker er
      , specialRenderAda (erReward er)
      , Text.pack (if erPercent er == 0.0 then "0.0" else printf "%.3f" (erPercent er))
      ]

    specialRenderAda :: DB.Ada -> Text
    specialRenderAda ada = if ada == 0 then "0.0" else DB.renderAda ada

rewardPercent :: Word64 -> Maybe Word64 -> Double
rewardPercent reward mDelegated =
  case mDelegated of
    Nothing -> 0.0
    Just deleg -> 100.0 * 365.25 / 5.0 * fromIntegral reward / fromIntegral deleg
