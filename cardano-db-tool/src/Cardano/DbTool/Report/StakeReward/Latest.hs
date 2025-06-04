{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.StakeReward.Latest (
  reportEpochStakeRewards,
  reportLatestStakeRewards,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (textShow)
import Control.Monad.IO.Class (MonadIO)
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
  xs <- catMaybes <$> DB.runDbNoLoggingEnv (mapM (queryEpochStakeRewards epochNum) saddr)
  renderRewards xs

reportLatestStakeRewards :: [Text] -> IO ()
reportLatestStakeRewards saddr = do
  xs <- catMaybes <$> DB.runDbNoLoggingEnv (mapM queryLatestStakeRewards saddr)
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

queryEpochStakeRewards :: MonadIO m => Word64 -> Text -> DB.DbAction m (Maybe EpochReward)
queryEpochStakeRewards epochNum address = do
  mdel <- DB.queryDelegationForEpoch address epochNum
  case mdel of
    Nothing -> pure Nothing
    Just delegation -> Just <$> queryReward epochNum address delegation

queryLatestStakeRewards :: MonadIO m => Text -> DB.DbAction m (Maybe EpochReward)
queryLatestStakeRewards address = do
  epochNum <- DB.queryLatestMemberRewardEpochNo
  mdel <- DB.queryDelegationForEpoch address epochNum
  case mdel of
    Nothing -> pure Nothing
    Just delegation -> Just <$> queryReward epochNum address delegation

queryReward ::
  MonadIO m =>
  Word64 ->
  Text ->
  (DB.StakeAddressId, UTCTime, DB.DbLovelace, DB.PoolHashId) ->
  DB.DbAction m EpochReward
queryReward en address (saId, date, DB.DbLovelace delegated, poolId) = do
  mRewardAmount <- DB.queryRewardAmount en saId
  mPoolTicker <- DB.queryPoolTicker poolId

  let reward = maybe 0 DB.unDbLovelace mRewardAmount
      poolTicker = maybe "???" id mPoolTicker

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
          , leftPad 14 (DB.renderAda (erDelegated er))
          , separator
          , leftPad 7 (textShow $ erPoolId er)
          , separator
          , rightPad 6 (erPoolTicker er)
          , separator
          , leftPad 12 (specialRenderAda (erReward er))
          , separator
          , Text.pack (if erPercent er == 0.0 then "   0.0" else printf "%8.3f" (erPercent er))
          ]

    specialRenderAda :: DB.Ada -> Text
    specialRenderAda ada = if ada == 0 then "0.0     " else DB.renderAda ada

rewardPercent :: Word64 -> Maybe Word64 -> Double
rewardPercent reward mDelegated =
  case mDelegated of
    Nothing -> 0.0
    Just deleg -> 100.0 * 365.25 / 5.0 * fromIntegral reward / fromIntegral deleg
