{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.StakeReward.History (
  reportStakeRewardHistory,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (textShow)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Text.Printf (printf)

reportStakeRewardHistory :: Text -> IO ()
reportStakeRewardHistory saddr = do
  xs <- DB.runDbNoLoggingEnv (queryHistoryStakeRewards saddr)
  if List.null xs
    then errorMsg
    else renderRewards saddr xs
  where
    errorMsg :: IO ()
    errorMsg =
      Text.putStrLn $
        mconcat
          [ "Error: Stake address '"
          , saddr
          , "' not found in database.\n"
          , "Expecting as Bech32 encoded stake address. eg 'stake1...'."
          ]

-- -------------------------------------------------------------------------------------------------

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

queryHistoryStakeRewards :: MonadIO m => Text -> DB.DbAction m [EpochReward]
queryHistoryStakeRewards address = do
  maxEpoch <- DB.queryLatestMemberRewardEpochNo
  delegations <- DB.queryDelegationHistory address maxEpoch
  mapM queryReward delegations
  where
    queryReward ::
      MonadIO m =>
      (DB.StakeAddressId, Word64, UTCTime, DB.DbLovelace, DB.PoolHashId) ->
      DB.DbAction m EpochReward
    queryReward (saId, en, date, DB.DbLovelace delegated, poolId) = do
      mReward <- DB.queryRewardForEpoch en saId
      mPoolTicker <- DB.queryPoolTicker poolId

      let reward = maybe 0 DB.unDbLovelace mReward
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

renderRewards :: Text -> [EpochReward] -> IO ()
renderRewards saddr xs = do
  Text.putStrLn $ mconcat ["\nRewards for: ", saddr, "\n"]
  putStrLn " epoch |      reward_date        |    delegated   | pool_id | ticker |   reward     | RoS (%pa)"
  putStrLn "-------+-------------------------+----------------+---------+--------+--------------+-----------"
  mapM_ renderReward xs
  putStrLn ""
  where
    renderReward :: EpochReward -> IO ()
    renderReward er =
      Text.putStrLn $
        mconcat
          [ leftPad 6 (textShow $ erEpochNo er)
          , separator
          , textShow (erDate er)
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
