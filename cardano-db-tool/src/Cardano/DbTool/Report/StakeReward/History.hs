{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.StakeReward.History (
  reportStakeRewardHistory,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (fromMaybe, textShow)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Text.Printf (printf)

reportStakeRewardHistory :: Text -> IO ()
reportStakeRewardHistory saddr = do
  xs <- DB.runDbStandaloneSilent (queryHistoryStakeRewards saddr)
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

queryHistoryStakeRewards :: Text -> DB.DbM [EpochReward]
queryHistoryStakeRewards address = do
  maxEpoch <- DB.queryLatestMemberRewardEpochNo
  delegations <- DB.queryDelegationHistory address maxEpoch
  mapM queryReward delegations
  where
    queryReward ::
      (DB.StakeAddressId, Word64, UTCTime, DB.DbLovelace, DB.PoolHashId) ->
      DB.DbM EpochReward
    queryReward (saId, en, date, DB.DbLovelace delegated, poolId) = do
      mReward <- DB.queryRewardForEpoch en saId
      mPoolTicker <- DB.queryPoolTicker poolId

      let reward = maybe 0 DB.unDbLovelace mReward
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

renderRewards :: Text -> [EpochReward] -> IO ()
renderRewards saddr xs = do
  Text.putStrLn $ mconcat ["\nRewards for: ", saddr, "\n"]
  mapM_ Text.putStrLn (renderTable cols (map toRow xs))
  putStrLn ""
  where
    cols :: [(Align, Text)]
    cols =
      [ (AlignRight, "epoch")
      , (AlignLeft, "reward_date")
      , (AlignRight, "delegated")
      , (AlignRight, "pool_id")
      , (AlignLeft, "ticker")
      , (AlignRight, "reward")
      , (AlignRight, "RoS (%pa)")
      ]

    toRow :: EpochReward -> [Text]
    toRow er =
      [ textShow (erEpochNo er)
      , formatReportTime (erDate er)
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
