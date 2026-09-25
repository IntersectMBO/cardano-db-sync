{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.StakeReward.Latest (
  reportEpochStakeRewards,
  reportLatestStakeRewards,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (fromMaybe, textShow)
import Control.Monad (unless)
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import Text.Printf (printf)

reportEpochStakeRewards :: Word64 -> [Text] -> IO ()
reportEpochStakeRewards epochNum saddr = do
  result <- DB.runDbStandaloneSilent $ do
    latestEpoch <- DB.queryLatestMemberRewardEpochNo
    if epochNum > latestEpoch
      then pure $ Left latestEpoch
      else Right <$> mapM (queryEpochStakeRewards epochNum) saddr
  case result of
    Left latestEpoch ->
      Text.putStrLn $
        mconcat
          [ "Error: Rewards for epoch "
          , textShow epochNum
          , " have not been distributed yet. The latest epoch with rewards is "
          , textShow latestEpoch
          , "."
          ]
    Right xs -> renderResults xs

reportLatestStakeRewards :: [Text] -> IO ()
reportLatestStakeRewards saddr = do
  xs <- DB.runDbStandaloneSilent $ do
    epochNum <- DB.queryLatestMemberRewardEpochNo
    mapM (queryEpochStakeRewards epochNum) saddr
  renderResults xs

data EpochReward = EpochReward
  { erAddressId :: !DB.StakeAddressId
  , erEpochNo :: !Word64
  , erAddress :: !Text
  , erPoolTicker :: !Text
  , erPoolView :: !Text
  , erReward :: !DB.Ada
  , erDelegated :: !DB.Ada
  , erPercent :: !Double
  }

-- | Query the rewards for a stake address in the given epoch, or an error message explaining
-- why there are none.
queryEpochStakeRewards :: Word64 -> Text -> DB.DbM (Either Text EpochReward)
queryEpochStakeRewards epochNum address = do
  mSaId <- DB.queryStakeAddressId address
  case mSaId of
    Nothing ->
      pure . Left $
        mconcat
          [ "Error: Stake address '"
          , address
          , "' not found in database.\n"
          , "Expecting as Bech32 encoded stake address. eg 'stake1...'."
          ]
    Just saId -> do
      mStake <- DB.queryEpochStakeForAddress saId epochNum
      case mStake of
        Nothing ->
          pure . Left $
            mconcat
              [ "Error: Stake address '"
              , address
              , "' has no entry in the 'epoch_stake' table for epoch "
              , textShow epochNum
              , " (it was not delegated or had no stake in that epoch)."
              ]
        Just stake -> Right <$> queryReward epochNum address saId stake

queryReward ::
  Word64 ->
  Text ->
  DB.StakeAddressId ->
  (DB.DbLovelace, DB.PoolHashId) ->
  DB.DbM EpochReward
queryReward en address saId (DB.DbLovelace delegated, poolId) = do
  mRewardAmount <- DB.queryRewardForEpoch en saId poolId
  mPoolTicker <- DB.queryPoolTicker poolId
  mPoolView <- DB.queryPoolHashView poolId

  let reward = maybe 0 DB.unDbLovelace mRewardAmount
      poolTicker = fromMaybe "???" mPoolTicker

  pure $
    EpochReward
      { erAddressId = saId
      , erPoolTicker = poolTicker
      , erPoolView = maybe "???" shortenPoolId mPoolView
      , erEpochNo = en
      , erAddress = address
      , erReward = DB.word64ToAda reward
      , erDelegated = DB.word64ToAda delegated
      , erPercent = rewardPercent reward (if delegated == 0 then Nothing else Just delegated)
      }

-- | Render the table of rewards found (if any), followed by the errors for the stake addresses
-- where no rewards were found.
renderResults :: [Either Text EpochReward] -> IO ()
renderResults results = do
  let (errs, xs) = partitionEithers results
  unless (List.null xs) $ renderRewards xs
  mapM_ Text.putStrLn errs

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
      , (AlignLeft, "stake pool")
      , (AlignLeft, "ticker")
      , (AlignRight, "reward")
      , (AlignRight, "RoS (%pa)")
      ]

    toRow :: EpochReward -> [Text]
    toRow er =
      [ textShow (erEpochNo er)
      , erAddress er
      , DB.renderAda (erDelegated er)
      , erPoolView er
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
