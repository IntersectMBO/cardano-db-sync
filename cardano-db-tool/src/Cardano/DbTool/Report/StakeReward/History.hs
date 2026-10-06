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
  result <- DB.runDbStandaloneSilent (queryHistoryStakeRewards saddr)
  case result of
    Left err -> Text.putStrLn $ renderHistoryError saddr err
    Right xs -> renderRewards saddr xs

data HistoryError
  = EpochSyncUninitialised
  | EpochSyncDisabled
  | EpochFinalizedEmpty
  | StakeAddressNotFound
  | NoDelegationHistory !Word64

renderHistoryError :: Text -> HistoryError -> Text
renderHistoryError saddr err =
  case err of
    EpochSyncUninitialised ->
      mconcat
        [ "Error: The 'epoch_sync_enabled' table has no row.\n"
        , "This row is written by cardano-db-sync when it starts up. Start cardano-db-sync"
        , " (with \"disable_epoch\" unset or false in the \"insert_options\" section of its config)"
        , " to initialise it."
        ]
    EpochSyncDisabled ->
      mconcat
        [ "Error: Epoch data is disabled in this database (epoch_sync_enabled.enabled = false).\n"
        , "This report requires epoch data. Remove \"disable_epoch\": true from (or set it to false in)"
        , " the \"insert_options\" section of the cardano-db-sync config and restart cardano-db-sync."
        ]
    EpochFinalizedEmpty ->
      mconcat
        [ "Error: The 'epoch_finalized' table is empty.\n"
        , "It is backfilled by cardano-db-sync on startup when \"disable_epoch\" is false. Restart"
        , " cardano-db-sync and wait for the 'epoch_finalized backfill complete.' log message."
        ]
    StakeAddressNotFound ->
      mconcat
        [ "Error: Stake address '"
        , saddr
        , "' not found in database.\n"
        , "Expecting as Bech32 encoded stake address. eg 'stake1...'."
        ]
    NoDelegationHistory maxEpoch ->
      mconcat
        [ "Error: Stake address '"
        , saddr
        , "' has no entries in the 'epoch_stake' table up to epoch "
        , textShow maxEpoch
        , "."
        ]

-- -------------------------------------------------------------------------------------------------

data EpochReward = EpochReward
  { erAddressId :: !DB.StakeAddressId
  , erEpochNo :: !Word64
  , erDate :: !(Maybe UTCTime)
  , erAddress :: !Text
  , erPoolTicker :: !Text
  , erPoolView :: !Text
  , erReward :: !DB.Ada
  , erDelegated :: !DB.Ada
  , erPercent :: !Double
  }

queryHistoryStakeRewards :: Text -> DB.DbM (Either HistoryError [EpochReward])
queryHistoryStakeRewards address = do
  mEnabled <- DB.queryEpochSyncEnabled
  finalizedCount <- DB.queryEpochFinalizedCount
  mSaId <- DB.queryStakeAddressId address
  case (mEnabled, mSaId) of
    (Nothing, _) -> pure $ Left EpochSyncUninitialised
    (Just False, _) -> pure $ Left EpochSyncDisabled
    _ | finalizedCount == 0 -> pure $ Left EpochFinalizedEmpty
    (_, Nothing) -> pure $ Left StakeAddressNotFound
    (_, Just saId) -> do
      maxEpoch <- DB.queryLatestMemberRewardEpochNo
      delegations <- DB.queryDelegationHistory saId maxEpoch
      if List.null delegations
        then pure $ Left (NoDelegationHistory maxEpoch)
        else Right <$> mapM queryReward delegations
  where
    queryReward ::
      (DB.StakeAddressId, Word64, Maybe UTCTime, DB.DbLovelace, DB.PoolHashId) ->
      DB.DbM EpochReward
    queryReward (saId, en, date, DB.DbLovelace delegated, poolId) = do
      mReward <- DB.queryRewardForEpoch en saId poolId
      mPoolTicker <- DB.queryPoolTickerForEpoch poolId en
      mPoolView <- DB.queryPoolHashView poolId
      let reward = maybe 0 DB.unDbLovelace mReward
          poolTicker = fromMaybe "???" mPoolTicker

      pure $
        EpochReward
          { erAddressId = saId
          , erPoolTicker = poolTicker
          , erPoolView = maybe "???" shortenBech32 mPoolView
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
      , (AlignLeft, "stake pool")
      , (AlignLeft, "ticker")
      , (AlignRight, "reward")
      , (AlignRight, "RoS (%pa)")
      ]

    toRow :: EpochReward -> [Text]
    toRow er =
      [ textShow (erEpochNo er)
      , maybe "-" formatReportTime (erDate er)
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
