{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Report.StakeReward.History (
  reportStakeRewardHistory,
) where

import Cardano.Db
import Cardano.DbTool.Report.Display
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.List as List
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

reportStakeRewardHistory :: Text -> IO ()
reportStakeRewardHistory saddr = do
  xs <- runDbNoLoggingEnv (queryHistoryStakeRewards saddr)
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

queryHistoryStakeRewards :: MonadIO m => Text -> ReaderT SqlBackend m [EpochReward]
queryHistoryStakeRewards address = do
  maxEpoch <- queryLatestMemberRewardEpochNo
  mapM queryReward =<< queryDelegation maxEpoch
  where
    queryDelegation ::
      MonadIO m =>
      Word64 ->
      ReaderT SqlBackend m [(StakeAddressId, Word64, UTCTime, DbLovelace, PoolHashId)]
    queryDelegation maxEpoch = do
      res <- select $ do
        (ep :& es :& saddr) <-
          from
            $ table @Epoch
              `innerJoin` table @EpochStake
            `on` (\(ep :& es) -> ep ^. EpochNo ==. es ^. EpochStakeEpochNo)
              `innerJoin` table @StakeAddress
            `on` (\(_ep :& es :& saddr) -> saddr ^. StakeAddressId ==. es ^. EpochStakeAddrId)
        where_ (saddr ^. StakeAddressView ==. val address)
        where_ (es ^. EpochStakeEpochNo <=. val maxEpoch)
        pure
          ( es ^. EpochStakeAddrId
          , es ^. EpochStakeEpochNo
          , ep ^. EpochEndTime
          , es ^. EpochStakeAmount
          , es ^. EpochStakePoolId
          )
      pure $ map unValue5 res

    queryReward ::
      MonadIO m =>
      (StakeAddressId, Word64, UTCTime, DbLovelace, PoolHashId) ->
      ReaderT SqlBackend m EpochReward
    queryReward (saId, en, date, DbLovelace delegated, poolId) = do
      res <- select $ do
        (saddr :& rwd :& ep) <-
          from
            $ table @StakeAddress
              `innerJoin` table @Reward
            `on` (\(saddr :& rwd) -> saddr ^. StakeAddressId ==. rwd ^. RewardAddrId)
              `innerJoin` table @Epoch
            `on` (\(_saddr :& rwd :& ep) -> ep ^. EpochNo ==. rwd ^. RewardEarnedEpoch)
        where_ (ep ^. EpochNo ==. val en)
        where_ (saddr ^. StakeAddressId ==. val saId)
        orderBy [asc (ep ^. EpochNo)]
        pure (rwd ^. RewardAmount)

      mtn <- select $ do
        pod <- from $ table @OffChainPoolData
        where_ (pod ^. OffChainPoolDataPoolId ==. val poolId)
        -- Use the `id` column as a proxy for time where larger `id` means later time.
        orderBy [desc (pod ^. OffChainPoolDataId)]
        pure (pod ^. OffChainPoolDataTickerName)

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
