{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Validate
  ( validateEpochRewards
  ) where

import           Cardano.Prelude hiding (from, on)

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Cardano.Db (DbLovelace, RewardSource)
import qualified Cardano.Db as Db

import           Cardano.Ledger.BaseTypes (Network)
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), countRows, desc, from, on,
                   orderBy, select, sum_, val, where_, (==.), (^.))

import           Database.Persist.Sql (SqlBackend)


validateEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Network -> EpochNo -> Map (Ledger.StakeCredential c) Coin
    -> ReaderT SqlBackend m ()
validateEpochRewards tracer nw currentEpoch rmap = do
    actual <- queryEpochRewardTotal currentEpoch
    if actual /= expected
        then do
          liftIO . logWarning tracer $ mconcat
                      [ "validateEpochRewards: rewards spendable in epoch "
                      , textShow (unEpochNo currentEpoch), " expected total of ", textShow expected
                      , " ADA but got " , textShow actual, " ADA"
                      ]
          logFullRewardMap currentEpoch (convertRewardMap nw rmap)
        else
          liftIO . logInfo tracer $ mconcat
                      [ "validateEpochRewards: total rewards that become spendable in epoch "
                      , textShow (unEpochNo currentEpoch), " is ", textShow actual, " ADA"
                      ]
  where
    expected :: Db.Ada
    expected = Db.word64ToAda . fromIntegral . sum $ map unCoin (Map.elems rmap)

-- -------------------------------------------------------------------------------------------------

queryEpochRewardTotal
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> ReaderT SqlBackend m Db.Ada
queryEpochRewardTotal (EpochNo epochNo) = do
  res <- select . from $ \ rwd -> do
            where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
            pure (sum_ $ rwd ^. Db.RewardAmount)
  pure $ Db.unValueSumAda (listToMaybe res)

-- -------------------------------------------------------------------------------------------------

convertRewardMap :: Network -> Map (Ledger.StakeCredential c) Coin -> Map Generic.StakeCred Coin
convertRewardMap nw = Map.mapKeys (Generic.toStakeCred nw)


logFullRewardMap
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> Map Generic.StakeCred Coin -> ReaderT SqlBackend m ()
logFullRewardMap epochNo ledgerMap = do
    dbMap <- queryRewardMap epochNo
    when (Map.size dbMap > 0 && Map.size ledgerMap > 0) $
      diffRewardMap epochNo dbMap ledgerMap


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
    => EpochNo -> Map Generic.StakeCred [(RewardSource, DbLovelace)] -> Map Generic.StakeCred Coin
    -> ReaderT SqlBackend m ()
diffRewardMap epochNo dbMap ledgerMap = do
    liftIO $ do
      putStrLn $ "dbMap length: " ++ show (Map.size dbMap)
      putStrLn $ "ledgerMap length: " ++ show (Map.size ledgerMap)
      putStrLn $ "diffMap length: " ++ show (Map.size diffMap)
      mapM_ reportDiff $ Map.toList diffMap
    when (Map.size diffMap > 0) $ do
      reportCount epochNo diffMap
      panicAbort $ Text.unlines
            [ "Rewards differ between ledger and db-sync."
            , "Please report at https://github.com/input-output-hk/cardano-db-sync/issues."
            ]
  where
    keys :: [Generic.StakeCred]
    keys = List.nubOrd (Map.keys dbMap ++ Map.keys ledgerMap)

    diffMap :: Map Generic.StakeCred ([(RewardSource, DbLovelace)], Coin)
    diffMap = List.foldl' mkDiff mempty keys

    mkDiff
        :: Map Generic.StakeCred ([(RewardSource, DbLovelace)], Coin) -> Generic.StakeCred
        -> Map Generic.StakeCred ([(RewardSource, DbLovelace)], Coin)
    mkDiff !acc addr =
        case (Map.lookup addr dbMap, Map.lookup addr ledgerMap) of
          (Just s, Just coin) ->
                if fromIntegral (sum $ map (Db.unDbLovelace . snd) s) == unCoin coin
                  then acc
                  else Map.insert addr (s, coin) acc
          (Nothing, Just coin) -> Map.insert addr (mempty, coin) acc
          (Just s, Nothing) -> Map.insert addr (s, Coin (-1)) acc
          (Nothing, Nothing) -> acc

reportDiff :: (Generic.StakeCred, ([(RewardSource, DbLovelace)], Coin)) -> IO ()
reportDiff (cred, (xs, coin)) =
    putStrLn . concat $
      [ show cred, " (", show (sum $ map (Db.unDbLovelace . snd) xs), ", ", show (length xs), "): "
      , show xs, ", ", show coin
      ]

reportCount
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> Map Generic.StakeCred ([(RewardSource, DbLovelace)], Coin)
    -> ReaderT SqlBackend m ()
reportCount epochNo diffMap =
    when (Map.size diffMap > 0) $
      mapM_ report $ Map.toList diffMap
  where
    report
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred,  ([(RewardSource, DbLovelace)], Coin))
        -> ReaderT SqlBackend m ()
    report (cred, (xs, _coin)) = do
      count <- queryRewardEntries epochNo cred
      unless (count == length xs) $
        putStrLn $ "reportCount: " ++ show count ++ " == " ++ show (length xs) ++ " ???"


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
