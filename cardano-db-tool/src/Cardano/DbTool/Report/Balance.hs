{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbTool.Report.Balance
  ( reportBalance
  ) where

import           Cardano.Db
import           Cardano.DbTool.Report.Display

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Fixed (Micro)
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import           Data.Ord (Down (..))
import           Data.Text (Text)
import qualified Data.Text.IO as Text

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), from, just, on, select,
                   sum_, val, where_, (&&.), (<=.), (==.), (^.))
import           Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Redundant ^." -}

reportBalance :: [Text] -> IO ()
reportBalance saddr = do
  xs <- catMaybes <$> runDbNoLoggingEnv (mapM queryStakeAddressBalance saddr)
  renderBalances xs

-- -------------------------------------------------------------------------------------------------

data Balance = Balance
  { balAddressId :: !StakeAddressId
  , balAddress :: !Text
  , balInputs :: !Ada
  , balOutputs :: !Ada
  , balFees :: !Ada
  , balDeposit :: !Ada
  , balRewards :: !Ada
  , balWithdrawals :: !Ada
  , balTotal :: !Ada
  }


queryStakeAddressBalance :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe Balance)
queryStakeAddressBalance address = do
    mSaId <- queryStakeAddressId
    case mSaId of
      Nothing -> pure Nothing
      Just saId -> Just <$> queryBalance saId
  where
    queryStakeAddressId :: MonadIO m => ReaderT SqlBackend m (Maybe StakeAddressId)
    queryStakeAddressId = do
      res <- select . from $ \ saddr -> do
                where_ (saddr ^. StakeAddressView ==. val address)
                pure (saddr ^. StakeAddressId)
      pure $ fmap unValue (listToMaybe res)

    queryBalance :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Balance
    queryBalance saId = do
      inputs <- queryInputs saId
      (outputs, fees, deposit) <- queryOutputs saId
      rewards <- queryRewards saId
      withdrawals <- queryWithdrawals saId
      pure $ Balance
                { balAddressId = saId
                , balAddress = address
                , balInputs = inputs
                , balOutputs = outputs
                , balFees  = fees
                , balDeposit = deposit
                , balRewards = rewards
                , balWithdrawals  = withdrawals
                , balTotal = inputs - outputs + rewards - withdrawals
                }

    queryInputs :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Ada
    queryInputs saId = do
      res <- select . from $ \ txo -> do
                where_ (txo ^. TxOutStakeAddressId ==. just (val saId))
                pure (sum_ (txo ^. TxOutValue))
      pure $ unValueSumAda (listToMaybe res)

    queryRewards :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Ada
    queryRewards saId = do
      -- This query does not run unless we are pretty close to the chain tip.
      -- Therefore to get current rewards, we limit the cacluation to current epoch minus 2.
      currentEpoch <- queryLatestEpochNo
      res <- select . from $ \ rwd -> do
                where_ (rwd ^. RewardAddrId ==. val saId)
                where_ (rwd ^. RewardSpendableEpoch <=. val currentEpoch)
                pure (sum_ (rwd ^. RewardAmount))
      pure $ unValueSumAda (listToMaybe res)

    queryWithdrawals :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Ada
    queryWithdrawals saId = do
      res <- select . from $ \ wdrl -> do
                where_ (wdrl ^. WithdrawalAddrId ==. val saId)
                pure (sum_ (wdrl ^. WithdrawalAmount))
      pure $ unValueSumAda (listToMaybe res)

    queryOutputs :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m (Ada, Ada, Ada)
    queryOutputs saId = do
      res <- select . from $ \ (txOut `InnerJoin` tx `InnerJoin` txIn) -> do
                on (txOut ^. TxOutTxId ==. tx ^. TxId)
                on (txIn ^. TxInTxOutId ==. tx ^. TxId &&. txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex)
                where_ (txOut ^. TxOutStakeAddressId ==. just (val saId))
                pure (sum_ (txOut ^. TxOutValue), sum_ (tx ^. TxFee), sum_ (tx ^. TxDeposit))
      pure $ maybe (0, 0, 0) convert (listToMaybe res)

    convert :: (Value (Maybe Micro), Value (Maybe Micro), Value (Maybe Micro)) -> (Ada, Ada, Ada)
    convert (Value mval, Value mfee, Value mdep) =
      (maybe 0 lovelaceToAda mval, maybe 0 lovelaceToAda mfee, maybe 0 lovelaceToAda mdep)

renderBalances :: [Balance] -> IO ()
renderBalances xs = do
    putStrLn "                       stake_address                         |     balance"
    putStrLn "-------------------------------------------------------------+----------------"
    mapM_ renderReward (List.sortOn (Down . balTotal) xs)
    putStrLn "-------------------------------------------------------------+----------------"
    putStr   "                          total                              | "
    Text.putStrLn $ leftPad 14 (renderAda . sum $ map balTotal xs)
    putStrLn ""
  where
    renderReward :: Balance -> IO ()
    renderReward b =
      Text.putStrLn $ mconcat
        [ " "
        , balAddress b
        , separator
        , leftPad 14 (renderAda $ balTotal b)
        ]

