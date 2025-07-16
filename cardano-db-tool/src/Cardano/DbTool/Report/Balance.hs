{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Report.Balance (
  reportBalance,
) where

import Cardano.Db
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbTool.Report.Display
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Fixed (Micro)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Database.Esqueleto.Experimental (
  SqlBackend,
  Value (..),
  from,
  innerJoin,
  just,
  on,
  select,
  sum_,
  table,
  val,
  where_,
  (&&.),
  (<=.),
  (==.),
  (^.),
  type (:&) ((:&)),
 )

{- HLINT ignore "Redundant ^." -}
{- HLINT ignore "Fuse on/on" -}

reportBalance :: TxOutVariantType -> [Text] -> IO ()
reportBalance txOutTableType saddr = do
  xs <- catMaybes <$> runDbNoLoggingEnv (mapM (queryStakeAddressBalance txOutTableType) saddr)
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

queryStakeAddressBalance :: MonadIO m => TxOutVariantType -> Text -> ReaderT SqlBackend m (Maybe Balance)
queryStakeAddressBalance txOutTableType address = do
  mSaId <- queryStakeAddressId
  case mSaId of
    Nothing -> pure Nothing
    Just saId -> Just <$> queryBalance saId
  where
    queryStakeAddressId :: MonadIO m => ReaderT SqlBackend m (Maybe StakeAddressId)
    queryStakeAddressId = do
      res <- select $ do
        saddr <- from $ table @StakeAddress
        where_ (saddr ^. StakeAddressView ==. val address)
        pure (saddr ^. StakeAddressId)
      pure $ fmap unValue (listToMaybe res)

    queryBalance :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Balance
    queryBalance saId = do
      inputs <- queryInputs saId
      (outputs, fees, deposit) <- queryOutputs saId
      rewards <- queryRewardsSum saId
      withdrawals <- queryWithdrawals saId
      pure $
        Balance
          { balAddressId = saId
          , balAddress = address
          , balInputs = inputs
          , balOutputs = outputs
          , balFees = fees
          , balDeposit = deposit
          , balRewards = rewards
          , balWithdrawals = withdrawals
          , balTotal = inputs - outputs + rewards - withdrawals
          }

    queryInputs :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Ada
    queryInputs saId = case txOutTableType of
      TxOutVariantCore -> do
        res <- select $ do
          txo <- from $ table @VC.TxOut
          where_ (txo ^. VC.TxOutStakeAddressId ==. just (val saId))
          pure (sum_ (txo ^. VC.TxOutValue))
        pure $ unValueSumAda (listToMaybe res)
      TxOutVariantAddress -> do
        res <- select $ do
          (txo :& addr) <-
            from $
              table @VA.TxOut
                `innerJoin` table @VA.Address
                  `on` (\(txo :& addr) -> txo ^. VA.TxOutAddressId ==. addr ^. VA.AddressId)
          where_ (addr ^. VA.AddressStakeAddressId ==. just (val saId))
          pure (sum_ (txo ^. VA.TxOutValue))
        pure $ unValueSumAda (listToMaybe res)

    queryRewardsSum :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Ada
    queryRewardsSum saId = do
      currentEpoch <- queryLatestEpochNo
      res <- select $ do
        rwd <- from $ table @Reward
        where_ (rwd ^. RewardAddrId ==. val saId)
        where_ (rwd ^. RewardSpendableEpoch <=. val currentEpoch)
        pure (sum_ (rwd ^. RewardAmount))
      pure $ unValueSumAda (listToMaybe res)

    queryWithdrawals :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m Ada
    queryWithdrawals saId = do
      res <- select $ do
        wdrl <- from $ table @Withdrawal
        where_ (wdrl ^. WithdrawalAddrId ==. val saId)
        pure (sum_ (wdrl ^. WithdrawalAmount))
      pure $ unValueSumAda (listToMaybe res)

    queryOutputs :: MonadIO m => StakeAddressId -> ReaderT SqlBackend m (Ada, Ada, Ada)
    queryOutputs saId = case txOutTableType of
      TxOutVariantCore -> do
        res <- select $ do
          (txOut :& tx :& _txIn) <-
            from $
              table @VC.TxOut
                `innerJoin` table @Tx
                  `on` (\(txOut :& tx) -> txOut ^. VC.TxOutTxId ==. tx ^. TxId)
                `innerJoin` table @TxIn
                  `on` (\(txOut :& tx :& txIn) -> txIn ^. TxInTxOutId ==. tx ^. TxId &&. txIn ^. TxInTxOutIndex ==. txOut ^. VC.TxOutIndex)
          where_ (txOut ^. VC.TxOutStakeAddressId ==. just (val saId))
          pure (sum_ (txOut ^. VC.TxOutValue), sum_ (tx ^. TxFee), sum_ (tx ^. TxDeposit))
        pure $ maybe (0, 0, 0) convert (listToMaybe res)
      TxOutVariantAddress -> do
        res <- select $ do
          (txOut :& addr :& tx :& _txIn) <-
            from $
              table @VA.TxOut
                `innerJoin` table @VA.Address
                  `on` (\(txOut :& addr) -> txOut ^. VA.TxOutAddressId ==. addr ^. VA.AddressId)
                `innerJoin` table @Tx
                  `on` (\(txOut :& _addr :& tx) -> txOut ^. VA.TxOutTxId ==. tx ^. TxId)
                `innerJoin` table @TxIn
                  `on` (\(txOut :& _addr :& tx :& txIn) -> txIn ^. TxInTxOutId ==. tx ^. TxId &&. txIn ^. TxInTxOutIndex ==. txOut ^. VA.TxOutIndex)
          where_ (addr ^. VA.AddressStakeAddressId ==. just (val saId))
          pure (sum_ (txOut ^. VA.TxOutValue), sum_ (tx ^. TxFee), sum_ (tx ^. TxDeposit))
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
  putStr "                          total                              | "
  Text.putStrLn $ leftPad 14 (renderAda . sum $ map balTotal xs)
  putStrLn ""
  where
    renderReward :: Balance -> IO ()
    renderReward b =
      Text.putStrLn $
        mconcat
          [ " "
          , balAddress b
          , separator
          , leftPad 14 (renderAda $ balTotal b)
          ]
