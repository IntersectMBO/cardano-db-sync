{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.Balance (
  reportBalance,
) where

import Cardano.Db
import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text

reportBalance :: TxOutVariantType -> [Text] -> IO ()
reportBalance txOutVariantType saddr = do
  xs <- catMaybes <$> DB.runDbStandaloneSilent (mapM (queryStakeAddressBalance txOutVariantType) saddr)
  renderBalances xs

-- -------------------------------------------------------------------------------------------------

data Balance = Balance
  { balAddressId :: !DB.StakeAddressId
  , balAddress :: !Text
  , balInputs :: !Ada
  , balOutputs :: !Ada
  , balFees :: !Ada
  , balDeposit :: !Ada
  , balRewards :: !Ada
  , balWithdrawals :: !Ada
  , balTotal :: !Ada
  }

queryStakeAddressBalance :: TxOutVariantType -> Text -> DB.DbM (Maybe Balance)
queryStakeAddressBalance txOutVariantType address = do
  mSaId <- DB.queryStakeAddressId address
  case mSaId of
    Nothing -> pure Nothing
    Just saId -> Just <$> queryBalance saId
  where
    queryBalance :: DB.StakeAddressId -> DB.DbM Balance
    queryBalance saId = do
      inputs <- queryInputs saId
      (outputs, fees, deposit) <- queryOutputs saId
      currentEpoch <- DB.queryLatestEpochNoFromBlock
      rewards <- DB.queryRewardsSum saId currentEpoch
      withdrawals <- DB.queryWithdrawalsSum saId
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

    queryInputs :: DB.StakeAddressId -> DB.DbM Ada
    queryInputs saId = case txOutVariantType of
      TxOutVariantCore -> DB.queryInputsSumCore saId
      TxOutVariantAddress -> DB.queryInputsSumAddress saId

    queryOutputs :: DB.StakeAddressId -> DB.DbM (Ada, Ada, Ada)
    queryOutputs saId = case txOutVariantType of
      TxOutVariantCore -> DB.queryOutputsCore saId
      TxOutVariantAddress -> DB.queryOutputsAddress saId

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
