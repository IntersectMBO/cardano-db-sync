{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.Balance (
  reportBalance,
) where

import Cardano.Db
import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Asset
import Cardano.DbTool.Report.Display
import Control.Monad (unless)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text

-- | Report the balance of each stake address. Unless 'assetFilter' is 'NoAssets', the multi-asset
-- balances are also reported.
reportBalance :: TxOutVariantType -> AssetFilter -> [Text] -> IO ()
reportBalance txOutVariantType assetFilter saddr = do
  xs <- catMaybes <$> DB.runDbStandaloneSilent (mapM (queryStakeAddressBalance txOutVariantType assetFilter) saddr)
  renderBalances (includesAssets assetFilter) xs

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
  , balAssets :: ![AssetQuantity]
  }

queryStakeAddressBalance :: TxOutVariantType -> AssetFilter -> Text -> DB.DbM (Maybe Balance)
queryStakeAddressBalance txOutVariantType assetFilter address = do
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
      assets <-
        if includesAssets assetFilter
          then filterAssets assetFilter <$> queryAssetBalances txOutVariantType saId
          else pure []
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
          , balAssets = assets
          }

    queryInputs :: DB.StakeAddressId -> DB.DbM Ada
    queryInputs saId = case txOutVariantType of
      TxOutVariantCore -> DB.queryInputsSumCore saId
      TxOutVariantAddress -> DB.queryInputsSumAddress saId

    queryOutputs :: DB.StakeAddressId -> DB.DbM (Ada, Ada, Ada)
    queryOutputs saId = case txOutVariantType of
      TxOutVariantCore -> DB.queryOutputsCore saId
      TxOutVariantAddress -> DB.queryOutputsAddress saId

-- | Render the balances, followed by the totals. If 'includeAssets' is set, an 'asset' column is
-- added and each ADA balance is followed by a row for each multi-asset balance.
renderBalances :: Bool -> [Balance] -> IO ()
renderBalances includeAssets xs = do
  mapM_ Text.putStrLn (withTotalDivider (renderTable cols (bodyRows ++ totalRows)))
  putStrLn ""
  unless (all hasKnownDecimals allAssets) $
    Text.putStrLn $
      rawQuantityNote <> "\n"
  where
    sorted = List.sortOn (Down . balTotal) xs

    allAssets :: [AssetQuantity]
    allAssets = concatMap balAssets xs

    cols :: [(Align, Text)]
    cols =
      [ (AlignLeft, "stake_address")
      , (AlignRight, "balance")
      ]
        ++ [(AlignLeft, "asset") | includeAssets]

    bodyRows :: [[Text]]
    bodyRows = concatMap (\b -> balanceRows (balAddress b) (balTotal b) (balAssets b)) sorted

    -- The total ADA balance, and the total of each asset across all the stake addresses.
    totalRows :: [[Text]]
    totalRows = balanceRows "total" (sum $ map balTotal xs) (totalAssets allAssets)

    -- A row for the ADA balance, followed by a row for each asset balance, with the known
    -- assets first and then by amount (biggest first).
    balanceRows :: Text -> Ada -> [AssetQuantity] -> [[Text]]
    balanceRows label ada assets =
      ([label, renderAda ada] ++ ["ADA" | includeAssets])
        : map (\aq -> ["", renderAssetQuantity aq, renderAssetName aq]) (sortAssetsByAmount assets)

    totalAssets :: [AssetQuantity] -> [AssetQuantity]
    totalAssets assets =
      [ AssetQuantity fingerprint name quantity
      | ((fingerprint, name), quantity) <-
          Map.toList $
            Map.fromListWith (+) [((aqFingerprint aq, aqName aq), aqQuantity aq) | aq <- assets]
      ]

    -- Set the total rows off with a divider, reusing the header underline.
    withTotalDivider :: [Text] -> [Text]
    withTotalDivider ls = case ls of
      (header : divider : body) ->
        header : divider : take (length bodyRows) body ++ [divider] ++ drop (length bodyRows) body
      _ -> ls
