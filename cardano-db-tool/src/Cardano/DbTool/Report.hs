module Cardano.DbTool.Report (
  module X,
  AssetFilter (..),
  Report (..),
  runReport,
) where

import Cardano.Db (TxOutVariantType)
import Cardano.DbTool.Report.Asset (AssetFilter (..))
import Cardano.DbTool.Report.Balance (reportBalance)
import Cardano.DbTool.Report.StakeReward (
  reportEpochStakeRewards,
  reportLatestStakeRewards,
  reportStakeRewardHistory,
 )
import Cardano.DbTool.Report.Synced as X
import Cardano.DbTool.Report.Transactions (reportTransactions)
import Data.Text (Text)
import Data.Word (Word64)

data Report
  = ReportAllRewards [Text]
  | ReportBalance !AssetFilter [Text]
  | ReportEpochRewards Word64 [Text]
  | ReportLatestRewards [Text]
  | ReportTransactions !AssetFilter [Text]

runReport :: Report -> TxOutVariantType -> IO ()
runReport report txOutTableType = do
  assertFullySynced
  case report of
    ReportAllRewards sas -> mapM_ reportStakeRewardHistory sas
    ReportBalance assetFilter sas -> reportBalance txOutTableType assetFilter sas
    ReportEpochRewards ep sas -> reportEpochStakeRewards ep sas
    ReportLatestRewards sas -> reportLatestStakeRewards sas
    ReportTransactions assetFilter sas -> reportTransactions txOutTableType assetFilter sas
