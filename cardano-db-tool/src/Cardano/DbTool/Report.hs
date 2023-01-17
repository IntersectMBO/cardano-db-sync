module Cardano.DbTool.Report (
  module X,
  Report (..),
  runReport,
) where

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
  | ReportBalance [Text]
  | ReportEpochRewards Word64 [Text]
  | ReportLatestRewards [Text]
  | ReportTransactions [Text]

runReport :: Report -> IO ()
runReport report = do
  assertFullySynced
  case report of
    ReportAllRewards sas -> mapM_ reportStakeRewardHistory sas
    ReportBalance sas -> reportBalance sas
    ReportEpochRewards ep sas -> reportEpochStakeRewards ep sas
    ReportLatestRewards sas -> reportLatestStakeRewards sas
    ReportTransactions sas -> reportTransactions sas
