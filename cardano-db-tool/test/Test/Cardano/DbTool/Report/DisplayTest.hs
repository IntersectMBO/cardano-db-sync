{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.DbTool.Report.DisplayTest (
  tests,
) where

import Cardano.DbTool.Report.Display (formatReportTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Report.Display"
    [ testCase "formatReportTime drops sub-second noise" $
        formatReportTime sampleTime @?= "2024-01-15 12:34:56 UTC"
    , testCase "formatReportTime is uniform regardless of fractional seconds" $
        formatReportTime sampleTime @?= formatReportTime wholeSecondTime
    ]

-- 12:34:56.5 UTC
sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) 45296.5

-- 12:34:56 UTC
wholeSecondTime :: UTCTime
wholeSecondTime = UTCTime (fromGregorian 2024 1 15) 45296
