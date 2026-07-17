{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.DbTool.Report.DisplayTest (
  tests,
) where

import Cardano.DbTool.Report.Display (Align (..), formatReportTime, renderTable)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Report.Display"
    [ testCase "formatReportTime drops sub-second noise" $
        formatReportTime sampleTime @?= "2024-01-15 12:34:56 UTC"
    , testCase "formatReportTime is uniform regardless of fractional seconds" $
        formatReportTime sampleTime @?= formatReportTime wholeSecondTime
    , testCase "renderTable aligns every line to a uniform width" $
        assertBool (show (map Text.length rendered)) (allEqual (map Text.length rendered))
    , testCase "renderTable underline has no stray spaces" $
        assertBool (Text.unpack underline) (not (" " `Text.isInfixOf` underline))
    ]

-- 12:34:56.5 UTC
sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) 45296.5

-- 12:34:56 UTC
wholeSecondTime :: UTCTime
wholeSecondTime = UTCTime (fromGregorian 2024 1 15) 45296

-- A row with an over-long value in the first column must not shift the second.
rendered :: [Text]
rendered =
  renderTable
    [(AlignRight, "epoch"), (AlignLeft, "stake_address")]
    [["1", "stake_test1abc"], ["100", "stake1xy"]]

underline :: Text
underline = rendered !! 1

allEqual :: Eq a => [a] -> Bool
allEqual xs = length (List.nub xs) <= 1
