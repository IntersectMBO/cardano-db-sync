{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.Display (
  formatReportTime,
  leftPad,
  rightPad,
  separator,
  spaces,
) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ICU as ICU
import Data.Time.Clock (UTCTime)

-- Crude placeholder rendering, replaced by a fixed-width format in the fix commit.
formatReportTime :: UTCTime -> Text
formatReportTime = Text.pack . show

leftPad :: Int -> Text -> Text
leftPad width txt = Text.take (width - textDisplayLen txt) spaces <> txt

rightPad :: Int -> Text -> Text
rightPad width txt = txt <> Text.take (width - textDisplayLen txt) spaces

separator :: Text
separator = " | "

spaces :: Text
spaces = "                                                        "

-- Calculates the screen character count a `Text` object will use when printed.
textDisplayLen :: Text -> Int
textDisplayLen = List.length . ICU.breaks (ICU.breakCharacter ICU.Root)
