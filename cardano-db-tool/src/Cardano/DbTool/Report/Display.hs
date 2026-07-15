{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.Display (
  Align (..),
  formatReportTime,
  leftPad,
  renderTable,
  rightPad,
  separator,
) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ICU as ICU
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data Align = AlignLeft | AlignRight

-- Render an aligned table: each column is sized to the widest of its header and
-- its cells, so no column shifts when a value (e.g. a bech32 address) is longer
-- than the header. Returns the header line, the underline, and one line per row.
renderTable :: [(Align, Text)] -> [[Text]] -> [Text]
renderTable cols rows =
  headerLine : underline : map renderRow rows
  where
    aligns = map fst cols
    headers = map snd cols
    cellWidths
      | null rows = map (const 0) cols
      | otherwise = map (maximum . map textDisplayLen) (List.transpose rows)
    widths = zipWith max (map textDisplayLen headers) cellWidths

    pad :: Align -> Int -> Text -> Text
    pad AlignLeft = rightPad
    pad AlignRight = leftPad

    headerLine = Text.intercalate separator (zipWith3 pad aligns widths headers)
    underline = Text.intercalate "-+-" (map (`Text.replicate` "-") widths)
    renderRow = Text.intercalate separator . zipWith3 pad aligns widths

formatReportTime :: UTCTime -> Text
formatReportTime = Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

leftPad :: Int -> Text -> Text
leftPad width txt = Text.replicate (width - textDisplayLen txt) " " <> txt

rightPad :: Int -> Text -> Text
rightPad width txt = txt <> Text.replicate (width - textDisplayLen txt) " "

separator :: Text
separator = " | "

-- Calculates the screen character count a `Text` object will use when printed.
textDisplayLen :: Text -> Int
textDisplayLen = List.length . ICU.breaks (ICU.breakCharacter ICU.Root)
