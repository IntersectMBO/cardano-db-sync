{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.Report.Display (
  Align (..),
  formatReportTime,
  leftPad,
  renderTable,
  rightPad,
  separator,
  shortenBech32,
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

    -- Trailing whitespace (from padding a left aligned last column) is stripped.
    headerLine = Text.stripEnd $ Text.intercalate separator (zipWith3 pad aligns widths headers)
    underline = Text.intercalate "-+-" (map (`Text.replicate` "-") widths)
    renderRow = Text.stripEnd . Text.intercalate separator . zipWith3 pad aligns widths

-- | Shorten a Bech32 encoded value to its human readable prefix, "..." and its last 8
-- characters, eg "pool1z5uq...d7yws0xt" becomes "pool...d7yws0xt".
shortenBech32 :: Text -> Text
shortenBech32 bech32
  | Text.length bech32 <= Text.length shortened = bech32
  | otherwise = shortened
  where
    -- The human readable prefix is everything before the last '1'.
    prefix = Text.dropEnd 1 . fst $ Text.breakOnEnd "1" bech32
    shortened = prefix <> "..." <> Text.takeEnd 8 bech32

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
