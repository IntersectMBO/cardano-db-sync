{-# LANGUAGE OverloadedStrings #-}
module Cardano.Db.Tool.Report.Display
  ( leftPad
  , separator
  , spaces
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text


leftPad :: Int -> Text -> Text
leftPad width txt = Text.take (width - Text.length txt) spaces <> txt

separator :: Text
separator = " | "

spaces :: Text
spaces = "                                                        "
