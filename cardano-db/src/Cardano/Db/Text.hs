module Cardano.Db.Text
  ( textShow
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

textShow :: Show a => a -> Text
textShow = Text.pack . show
