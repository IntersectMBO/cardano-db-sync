{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.DB.Types
  ( Ada (..)
  , lovelaceToAda
  , renderAda
  , word64ToAda
  ) where


import           Data.Fixed (Micro)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

newtype Ada = Ada
  { unAda :: Micro
  } deriving (Eq, Ord, Show)

lovelaceToAda :: Micro -> Ada
lovelaceToAda ll =
  Ada (ll / 1000000)

word64ToAda :: Word64 -> Ada
word64ToAda w =
  Ada (fromIntegral w / 1000000)

renderAda :: Ada -> Text
renderAda (Ada a) = Text.pack (show a)
