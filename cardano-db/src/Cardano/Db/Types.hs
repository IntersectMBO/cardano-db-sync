{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Types
  ( Ada (..)
  , lovelaceToAda
  , renderAda
  , scientificToAda
  , word64ToAda
  ) where


import           Data.Aeson.Encoding (unsafeToEncoding)
import           Data.Aeson.Types (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Builder as BS (string8)
import           Data.Fixed (Micro, showFixed)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

import           GHC.Generics (Generic)

newtype Ada = Ada
  { unAda :: Micro
  } deriving (Eq, Num, Ord, Generic)

instance FromJSON Ada where
  parseJSON =
    Aeson.withScientific "Ada" (pure . scientificToAda)

instance ToJSON Ada where
    --toJSON (Ada ada) = Data.Aeson.Types.Number $ fromRational $ toRational ada
    -- `Number` results in it becoming `7.3112484749601107e10` while the old explorer is returning `73112484749.601107`
    toEncoding (Ada ada) =
        unsafeToEncoding $   -- convert ByteString to Aeson's Encoding
        BS.string8 $         -- convert String to ByteString using Latin1 encoding
        showFixed True ada   -- convert Micro to String chopping off trailing zeros

    toJSON = error "Ada.toJSON not supported due to numeric issues. Use toEncoding instead."


instance Show Ada where
    show (Ada ada) = showFixed True ada

lovelaceToAda :: Micro -> Ada
lovelaceToAda ll =
  Ada (ll / 1000000)

renderAda :: Ada -> Text
renderAda (Ada a) = Text.pack (show a)

scientificToAda :: Scientific -> Ada
scientificToAda s =
  word64ToAda $ floor (s * 1000000)

word64ToAda :: Word64 -> Ada
word64ToAda w =
  Ada (fromIntegral w / 1000000)

