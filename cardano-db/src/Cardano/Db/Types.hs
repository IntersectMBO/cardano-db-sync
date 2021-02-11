{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Types
  ( Ada (..)
  , DbLovelace (..)
  , DbInt65 (..)
  , DbWord64 (..)
  , integerToDbInt65
  , lovelaceToAda
  , renderAda
  , scientificToAda
  , readDbInt65
  , showDbInt65
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

import           Quiet (Quiet (..))

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

-- This is horrible. Need a 'Word64' with an extra sign bit.
data DbInt65
  = PosInt65 !Word64
  | NegInt65 !Word64
  deriving (Eq, Generic, Show)

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbLovelace
  = DbLovelace { unDbLovelace :: Word64 }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet DbLovelace)

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbWord64
  = DbWord64 { unDbWord64 :: Word64 }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet DbWord64)

integerToDbInt65 :: Integer -> DbInt65
integerToDbInt65 i =
  if i >= 0
    then PosInt65 (fromIntegral i)
    else NegInt65 (fromIntegral $ negate i)

lovelaceToAda :: Micro -> Ada
lovelaceToAda ll =
  Ada (ll / 1000000)

renderAda :: Ada -> Text
renderAda (Ada a) = Text.pack (show a)

scientificToAda :: Scientific -> Ada
scientificToAda s =
  word64ToAda $ floor (s * 1000000)

readDbInt65 :: String -> DbInt65
readDbInt65 str =
  case str of
    ('-':rest) -> NegInt65 $ read rest
    _other -> PosInt65 $ read str

showDbInt65 :: DbInt65 -> String
showDbInt65 i65 =
  case i65 of
    PosInt65 w -> show w
    NegInt65 0 ->  "0"
    NegInt65 w ->  '-' : show w

word64ToAda :: Word64 -> Ada
word64ToAda w =
  Ada (fromIntegral w / 1000000)
