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
  , RewardSource (..)
  , SyncState (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  , deltaCoinToDbInt65
  , integerToDbInt65
  , lovelaceToAda
  , renderAda
  , scientificToAda
  , readDbInt65
  , showDbInt65
  , readRewardSource
  , readScriptPurpose
  , readScriptType
  , readSyncState
  , renderScriptPurpose
  , renderScriptType
  , renderSyncState
  , rewardTypeToSource
  , showRewardSource
  , word64ToAda
  ) where

import           Cardano.Ledger.Coin (DeltaCoin (..))

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

import qualified Shelley.Spec.Ledger.Rewards as Shelley


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
  deriving (Eq, Generic, Ord)
  deriving (Read, Show) via (Quiet DbLovelace)

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbWord64
  = DbWord64 { unDbWord64 :: Word64 }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet DbWord64)

data RewardSource
  = RwdMember
  | RwdLeader
  | RwdReserves
  | RwdTreasury
  deriving (Bounded, Enum, Eq, Ord, Show)

data SyncState
  = SyncLagging         -- Local tip is lagging the global chain tip.
  | SyncFollowing       -- Local tip is following global chain tip.
  deriving (Eq, Show)

data ScriptPurpose
  = Spend
  | Mint
  | Cert
  | Rewrd
  deriving (Eq, Generic, Show)

data ScriptType
  = Timelock
  | Plutus
  deriving (Eq, Generic, Show)

deltaCoinToDbInt65 :: DeltaCoin -> DbInt65
deltaCoinToDbInt65 (DeltaCoin dc) =
  if dc < 0
    then NegInt65 (fromIntegral $ abs dc)
    else PosInt65 (fromIntegral dc)

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
    NegInt65 0 -> "0"
    NegInt65 w -> '-' : show w

readRewardSource :: Text -> RewardSource
readRewardSource str =
  case str of
    "member" -> RwdMember
    "leader" -> RwdLeader
    "reserves" -> RwdReserves
    "treasury" -> RwdTreasury
    -- This should never happen. On the Postgres side we defined an ENUM with
    -- only the two values as above.
    _other -> error $ "readRewardSource: Unknown RewardSource " ++ Text.unpack str

readSyncState :: String -> SyncState
readSyncState str =
  case str of
    "lagging" -> SyncLagging
    "following" -> SyncFollowing
    -- This should never happen. On the Postgres side we defined an ENUM with
    -- only the two values as above.
    _other -> error $ "readSyncState: Unknown SyncState " ++ str

renderSyncState :: SyncState -> Text
renderSyncState ss =
  case ss of
    SyncFollowing -> "following"
    SyncLagging -> "lagging"

rewardTypeToSource :: Shelley.RewardType -> RewardSource
rewardTypeToSource rt =
  case rt of
    Shelley.LeaderReward -> RwdLeader
    Shelley.MemberReward -> RwdMember

renderScriptPurpose :: ScriptPurpose -> Text
renderScriptPurpose ss =
  case ss of
    Spend -> "spend"
    Mint -> "mint"
    Cert -> "cert"
    Rewrd -> "reward"

readScriptPurpose :: String -> ScriptPurpose
readScriptPurpose str =
  case str of
    "spend" -> Spend
    "mint" -> Mint
    "cert" -> Cert
    "reward" -> Rewrd
    _other -> error $ "readScriptPurpose: Unknown ScriptPurpose " ++ str

showRewardSource :: RewardSource -> Text
showRewardSource rs =
  case rs of
    RwdMember -> "member"
    RwdLeader -> "leader"
    RwdReserves -> "reserves"
    RwdTreasury -> "treasury"

renderScriptType :: ScriptType -> Text
renderScriptType st =
  case st of
    Timelock -> "timelock"
    Plutus -> "plutus"

readScriptType :: String -> ScriptType
readScriptType str =
  case str of
    "timelock" -> Timelock
    "plutus" -> Plutus
    _other -> error $ "readScriptType: Unknown ScriptType " ++ str

word64ToAda :: Word64 -> Ada
word64ToAda w =
  Ada (fromIntegral w / 1000000)
