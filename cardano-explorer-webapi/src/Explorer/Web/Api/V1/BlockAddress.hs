{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Explorer.Web.Api.V1.BlockAddress
  ( V1Utxo (..)
  ) where

import           GHC.Generics (Generic)

import           Data.Aeson.TH (defaultOptions, deriveToJSON)
import           Data.Word (Word16)

import           Explorer.Web.ClientTypes (CTxHash, CAddress, CCoin)

data V1Utxo = V1Utxo
    { cuId       :: !CTxHash
    , cuOutIndex :: !Word16
    , cuAddress  :: !CAddress
    , cuCoins    :: !CCoin
    } deriving (Show, Generic)

deriveToJSON defaultOptions ''V1Utxo
