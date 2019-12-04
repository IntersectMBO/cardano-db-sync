{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Explorer.Web.Api.HttpBridge
  ( HttpBridgeApi (..)
  ) where

import           GHC.Generics (Generic)

import           Explorer.Web.ClientTypes (CAddress, CNetwork, CAddressBalanceError)

import           Servant.API ((:>), Capture, Get, JSON, Summary)
import           Servant.API.Generic ((:-))

data HttpBridgeApi route = HttpBridgeApi
  { _addressBalance :: route
        :- Summary "Get current balance of provided address."
        :> Capture "network" CNetwork
        :> "utxos"
        :> Capture "address" CAddress
        :> Get '[JSON] CAddressBalanceError
  } deriving Generic
