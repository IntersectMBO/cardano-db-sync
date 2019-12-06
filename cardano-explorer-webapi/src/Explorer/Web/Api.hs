{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Explorer.Web.Api
  ( ExplorerApi
  , explorerApi
  ) where

import           Data.Proxy (Proxy (..))

import           Explorer.Web.Api.HttpBridge (HttpBridgeApi)
import           Explorer.Web.Api.Legacy (ExplorerApiRecord)

import           Servant.API ((:>), (:<|>))
import           Servant.API.Generic (ToServantApi)

-- | Servant API which provides access to explorer
type ExplorerApi
    = "api" :> ToServantApi ExplorerApiRecord
    -- This one needs to be last in the list because it does not start with
    -- as static string.
    :<|> ToServantApi HttpBridgeApi

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy

