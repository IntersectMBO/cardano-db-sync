{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Explorer.Web.Api
  ( ExplorerApi
  , explorerApi
  ) where

import           Data.Proxy (Proxy (..))

import           Explorer.Web.Api.HttpBridge (HttpBridgeApi)
import           Explorer.Web.Api.Legacy (ExplorerApiRecord)
import           Explorer.Web.Api.V1 (ExplorerApi1Record)

import           Servant.API ((:>), (:<|>))
import           Servant.API.Generic (ToServantApi)

-- | Servant API which provides access to explorer
type ExplorerApi
    = "api" :> ToServantApi ExplorerApiRecord
    :<|> "api" :> "v1" :> ToServantApi ExplorerApi1Record
    -- This one needs to be last in the list because it does not start with
    -- as static string.
    :<|> ToServantApi HttpBridgeApi

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy

