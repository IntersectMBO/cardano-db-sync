{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Explorer.Web.Api
  ( ExplorerApi
  , explorerApi
  ) where

import           Data.Proxy (Proxy (..))

import           Explorer.Web.LegacyApi (ExplorerApiRecord)
import           Explorer.Web.API1 (ExplorerApi1Record)
import           Explorer.Web.HttpBridge (HttpBridgeApi)

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

