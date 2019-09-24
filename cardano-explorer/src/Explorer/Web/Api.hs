{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Explorer.Web.Api (explorerApi, ExplorerApi) where

import           Explorer.Web.LegacyApi (ExplorerApiRecord)
import           Explorer.Web.API1 (ExplorerApi1Record)
import           Servant.API ((:>), (:<|>))
import           Servant.API.Generic (ToServantApi)
import           Data.Proxy (Proxy (Proxy))

-- | Servant API which provides access to explorer
type ExplorerApi = "api" :> ToServantApi ExplorerApiRecord :<|> "api" :> "v1" :> ToServantApi ExplorerApi1Record

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy

