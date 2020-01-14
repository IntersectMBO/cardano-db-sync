module Explorer.Web
  ( runServer

  -- For testing.
  , CAddress (..)
  , CAddressSummary (..)
  , CCoin (..)
  , CGenesisAddressInfo (..)
  , queryAddressSummary
  , queryAllGenesisAddresses
  , queryChainTip
  , runQuery
  ) where

import Explorer.Web.Api.Legacy.AddressSummary (queryAddressSummary)
import Explorer.Web.Api.Legacy.GenesisAddress (queryAllGenesisAddresses)
import Explorer.Web.Api.Legacy.Util (runQuery)
import Explorer.Web.ClientTypes (CAddress (..), CAddressSummary (..), CCoin (..),
        CGenesisAddressInfo (..))
import Explorer.Web.Query (queryChainTip)
import Explorer.Web.Server (runServer)
