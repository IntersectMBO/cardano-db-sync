module Explorer.Web
  ( runServer

  -- For testing.
  , CAddress (..)
  , CAddressSummary (..)
  , CCoin (..)
  , CGenesisAddressInfo (..)
  , CHash (..)
  , CTxBrief (..)
  , CTxHash (..)
  , queryAddressSummary
  , queryAllGenesisAddresses
  , queryChainTip
  , runQuery
  ) where

import Explorer.Web.Api.Legacy.AddressSummary (queryAddressSummary)
import Explorer.Web.Api.Legacy.GenesisAddress (queryAllGenesisAddresses)
import Explorer.Web.Api.Legacy.Util (runQuery)
import Explorer.Web.ClientTypes (CAddress (..), CAddressSummary (..), CCoin (..),
        CGenesisAddressInfo (..), CHash (..), CTxBrief (..), CTxHash (..))
import Explorer.Web.Query (queryChainTip)
import Explorer.Web.Server (runServer)
