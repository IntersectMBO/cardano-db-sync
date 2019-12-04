module Explorer.Web
  ( runServer

  -- For testing.
  , CAddress (..)
  , CAddressSummary (..)
  , CCoin (..)
  , queryAddressSummary
  , queryChainTip
  , runQuery
  ) where
import Explorer.Web.Api.Legacy.AddressSummary (queryAddressSummary)
import Explorer.Web.Api.Legacy.Util (runQuery)
import Explorer.Web.ClientTypes (CAddress (..), CAddressSummary (..), CCoin (..))
import Explorer.Web.Query (queryChainTip)
import Explorer.Web.Server (runServer)
