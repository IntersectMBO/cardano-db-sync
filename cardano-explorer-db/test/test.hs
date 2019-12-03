import           Hedgehog.Main (defaultMain)

import qualified Test.Property.Explorer.DB.Migration
import qualified Test.Property.Explorer.DB.Types
import qualified Test.Property.Upstream

main :: IO ()
main =
  defaultMain
    [ Test.Property.Upstream.tests
    , Test.Property.Explorer.DB.Migration.tests
    , Test.Property.Explorer.DB.Types.tests
    ]
