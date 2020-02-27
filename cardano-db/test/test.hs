import           Hedgehog.Main (defaultMain)

import qualified Test.Property.Cardano.Db.Migration
import qualified Test.Property.Cardano.Db.Types
import qualified Test.Property.Upstream

main :: IO ()
main =
  defaultMain
    [ Test.Property.Upstream.tests
    , Test.Property.Cardano.Db.Migration.tests
    , Test.Property.Cardano.Db.Types.tests
    ]
