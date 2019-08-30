import           Hedgehog.Main (defaultMain)

import qualified Test.Property.Explorer.DB.Migration
import qualified Test.Property.Explorer.DB.Types

main :: IO ()
main =
  defaultMain
    [ Test.Property.Explorer.DB.Migration.tests
    , Test.Property.Explorer.DB.Types.tests
    ]
