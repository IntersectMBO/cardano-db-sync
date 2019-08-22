import           Hedgehog.Main (defaultMain)

import qualified Test.Property.Explorer.DB.Migration

main :: IO ()
main =
  defaultMain
    [ Test.Property.Explorer.DB.Migration.tests
    ]
