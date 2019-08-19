import           Hedgehog.Main (defaultMain)

import qualified Test.Explorer.DB.Migration

main :: IO ()
main =
  defaultMain
    [ Test.Explorer.DB.Migration.tests
    ]
