import           Hedgehog.Main (defaultMain)

import qualified Test.Explorer.Core.DB.Migration

main :: IO ()
main =
  defaultMain
    [ Test.Explorer.Core.DB.Migration.tests
    ]
