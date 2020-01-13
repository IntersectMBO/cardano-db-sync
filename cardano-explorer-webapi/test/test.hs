import           Hedgehog.Main (defaultMain)

import qualified Test.Explorer.Web.Property.Types

main :: IO ()
main =
  defaultMain
    [ Test.Explorer.Web.Property.Types.tests
    ]
