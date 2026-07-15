import Test.Tasty (defaultMain, testGroup)
import qualified Test.Cardano.DbTool.Report.DisplayTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "cardano-db-tool"
      [ Test.Cardano.DbTool.Report.DisplayTest.tests
      ]
