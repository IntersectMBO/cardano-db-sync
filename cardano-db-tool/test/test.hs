import qualified Test.Cardano.DbTool.Report.DisplayTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "cardano-db-tool"
      [ Test.Cardano.DbTool.Report.DisplayTest.tests
      ]
