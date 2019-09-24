module Main (main) where

import           Test.Tasty (defaultMain, testGroup)

import qualified Test.IO.Explorer.Web.Query

main :: IO ()
main = do
  defaultMain $ testGroup "Database"
    [ Test.IO.Explorer.Web.Query.tests
    ]
