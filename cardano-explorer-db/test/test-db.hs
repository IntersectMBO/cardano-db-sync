{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty (defaultMain, testGroup)

import qualified Test.IO.Explorer.DB.Insert
import qualified Test.IO.Explorer.DB.Migration
import qualified Test.IO.Explorer.DB.TotalSupply

main :: IO ()
main =
  defaultMain $
    testGroup "Database"
      [ Test.IO.Explorer.DB.Migration.tests
      , Test.IO.Explorer.DB.Insert.tests
      , Test.IO.Explorer.DB.TotalSupply.tests
      ]
