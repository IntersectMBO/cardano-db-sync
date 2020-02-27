{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty (defaultMain, testGroup)

import qualified Test.IO.Cardano.Db.Insert
import qualified Test.IO.Cardano.Db.Migration
import qualified Test.IO.Cardano.Db.TotalSupply
import qualified Test.IO.Cardano.Db.Rollback

main :: IO ()
main =
  defaultMain $
    testGroup "Database"
      [ Test.IO.Cardano.Db.Migration.tests
      , Test.IO.Cardano.Db.Insert.tests
      , Test.IO.Cardano.Db.TotalSupply.tests
      , Test.IO.Cardano.Db.Rollback.tests
      ]
