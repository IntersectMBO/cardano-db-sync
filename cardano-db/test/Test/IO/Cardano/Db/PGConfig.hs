{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.IO.Cardano.Db.PGConfig (
  tests,
) where

import Cardano.Db (PGConfig (..), PGPassSource (..), readPGPass, runOrThrowIODb)
import Control.Monad (unless)
import System.Directory (getCurrentDirectory)
import System.Environment
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "PGConfig"
    [ testCase "Reading PGPass file" pgFileReadTest
    ]

-- Custom Eq instance that ignores the user field.
instance Eq PGConfig where
  (==) a b =
    pgcHost a == pgcHost b
      && pgcPort a == pgcPort b
      && pgcDbname a == pgcDbname b
      && pgcPassword a == pgcPassword b

pgFileReadTest :: IO ()
pgFileReadTest = do
  let expected = PGConfig "/var/run/postgresql" "5432" "cexplorer" "" "*"

  currentDir <- getCurrentDirectory
  setEnv "OTHER_PG_PASS_ENV_VARIABLE" (currentDir </> "../config/pgpass-mainnet")

  let pg = PGPassEnv "OTHER_PG_PASS_ENV_VARIABLE"

  result <- runOrThrowIODb $ readPGPass pg
  unless (result == expected) $
    error $
      mconcat
        [ "PGConfig mismatch. Expected "
        , show expected
        , " but got "
        , show result
        , "."
        ]
