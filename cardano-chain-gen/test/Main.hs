{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Cardano.Db (PGConfig (..), PGPassSource (..), readPGPassDefault)
import Cardano.Mock.ChainSync.Server
import Cardano.Prelude (Text)
import Control.Exception (throwIO)
import Control.Monad (when, (>=>))
import Data.Maybe (isNothing)
import MigrationValidations (KnownMigration (..), knownMigrations)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import qualified Test.Cardano.Db.Mock.Property.Property as Property
import qualified Test.Cardano.Db.Mock.Unit.Alonzo as Alonzo
import qualified Test.Cardano.Db.Mock.Unit.Babbage as Babbage
import qualified Test.Cardano.Db.Mock.Unit.Conway as Conway
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Prelude

main :: IO ()
main = do
  -- If the env is not set, set it to default.
  mPgPassFile <- lookupEnv "PGPASSFILE"
  when (isNothing mPgPassFile) $ do
    currentDir <- getCurrentDirectory
    print currentDir
    setEnv "PGPASSFILE" (currentDir </> "test/testfiles/pgpass-testing")
  withIOManager $
    tests >=> defaultMain

tests :: IOManager -> IO TestTree
tests iom = do
  -- Tests share a single Postgres instance and DB, so they must run serially.
  -- tasty 1.5+ defaults to parallel execution; override that here.
  pgCfg@PGConfig {..} <- either throwIO pure =<< readPGPassDefault

  -- TODO[sgillespie]: create a PG config for each era
  let
    pgCfg1 = pgCfg {pgcDbname = pgcDbname <> "_1"}
    pgCfg2 = pgCfg {pgcDbname = pgcDbname <> "_2"}
    pgCfg3 = pgCfg {pgcDbname = pgcDbname <> "_3"}
    pgCfg4 = pgCfg {pgcDbname = pgcDbname <> "_4"}

  pure $
    testGroup
      "all"
      [ -- Tests share a single Postgres instance and DB, so they must run serially.
        -- tasty 1.5+ defaults to parallel execution; override that here.
        testGroup
          "cardano-chain-gen"
          [ Conway.unitTests iom knownMigrationsPlain (PGPassCached pgCfg1)
          , Babbage.unitTests iom knownMigrationsPlain (PGPassCached pgCfg2)
          , Alonzo.unitTests iom knownMigrationsPlain (PGPassCached pgCfg3)
          , testProperty "QSM" $ Property.prop_empty_blocks (PGPassCached pgCfg4) iom knownMigrationsPlain
          ]
      ]
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations
