import           Cardano.Prelude (Text)

import           Prelude

import           Control.Monad (when, (>=>))
import           Data.Maybe (isNothing)

import           System.Directory (getCurrentDirectory)
import           System.Environment (lookupEnv, setEnv)
import           System.FilePath ((</>))

import           MigrationValidations (KnownMigration (..), knownMigrations)

import           Cardano.Mock.ChainSync.Server

import           Criterion.Main

import qualified Cardano.Db.Bench as Bench

main :: IO ()
main = do
    -- If the env is not set, set it to default.
    mPgPassFile <- lookupEnv "PGPASSFILE"
    when (isNothing mPgPassFile) $ do
      currentDir <- getCurrentDirectory
      setEnv "PGPASSFILE" (currentDir </> "bench/benchfiles/pgpass-bench")
    withIOManager $
        benchmarks >=> defaultMain
  where
--    config = defaultConfig
--      { resamples = 1
--      , reportFile = Just "report.html"
--      , csvFile = Just "report.csv"
--      , jsonFile = Just "reprt.json"
--      , junitFile = Just "report.junit"
--      }

benchmarks :: IOManager -> IO [Benchmark]
benchmarks iom = do
    pure $
      [ bgroup
        "cardano-chain"
          [ Bench.benchmark iom knownMigrationsPlain
          ]
      ]
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations
