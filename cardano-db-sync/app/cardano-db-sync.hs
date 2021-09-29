{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude

import           Cardano.DbSync (defDbSyncNodePlugin, runDbSyncNode)
import           Cardano.DbSync.Cli
import           Cardano.DbSync.Metrics (withMetricSetters)
import           Cardano.Sync.Config (readSyncNodeConfig)
import           Cardano.Sync.Config.Types

import qualified Data.Text as Text
import           Data.Version (showVersion)

import           MigrationValidations (KnownMigration (..), knownMigrations)
import qualified Options.Applicative as Opt
import           Paths_cardano_db_sync (version)

import           System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
  cmd <- Opt.execParser opts
  case cmd of
    CmdVersion -> runVersionCommand
    CmdRun params -> do
        prometheusPort <- dncPrometheusPort <$> readSyncNodeConfig (enpConfigFile params)

        withMetricSetters prometheusPort $ \metricsSetters ->
            runDbSyncNode metricsSetters defDbSyncNodePlugin knownMigrationsPlain params
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations

-- -------------------------------------------------------------------------------------------------

runVersionCommand :: IO ()
runVersionCommand = do
    liftIO . putTextLn $ mconcat
                [ "cardano-db-sync ", renderVersion version
                , " - ", Text.pack os, "-", Text.pack arch
                , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
                , "\ngit revision ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion
