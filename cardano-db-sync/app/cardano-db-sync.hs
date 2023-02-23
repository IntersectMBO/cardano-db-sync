{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Cardano.Db (MigrationDir (..), PGPassSource (PGPassDefaultEnv, PGPassEnv), gitRev)
import Cardano.DbSync (runDbSyncNode)
import Cardano.DbSync.Config
import Cardano.DbSync.Metrics (withMetricSetters)
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import Data.String (String)
import qualified Data.Text as Text
import Data.Version (showVersion)
import MigrationValidations (KnownMigration (..), knownMigrations)
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt
import Paths_cardano_db_sync (version)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
  cmd <- Opt.execParser opts
  case cmd of
    CmdVersion -> runVersionCommand
    CmdRun params -> do
      prometheusPort <- dncPrometheusPort <$> readSyncNodeConfig (enpConfigFile params)

      withMetricSetters prometheusPort $ \metricsSetters ->
        runDbSyncNode metricsSetters knownMigrationsPlain params
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations

-- -------------------------------------------------------------------------------------------------

opts :: ParserInfo SyncCommand
opts =
  Opt.info
    (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Cardano PostgreSQL sync node."
    )

pCommandLine :: Parser SyncCommand
pCommandLine =
  asum
    [ pVersionCommand
    , CmdRun <$> pRunDbSyncNode
    ]

pRunDbSyncNode :: Parser SyncNodeParams
pRunDbSyncNode =
  SyncNodeParams
    <$> pConfigFile
    <*> pSocketPath
    <*> pLedgerStateDir
    <*> pMigrationDir
    <*> pPGPassSource
    <*> pExtended
    <*> pHasCache
    <*> pHasLedger
    <*> pSkipFix
    <*> pOnlyFix
    <*> pForceIndexes
    <*> optional pSlotNo

pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile
    <$> Opt.strOption
      ( Opt.long "config"
          <> Opt.help "Path to the db-sync node config file"
          <> Opt.completer (Opt.bashCompleter "file")
          <> Opt.metavar "FILEPATH"
      )

pLedgerStateDir :: Parser LedgerStateDir
pLedgerStateDir =
  LedgerStateDir
    <$> Opt.strOption
      ( Opt.long "state-dir"
          <> Opt.help "The directory for persisting ledger state."
          <> Opt.completer (Opt.bashCompleter "directory")
          <> Opt.metavar "FILEPATH"
      )

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir
    <$> Opt.strOption
      ( Opt.long "schema-dir"
          <> Opt.help "The directory containing the migrations."
          <> Opt.completer (Opt.bashCompleter "directory")
          <> Opt.metavar "FILEPATH"
      )

pPGPassSource :: Parser PGPassSource
pPGPassSource =
  Opt.option
    (PGPassEnv <$> Opt.str)
    ( Opt.long "pg-pass-env"
        <> Opt.help "Alternative env variable to use, defaults to PGPASSFILE env variable."
        <> Opt.value PGPassDefaultEnv
        <> Opt.metavar "ENV"
    )

pExtended :: Parser Bool
pExtended =
  Opt.flag
    True
    False
    ( Opt.long "disable-epoch"
        <> Opt.help "Makes epoch table remain empty"
    )

pSkipFix :: Parser Bool
pSkipFix =
  Opt.flag
    False
    True
    ( Opt.long "skip-fix"
        <> Opt.help "Disables the db-sync fix procedure for the wrong datum and redeemer_data bytes."
    )

pForceIndexes :: Parser Bool
pForceIndexes =
  Opt.flag
    False
    True
    ( Opt.long "force-indexes"
        <> Opt.help "Forces the Index creation at the start of db-sync. Normally they're created later."
    )

pOnlyFix :: Parser Bool
pOnlyFix =
  Opt.flag
    False
    True
    ( Opt.long "fix-only"
        <> Opt.help "Runs only the db-sync fix procedure for the wrong datum, redeemer_data and plutus script bytes and exits. \
            \This doesn't run any migrations. This can also be ran on previous schema, ie 13.0 13.1 to fix the issues without \
            \bumping the schema version minor number."
    )

pHasCache :: Parser Bool
pHasCache =
  Opt.flag
    True
    False
    ( Opt.long "disable-cache"
        <> Opt.help "Disables the db-sync caches. Reduces memory usage but it takes longer to sync."
    )

pHasLedger :: Parser Bool
pHasLedger =
  Opt.flag
    True
    False
    ( Opt.long "disable-ledger"
        <> Opt.help "Disables the leger state. Drastically reduces memory usage and it syncs faster, but some data are missing."
    )

pSocketPath :: Parser SocketPath
pSocketPath =
  SocketPath
    <$> Opt.strOption
      ( Opt.long "socket-path"
          <> Opt.help "Path to a cardano-node socket"
          <> Opt.completer (Opt.bashCompleter "file")
          <> Opt.metavar "FILEPATH"
      )

pSlotNo :: Parser SlotNo
pSlotNo =
  SlotNo
    <$> Opt.option
      Opt.auto
      ( Opt.long "rollback-to-slot"
          <> Opt.help "Force a rollback to the specified slot (mainly for testing and debugging)."
          <> Opt.metavar "WORD"
      )

pVersionCommand :: Parser SyncCommand
pVersionCommand =
  asum
    [ Opt.subparser
        ( mconcat
            [command' "version" "Show the program version" (pure CmdVersion)]
        )
    , Opt.flag'
        CmdVersion
        ( Opt.long "version"
            <> Opt.help "Show the program version"
            <> Opt.hidden
        )
    ]

command' :: String -> String -> Parser a -> Opt.Mod Opt.CommandFields a
command' c descr p =
  Opt.command c $
    Opt.info (p <**> Opt.helper) $
      mconcat [Opt.progDesc descr]

runVersionCommand :: IO ()
runVersionCommand = do
  liftIO . putTextLn $
    mconcat
      [ "cardano-db-sync "
      , renderVersion version
      , " - "
      , Text.pack os
      , "-"
      , Text.pack arch
      , " - "
      , Text.pack compilerName
      , "-"
      , renderVersion compilerVersion
      , "\ngit revision "
      , gitRev
      ]
  where
    renderVersion = Text.pack . showVersion
