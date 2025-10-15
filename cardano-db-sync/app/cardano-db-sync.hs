{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Cardano.Db (MigrationDir (..), PGPassSource (PGPassDefaultEnv, PGPassEnv), gitRev)
import Cardano.DbSync (runDbSyncNode)
import Cardano.DbSync.Config
import Cardano.DbSync.Config.Types
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
import Prelude (error)

---------------------------------------------------------------------------------------------------
-- Main entry point into the app
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  cmd <- Opt.execParser opts
  case cmd of
    CmdVersion -> runVersionCommand
    CmdRun params -> run params
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations

    run :: SyncNodeParams -> IO ()
    run prms = do
      -- read the config file as early as possible
      syncNodeConfigFromFile <- readSyncNodeConfig (enpConfigFile prms)

      -- Validate state-dir/ledger
      let maybeLedgerStateDir = enpMaybeLedgerStateDir prms
          ledgerCfg = sioLedger (dncInsertOptions syncNodeConfigFromFile)

      void $ case (maybeLedgerStateDir, ledgerCfg) of
        -- It is an error to enable ledger and not specify the state
        (Nothing, LedgerEnable) -> error stateDirErrorMsg
        -- Or to ignore ledger and not specify the state
        (Nothing, LedgerIgnore) -> error stateDirErrorMsg
        -- Otherwise, it's OK
        _otherwise -> pure ()

      let prometheusPort = dncPrometheusPort syncNodeConfigFromFile
      withMetricSetters prometheusPort $ \metricsSetters ->
        runDbSyncNode metricsSetters knownMigrationsPlain prms syncNodeConfigFromFile

    stateDirErrorMsg :: [Char]
    stateDirErrorMsg =
      "Error: If not using --state-dir then make sure to have ledger disabled. "
        <> "For more details view https://github.com/IntersectMBO/cardano-db-sync/blob"
        <> "/master/doc/syncing-and-rollbacks.md#ledger-state"

---------------------------------------------------------------------------------------------------
-- Command Line Configurations
---------------------------------------------------------------------------------------------------
opts :: ParserInfo SyncCommand
opts =
  Opt.info
    (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Cardano PostgreSQL sync node."
    )

-- * Flags
pCommandLine :: Parser SyncCommand
pCommandLine =
  asum
    [ pVersionCommand
    , CmdRun <$> pRunDbSyncNode
    ]

pRunDbSyncNode :: Parser SyncNodeParams
pRunDbSyncNode = do
  SyncNodeParams
    <$> pConfigFile
    <*> pSocketPath
    <*> optional pLedgerStateDir
    <*> pMigrationDir
    <*> pPGPassSource
    <*> pEpochDisabled
    <*> pHasCache
    <*> pForceIndexes
    <*> pHasInOut
    <*> optional pRollbackSlotNo

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

pEpochDisabled :: Parser Bool
pEpochDisabled =
  Opt.flag
    False
    True
    ( Opt.long "disable-epoch"
        <> Opt.help "Makes epoch table remain empty"
    )

pForceIndexes :: Parser Bool
pForceIndexes =
  Opt.flag
    False
    True
    ( Opt.long "force-indexes"
        <> Opt.help "Forces the Index creation at the start of db-sync. Normally they're created later."
    )

pHasCache :: Parser Bool
pHasCache =
  Opt.flag
    True
    False
    ( Opt.long "disable-cache"
        <> Opt.help "Disables the db-sync caches. Reduces memory usage but it takes longer to sync."
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

pRollbackSlotNo :: Parser SlotNo
pRollbackSlotNo =
  SlotNo
    <$> Opt.option
      Opt.auto
      ( Opt.long "rollback-to-slot"
          <> Opt.help "Force a rollback to the specified slot (mainly for testing and debugging)."
          <> Opt.metavar "WORD"
      )

pHasInOut :: Parser Bool
pHasInOut =
  Opt.flag
    True
    False
    ( Opt.long "disable-in-out"
        <> Opt.help "Disables the tx_in and tx_out table"
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
  liftIO
    . putTextLn
    $ mconcat
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
