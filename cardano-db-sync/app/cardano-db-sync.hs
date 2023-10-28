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
import GHC.Base (error)
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
      let maybeLedgerStateDir = enpMaybeLedgerStateDir params
      case (maybeLedgerStateDir, enpShouldUseLedger params) of
        (Just _, True) -> run params
        (Nothing, False) -> run params
        (Just _, False) -> run params
        (Nothing, True) -> error stateDirErrorMsg
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations

    stateDirErrorMsg :: [Char]
    stateDirErrorMsg =
      "Error: If not using --state-dir then make sure to have --disable-ledger. "
        <> "For more details view https://github.com/input-output-hk/cardano-db-sync/blob/master/doc/syncing-and-rollbacks.md#ledger-state"

    run :: SyncNodeParams -> IO ()
    run prms = do
      prometheusPort <- dncPrometheusPort <$> readSyncNodeConfig (enpConfigFile prms)
      withMetricSetters prometheusPort $ \metricsSetters ->
        runDbSyncNode metricsSetters knownMigrationsPlain prms

-- -------------------------------------------------------------------------------------------------

opts :: ParserInfo SyncCommand
opts =
  Opt.info
    (depricated <*> pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Cardano PostgreSQL sync node."
    )

pCommandLine :: Parser SyncCommand
pCommandLine =
  asum
    [ pVersionCommand
    , CmdRun <$> pRunDbSyncNode
    ]

depricated :: Parser (a -> a)
depricated =
  Opt.abortOption (Opt.InfoMsg "Error: disable-offline-data has been depricated, please use disable-offchain-pool-data instead") $
    Opt.long "disable-offline-data"
      <> Opt.help "disable-offline-data is depricated"
      <> Opt.hidden

pRunDbSyncNode :: Parser SyncNodeParams
pRunDbSyncNode =
  SyncNodeParams
    <$> pConfigFile
    <*> pSocketPath
    <*> optional pLedgerStateDir
    <*> pMigrationDir
    <*> pPGPassSource
    <*> pEpochDisabled
    <*> pHasCache
    <*> pUseLedger
    <*> pSkipFix
    <*> pOnlyFix
    <*> pForceIndexes
    <*> pHasMultiAssets
    <*> pHasMetadata
    <*> pHasPlutusExtra
    <*> pHasOffChainPoolData
    <*> pForceTxIn
    <*> pTurboMode
    <*> pFullMode
    <*> pMigrateConsumed
    <*> pPruneTxOut
    <*> bootstrap
    <*> pure 500
    <*> pure 10000
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

pEpochDisabled :: Parser Bool
pEpochDisabled =
  Opt.flag
    False
    True
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
        <> Opt.help
          "Runs only the db-sync fix procedure for the wrong datum, redeemer_data and plutus script bytes and exits. \
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

pUseLedger :: Parser Bool
pUseLedger =
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

pHasMultiAssets :: Parser Bool
pHasMultiAssets =
  Opt.flag
    True
    False
    ( Opt.long "disable-multiassets"
        <> Opt.help "Disables the multi assets tables and entries."
    )

pHasMetadata :: Parser Bool
pHasMetadata =
  Opt.flag
    True
    False
    ( Opt.long "disable-metadata"
        <> Opt.help "Disables the tx_metadata table."
    )

pHasPlutusExtra :: Parser Bool
pHasPlutusExtra =
  Opt.flag
    True
    False
    ( Opt.long "disable-plutus-extra"
        <> Opt.help "Disables most tables and entries related to plutus and scripts."
    )

pHasOffChainPoolData :: Parser Bool
pHasOffChainPoolData =
  Opt.flag
    True
    False
    ( Opt.long "disable-offchain-pool-data"
        <> Opt.help "Disables fetching pool offchain metadata."
    )

pForceTxIn :: Parser Bool
pForceTxIn =
  Opt.flag
    False
    True
    ( Opt.long "force-tx-in"
        <> Opt.help "Force populating the tx_in table even if --consumed-tx-out is enabled"
    )

pTurboMode :: Parser Bool
pTurboMode =
  Opt.flag
    False
    True
    ( Opt.long "turbo"
        <> Opt.help "Enables turbo mode, which make db-sync running much faster with limited functionality."
    )

pFullMode :: Parser Bool
pFullMode =
  Opt.flag
    False
    True
    ( Opt.long "full"
        <> Opt.help "Makes db-sync run with all possible functionalities."
    )

pMigrateConsumed :: Parser Bool
pMigrateConsumed =
  Opt.flag
    False
    True
    ( Opt.long "consumed-tx-out"
        <> Opt.help
          "Runs the tx_out migration, which adds a new field.If this is set once,\
          \ then it must be always be set on following executions of db-sync, unless prune-tx-out\
          \ is used instead."
    )

pPruneTxOut :: Parser Bool
pPruneTxOut =
  Opt.flag
    False
    True
    ( Opt.long "prune-tx-out"
        <> Opt.help
          "Prunes the consumed tx_out periodically. This assumes \
          \ consumed-tx-out is also set, even if it's not. If this is set once,\
          \ then it must be always set on following executions of db-sync."
    )

bootstrap :: Parser Bool
bootstrap =
  Opt.flag
    False
    True
    ( Opt.long "bootstrap-tx-out"
        <> Opt.help
          "This syncs without populating the tx_out table. It eventually gets populated\
          \ by migrating the ledger state. It assumes the --consumed-tx-out and\
          \ not having the --disable-ledger-state"
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
