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
        _ -> pure ()

      let prometheusPort = dncPrometheusPort syncNodeConfigFromFile
      withMetricSetters prometheusPort $ \metricsSetters ->
        runDbSyncNode metricsSetters knownMigrationsPlain prms syncNodeConfigFromFile

    stateDirErrorMsg :: [Char]
    stateDirErrorMsg =
      "Error: If not using --state-dir then make sure to have ledger disabled. "
        <> "For more details view https://github.com/IntersectMBO/cardano-db-sync/blob"
        <> "/master/doc/syncing-and-rollbacks.md#ledger-state"

-- -------------------------------------------------------------------------------------------------

opts :: ParserInfo SyncCommand
opts =
  Opt.info
    (pDeprecated <*> pCommandLine <**> Opt.helper)
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

pDeprecated :: Parser (a -> a)
pDeprecated =
  pDisableOfflineData
    <*> pHasLedger
    <*> pShouldUseLedger
    <*> pKeepTxMetadata
    <*> pHasShelley
    <*> pHasMultiAssets
    <*> pHasMetadata
    <*> pHasPlutusExtra
    <*> pHasGov
    <*> pHasOffChainPoolData
    <*> pForceTxIn
    <*> pDisableAllMode
    <*> pFullMode
    <*> pOnlyUTxO
    <*> pOnlyGov
    <*> pMigrateConsumed
    <*> pPruneTxOut
    <*> pBootstrap

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
    <*> pSkipFix
    <*> pOnlyFix
    <*> pForceIndexes
    <*> pHasInOut
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

-- * Deprecated flags
pDisableOfflineData :: Parser (a -> a)
pDisableOfflineData =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-offline-data has been deprecated, please use disable-offchain-pool-data instead")
    ( Opt.long "disable-offline-data"
        <> Opt.help "disable-offline-data is deprecated"
        <> Opt.hidden
    )

pHasLedger :: Parser (a -> a)
pHasLedger =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-ledger has been deprecated, please configure ledger in db-sync-config.json instead")
    ( Opt.long "disable-ledger"
        <> Opt.help "disable-ledger is deprecated"
        <> Opt.hidden
    )

pShouldUseLedger :: Parser (a -> a)
pShouldUseLedger =
  Opt.abortOption
    (Opt.InfoMsg "Error: dont-use-ledger has been deprecated, please configure ledger in db-sync-config.json instead")
    ( Opt.long "dont-use-ledger"
        <> Opt.help "dont-use-ledger is deprecated"
        <> Opt.hidden
    )

pKeepTxMetadata :: Parser (a -> a)
pKeepTxMetadata =
  Opt.abortOption
    (Opt.InfoMsg "Error: keep-tx-metadata has been deprecated, please configure ledger in db-sync-config.json instead")
    ( Opt.long "keep-tx-metadata"
        <> Opt.help "keep-tx-metadata is deprecated"
        <> Opt.hidden
    )

pHasShelley :: Parser (a -> a)
pHasShelley =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-shelley has been deprecated, please configure shelley in db-sync-config.json instead")
    ( Opt.long "disable-shelley"
        <> Opt.help "disable-shelley is deprecated"
        <> Opt.hidden
    )

pHasMultiAssets :: Parser (a -> a)
pHasMultiAssets =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-multiassets has been deprecated, please configure multi-assets in db-sync-config.json instead")
    ( Opt.long "disable-multiassets"
        <> Opt.help "disable-multiassets is deprecated"
        <> Opt.hidden
    )

pHasMetadata :: Parser (a -> a)
pHasMetadata =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-metadata has been deprecated, please configure metadata in db-sync-config.json instead")
    ( Opt.long "disable-metadata"
        <> Opt.help "disable-metadata is deprecated"
        <> Opt.hidden
    )

pHasPlutusExtra :: Parser (a -> a)
pHasPlutusExtra =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-plutus-extra has been deprecated, please configure plutus in db-sync-config.json instead")
    ( Opt.long "disable-metadata"
        <> Opt.help "disable-metadata is deprecated"
        <> Opt.hidden
    )

pHasGov :: Parser (a -> a)
pHasGov =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-gov has been deprecated, please configure governance in db-sync-config.json instead")
    ( Opt.long "disable-gov"
        <> Opt.help "disable-gov is deprecated"
        <> Opt.hidden
    )

pHasOffChainPoolData :: Parser (a -> a)
pHasOffChainPoolData =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-offchain-pool-data has been deprecated, please configure offchain pool data in db-sync-config.json instead")
    ( Opt.long "disable-offchain-pool-data"
        <> Opt.help "disable-gov is deprecated"
        <> Opt.hidden
    )

pForceTxIn :: Parser (a -> a)
pForceTxIn =
  Opt.abortOption
    (Opt.InfoMsg "Error: force-tx-in has been deprecated, please configure tx-out in db-sync-config.json instead")
    ( Opt.long "force-tx-in"
        <> Opt.help "force-tx-in is deprecated"
        <> Opt.hidden
    )

pDisableAllMode :: Parser (a -> a)
pDisableAllMode =
  Opt.abortOption
    (Opt.InfoMsg "Error: disable-all has been deprecated, please configure db-sync-config.json instead")
    ( Opt.long "disable-all"
        <> Opt.help "disable-all is deprecated"
        <> Opt.hidden
    )

pFullMode :: Parser (a -> a)
pFullMode =
  Opt.abortOption
    (Opt.InfoMsg "Error: full has been deprecated, please configure db-sync-config.json instead")
    ( Opt.long "full"
        <> Opt.help "full is deprecated"
        <> Opt.hidden
    )

pOnlyUTxO :: Parser (a -> a)
pOnlyUTxO =
  Opt.abortOption
    (Opt.InfoMsg "Error: only-utxo has been deprecated, please configure db-sync-config.json instead")
    ( Opt.long "only-utxo"
        <> Opt.help "only-utxo is deprecated"
        <> Opt.hidden
    )

pOnlyGov :: Parser (a -> a)
pOnlyGov =
  Opt.abortOption
    (Opt.InfoMsg "Error: only-gov has been deprecated, please configure db-sync-config.json instead")
    ( Opt.long "only-gov"
        <> Opt.help "only-gov is deprecated"
        <> Opt.hidden
    )

pMigrateConsumed :: Parser (a -> a)
pMigrateConsumed =
  Opt.abortOption
    (Opt.InfoMsg "Error: consumed-tx-out has been deprecated, please configure tx-out in db-sync-config.json instead")
    ( Opt.long "consumed-tx-out"
        <> Opt.help "consumed-tx-out is deprecated"
        <> Opt.hidden
    )

pPruneTxOut :: Parser (a -> a)
pPruneTxOut =
  Opt.abortOption
    (Opt.InfoMsg "Error: prune-tx-out has been deprecated, please configure tx-out in db-sync-config.json instead")
    ( Opt.long "prune-tx-out"
        <> Opt.help "prune-tx-out is deprecated"
        <> Opt.hidden
    )

pBootstrap :: Parser (a -> a)
pBootstrap =
  Opt.abortOption
    (Opt.InfoMsg "Error: bootstrap-tx-out has been deprecated, please configure tx-out in db-sync-config.json instead")
    ( Opt.long "bootstrap-tx-out"
        <> Opt.help "bootstrap-tx-out is deprecated"
        <> Opt.hidden
    )

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
