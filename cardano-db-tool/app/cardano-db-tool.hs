{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db
import Cardano.DbSync.Config.Types hiding (CmdVersion, LogFileDir)
import Cardano.DbTool
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Applicative (optional)
import Control.Monad (unless, void, when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Data.Word (Word64)
import Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt
import Paths_cardano_db_tool (version)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
  Opt.customExecParser p opts >>= runCommand
  where
    opts :: ParserInfo Command
    opts =
      Opt.info
        (Opt.helper <*> pCommand)
        ( Opt.fullDesc
            <> Opt.header "cardano-db-tool - Manage the Cardano PostgreSQL Database"
        )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = CmdCreateMigration !MigrationDir
  | CmdReport !Report
  | CmdRollback !SlotNo
  | CmdRunMigrations !MigrationDir !Bool !Bool !(Maybe LogFileDir)
  | CmdTxOutMigration
  | CmdUtxoSetAtBlock !Word64
  | CmdPrepareSnapshot !PrepareSnapshotArgs
  | CmdValidateDb
  | CmdValidateAddressBalance !LedgerValidationParams
  | CmdVersion

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    CmdCreateMigration mdir -> runCreateMigration mdir
    CmdReport report -> runReport report
    CmdRollback slotNo -> runRollback slotNo
    CmdRunMigrations mdir forceIndexes mockFix mldir -> do
      pgConfig <- runOrThrowIODb (readPGPass PGPassDefaultEnv)
      unofficial <- snd <$> runMigrations pgConfig False mdir mldir Initial
      unless (null unofficial) $
        putStrLn $
          "Unofficial migration scripts found: " ++ show unofficial
      when forceIndexes $
        void $
          runMigrations pgConfig False mdir mldir Indexes
      when mockFix $
        void $
          runMigrations pgConfig False mdir mldir Fix
    CmdTxOutMigration -> do
      runWithConnectionNoLogging PGPassDefaultEnv $ migrateTxOut Nothing
    CmdUtxoSetAtBlock blkid -> utxoSetAtSlot blkid
    CmdPrepareSnapshot pargs -> runPrepareSnapshot pargs
    CmdValidateDb -> runDbValidation
    CmdValidateAddressBalance params -> runLedgerValidation params
    CmdVersion -> runVersionCommand

runCreateMigration :: MigrationDir -> IO ()
runCreateMigration mdir = do
  mfp <- createMigration PGPassDefaultEnv mdir
  case mfp of
    Nothing -> putStrLn "No migration needed."
    Just fp -> putStrLn $ "New migration '" ++ fp ++ "' created."

runRollback :: SlotNo -> IO ()
runRollback slotNo =
  print =<< runDbNoLoggingEnv (deleteBlocksSlotNoNoTrace slotNo)

runVersionCommand :: IO ()
runVersionCommand = do
  Text.putStrLn $
    mconcat
      [ "cardano-db-tool "
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

-- -----------------------------------------------------------------------------

pCommand :: Parser Command
pCommand =
  Opt.subparser $
    mconcat
      [ Opt.command "create-migration" $
          Opt.info
            pCreateMigration
            (Opt.progDesc "Create a database migration (only really used by devs).")
      , Opt.command "report" $
          Opt.info
            (CmdReport <$> pReport)
            (Opt.progDesc "Run a report using data from the database.")
      , Opt.command "rollback" $
          Opt.info
            pRollback
            (Opt.progDesc "Rollback the database to the block with the provided slot number.")
      , Opt.command "run-migrations" $
          Opt.info
            pRunMigrations
            ( Opt.progDesc $
                mconcat
                  [ "Run the database migrations (which are idempotent)."
                  , " By default this only runs the initial migrations that db-sync"
                  , " runs when it starts. You can force and mock other migrations"
                  , " but this is not advised in the general case."
                  ]
            )
      , Opt.command "tx_out-migration" $
          Opt.info
            (pure CmdTxOutMigration)
            ( Opt.progDesc $
                mconcat
                  [ "Runs the tx_out migration, which adds a new field"
                  ]
            )
      , Opt.command "utxo-set" $
          Opt.info
            pUtxoSetAtBlock
            (Opt.progDesc "Get UTxO set at specified BlockNo.")
      , Opt.command "prepare-snapshot" $
          Opt.info
            pPrepareSnapshot
            (Opt.progDesc "Prepare to create a snapshot pair")
      , Opt.command "validate" $
          Opt.info
            (pure CmdValidateDb)
            (Opt.progDesc "Run validation checks against the database.")
      , Opt.command "validate-address-balance" $
          Opt.info
            (CmdValidateAddressBalance <$> pValidateLedgerParams)
            (Opt.progDesc "Run validation checks against the database and the ledger Utxo set.")
      , Opt.command "version" $
          Opt.info
            (pure CmdVersion)
            (Opt.progDesc "Show the program version.")
      ]
  where
    pCreateMigration :: Parser Command
    pCreateMigration =
      CmdCreateMigration <$> pMigrationDir

    pRunMigrations :: Parser Command
    pRunMigrations =
      CmdRunMigrations
        <$> pMigrationDir
        <*> pForceIndexes
        <*> pMockFix
        <*> optional pLogFileDir

    pRollback :: Parser Command
    pRollback =
      CmdRollback . SlotNo . read
        <$> Opt.strOption
          ( Opt.long "slot"
              <> Opt.help "The slot number to roll back to."
          )

    pUtxoSetAtBlock :: Parser Command
    pUtxoSetAtBlock =
      CmdUtxoSetAtBlock . read
        <$> Opt.strOption
          ( Opt.long "slot-no"
              <> Opt.help "The SlotNo."
          )

    pPrepareSnapshot :: Parser Command
    pPrepareSnapshot =
      CmdPrepareSnapshot <$> pPrepareSnapshotArgs

pPrepareSnapshotArgs :: Parser PrepareSnapshotArgs
pPrepareSnapshotArgs = PrepareSnapshotArgs <$> pLedgerStateDir

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir
    <$> Opt.strOption
      ( Opt.long "mdir"
          <> Opt.help "The directory containing the migrations."
          <> Opt.completer (Opt.bashCompleter "directory")
      )

pLogFileDir :: Parser LogFileDir
pLogFileDir =
  LogFileDir
    <$> Opt.strOption
      ( Opt.long "ldir"
          <> Opt.help "The directory to write the log to."
          <> Opt.completer (Opt.bashCompleter "directory")
      )

pForceIndexes :: Parser Bool
pForceIndexes =
  Opt.flag
    False
    True
    ( Opt.long "force-indexes"
        <> Opt.help
          ( mconcat
              [ "Forces the creation of all indexes."
              , " Normally they are created by db-sync when it reaches"
              , " the tip of the chain."
              ]
          )
    )

pMockFix :: Parser Bool
pMockFix =
  Opt.flag
    False
    True
    ( Opt.long "mock-fix"
        <> Opt.help
          ( mconcat
              [ "Mocks the execution of the fix chainsync procedure"
              , " By using this flag, db-sync later won't run the fixing procedures."
              ]
          )
    )

pValidateLedgerParams :: Parser LedgerValidationParams
pValidateLedgerParams =
  LedgerValidationParams
    <$> pConfigFile
    <*> pLedgerStateDir
    <*> pAddress

pAddress :: Parser Text
pAddress =
  Opt.strOption $
    mconcat
      [ Opt.long "address"
      , Opt.help "Cardano address"
      , Opt.metavar "ADDRESS"
      ]

pLedgerStateDir :: Parser LedgerStateDir
pLedgerStateDir =
  LedgerStateDir
    <$> Opt.strOption
      ( Opt.long "state-dir"
          <> Opt.help "The directory for persisting ledger state."
          <> Opt.completer (Opt.bashCompleter "directory")
          <> Opt.metavar "FILEPATH"
      )

pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile
    <$> Opt.strOption
      ( Opt.long "config"
          <> Opt.help "Path to the db-sync node config file"
          <> Opt.completer (Opt.bashCompleter "file")
          <> Opt.metavar "FILEPATH"
      )

pReport :: Parser Report
pReport =
  Opt.subparser $
    mconcat
      [ Opt.command "balance" $
          Opt.info
            (ReportBalance <$> pStakeAddress)
            (Opt.progDesc "Report the balance of a given stake address (or addresses)")
      , Opt.command "rewards" $
          Opt.info
            pReward
            (Opt.progDesc "Rewards report")
      , Opt.command "transactions" $
          Opt.info
            (ReportTransactions <$> pStakeAddress)
            (Opt.progDesc "Report the transaction histiory for a given stake address (or addresses)")
      ]
  where
    pReward :: Parser Report
    pReward =
      Opt.subparser $
        mconcat
          [ Opt.command "epoch" $
              Opt.info
                (ReportEpochRewards <$> pEpochNo <*> pStakeAddress)
                (Opt.progDesc "Report the rewards fof the gievn epoch and stake address (or addresses)")
          , Opt.command "latest" $
              Opt.info
                (ReportLatestRewards <$> pStakeAddress)
                (Opt.progDesc "Report the latest epoch rewards for a given stake address (or addresses)")
          , Opt.command "history" $
              Opt.info
                (ReportAllRewards <$> pStakeAddress)
                (Opt.progDesc "Report the reward histiory for a given stake address (or addresses)")
          ]

    pStakeAddress :: Parser [Text]
    pStakeAddress =
      Text.split (== ',')
        . Text.pack
        <$> Opt.strOption
          ( Opt.long "stake-address"
              <> Opt.help "Either a single stake address or a comma separated list."
          )

    pEpochNo :: Parser Word64
    pEpochNo =
      Opt.option Opt.auto $
        mconcat
          [ Opt.long "epoch"
          , Opt.metavar "WORD"
          , Opt.help "The epoch number."
          ]
