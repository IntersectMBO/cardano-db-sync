
import           Cardano.Db.App
import           Cardano.Db

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Applicative (optional)

import           Data.Monoid ((<>))
import           Data.Word (Word64)

import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt


main :: IO ()
main = do
    Opt.customExecParser p opts >>= runCommand
  where
    opts :: ParserInfo Command
    opts = Opt.info (Opt.helper <*> pVersion <*> pCommand)
      ( Opt.fullDesc
      <> Opt.header "cardano-db-tool - Manage the Cardano PostgreSQL Database"
      )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = CreateMigration MigrationDir
  | Rollback SlotNo
  | RunMigrations MigrationDir (Maybe LogFileDir)
  | UtxoSetAtBlock Word64
  | Validate

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    CreateMigration mdir -> doCreateMigration mdir
    Rollback slotNo -> runRollback slotNo
    RunMigrations mdir mldir -> runMigrations id False mdir mldir
    UtxoSetAtBlock blkid -> utxoSetAtSlot blkid
    Validate -> runValidation

doCreateMigration :: MigrationDir -> IO ()
doCreateMigration mdir = do
  mfp <- createMigration mdir
  case mfp of
    Nothing -> putStrLn "No migration needed."
    Just fp -> putStrLn $ "New migration '" ++ fp ++ "' created."

runRollback :: SlotNo -> IO ()
runRollback slotNo =
  print =<< runDbNoLogging (deleteCascadeSlotNo slotNo)

-- -----------------------------------------------------------------------------

pVersion :: Parser (a -> a)
pVersion =
  Opt.infoOption "cardano-db-tool version 0.1.0.0"
    (  Opt.long "version"
    <> Opt.short 'v'
    <> Opt.help "Print the version and exit"
    )

pCommand :: Parser Command
pCommand =
  Opt.subparser
    ( Opt.command "create-migration"
        ( Opt.info pCreateMigration
          $ Opt.progDesc "Create a database migration (only really used by devs)."
          )
    <> Opt.command "rollback"
        ( Opt.info pRollback
          $ Opt.progDesc "Rollback the database to the block with the provided slot number."
          )
    <> Opt.command "run-migrations"
        ( Opt.info pRunMigrations
          $ Opt.progDesc "Run the database migrations (which are idempotent)."
          )
    <> Opt.command "utxo-set"
        ( Opt.info pUtxoSetAtBlock
          $ Opt.progDesc "Get UTxO set at specified BlockNo."
          )
    <> Opt.command "validate"
        ( Opt.info (pure Validate)
          $ Opt.progDesc "Run validation checks against the database."
          )
    )
  where
    pCreateMigration :: Parser Command
    pCreateMigration =
      CreateMigration <$> pMigrationDir

    pRunMigrations :: Parser Command
    pRunMigrations =
      RunMigrations <$> pMigrationDir <*> optional pLogFileDir

    pRollback :: Parser Command
    pRollback =
      Rollback . SlotNo . read <$> Opt.strOption
        (  Opt.long "slot"
        <> Opt.help "The slot number to roll back to."
        )

    pUtxoSetAtBlock :: Parser Command
    pUtxoSetAtBlock =
      UtxoSetAtBlock . read <$> Opt.strOption
        (  Opt.long "slot-no"
        <> Opt.help "The SlotNo."
        )

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir <$> Opt.strOption
    (  Opt.long "mdir"
    <> Opt.help "The directory containing the migrations."
    <> Opt.completer (Opt.bashCompleter "directory")
    )

pLogFileDir :: Parser LogFileDir
pLogFileDir =
  LogFileDir <$> Opt.strOption
    (  Opt.long "ldir"
    <> Opt.help "The directory to write the log to."
    <> Opt.completer (Opt.bashCompleter "directory")
    )
