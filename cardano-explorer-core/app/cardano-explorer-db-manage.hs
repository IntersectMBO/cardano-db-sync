
import           Data.Monoid ((<>))

import           Explorer.Core

import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt


main :: IO ()
main = do
    Opt.customExecParser p opts >>= runCommand
  where
    opts :: ParserInfo Command
    opts = Opt.info (Opt.helper <*> pVersion <*> pCommand)
      ( Opt.fullDesc
      <> Opt.header "cardano-explorer-db-manage - Mange the Cardano Explorer database"
      )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = CreateMigration PGPassFile MigrationDir
  | RunMigrations PGPassFile MigrationDir LogFileDir

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    CreateMigration pgpass mdir -> doCreateMigration pgpass mdir
    RunMigrations pgpass mdir ldir -> runMigrations False pgpass mdir ldir

doCreateMigration :: PGPassFile -> MigrationDir -> IO ()
doCreateMigration pgpassfile mdir = do
  mfp <- createMigration pgpassfile mdir
  case mfp of
    Nothing -> putStrLn "No migration needed."
    Just fp -> putStrLn $ "New migration '" ++ fp ++ "' created."

-- -----------------------------------------------------------------------------

pVersion :: Parser (a -> a)
pVersion =
  Opt.infoOption "cardano-explorer-db-manage version 0.1.0.0"
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
    <> Opt.command "run-migrations"
        ( Opt.info pRunMigrations
          $ Opt.progDesc "Run the database migrations (which are idempotent)."
          )
    )
  where
    pCreateMigration :: Parser Command
    pCreateMigration =
      CreateMigration <$> pPGPassFile <*> pMigrationDir
    pRunMigrations :: Parser Command
    pRunMigrations =
      RunMigrations <$> pPGPassFile <*> pMigrationDir <*> pLogFileDir

pPGPassFile :: Parser PGPassFile
pPGPassFile =
  PGPassFile <$> Opt.strOption
    (  Opt.long "pgpassfile"
    <> Opt.help "The PGPASSFILE location. The file should contain a single line: 'host:port:dbname:user:password'"
    )

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir <$> Opt.strOption
    (  Opt.long "mdir"
    <> Opt.help "The directory containing the migrations."
    )

pLogFileDir :: Parser LogFileDir
pLogFileDir =
  LogFileDir <$> Opt.strOption
    (  Opt.long "ldir"
    <> Opt.help "The directory to write the log to."
    )
