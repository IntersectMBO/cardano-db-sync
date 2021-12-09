{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Migration
  ( MigrationDir (..)
  , LogFileDir (..)
  , applyMigration
  , createMigration
  , getMigrationScripts
  , runMigrations
  , recreateDB
  , dropTables

  , MigrationValidate (..)
  , MigrationValidateError (..)
  , validateMigrations
  , hashMigrations
  , renderMigrationValidateError
  ) where

import           Control.Exception (SomeException, handle)
import           Control.Monad.Extra
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString.Char8 as BS
import           Data.Conduit.Binary (sinkHandle)
import           Data.Conduit.Process (sourceCmdWithConsumer)
import           Data.Either (partitionEithers)
import           Data.List ((\\))
import qualified Data.List as List
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

import           Database.Persist.Sql (SqlBackend, SqlPersistT, Single (..), entityVal, getMigration,
                   rawExecute, rawSql, selectFirst)

import           Cardano.Crypto.Hash (Blake2b_256, ByteString, Hash, hashToStringAsHex, hashWith)
import           Cardano.Db.Migration.Haskell
import           Cardano.Db.Migration.Version
import           Cardano.Db.PGConfig
import           Cardano.Db.Run
import           Cardano.Db.Schema
import           Cardano.Db.Text

import           System.Directory (listDirectory)
import           System.Exit (ExitCode (..), exitFailure)
import           System.FilePath ((</>))
import           System.IO (Handle, IOMode (AppendMode), hFlush, hPrint, hPutStrLn, stdout,
                   withFile)

newtype MigrationDir
  = MigrationDir FilePath

newtype LogFileDir
  = LogFileDir FilePath

data MigrationValidate = MigrationValidate
  { mvHash :: Text
  , mvFilepath :: Text
  } deriving (Eq, Show)

data MigrationValidateError = UnknownMigrationsFound
  { missingMigrations :: [MigrationValidate]
  , extraMigrations :: [MigrationValidate]
  } deriving (Eq, Show)

-- | Run the migrations in the provided 'MigrationDir' and write date stamped log file
-- to 'LogFileDir'.
runMigrations :: PGConfig -> Bool -> MigrationDir -> Maybe LogFileDir -> IO ()
runMigrations pgconfig quiet migrationDir mLogfiledir = do
    scripts <- getMigrationScripts migrationDir
    case mLogfiledir of
      Nothing -> do
        putStrLn "Running:"
        forM_ scripts $ applyMigration migrationDir quiet pgconfig Nothing stdout
        putStrLn "Success!"
      Just logfiledir -> do
        logFilename <- genLogFilename logfiledir
        withFile logFilename AppendMode $ \logHandle -> do
          unless quiet $ putStrLn "Running:"
          forM_ scripts $ applyMigration migrationDir quiet pgconfig (Just logFilename) logHandle
          unless quiet $ putStrLn "Success!"
  where
    genLogFilename :: LogFileDir -> IO FilePath
    genLogFilename (LogFileDir logdir) =
      (logdir </>)
        . formatTime defaultTimeLocale ("migrate-" ++ iso8601DateFormat (Just "%H%M%S") ++ ".log")
        <$> getCurrentTime

-- Build hash for each file found in a directory.
validateMigrations :: MigrationDir -> [(Text, Text)] -> ExceptT MigrationValidateError IO ()
validateMigrations migrationDir knownMigrations = do
    let knownMigrations' = uncurry MigrationValidate <$> knownMigrations
    scripts <- liftIO $ hashMigrations migrationDir
    when (scripts /= knownMigrations') $
      throwE $ UnknownMigrationsFound
          { missingMigrations = knownMigrations' \\ scripts -- Migrations missing at runtime that were present at compilation time
          , extraMigrations = scripts \\ knownMigrations'   -- Migrations found at runtime that were missing at compilation time
          }

applyMigration :: MigrationDir -> Bool -> PGConfig -> Maybe FilePath -> Handle -> (MigrationVersion, FilePath) -> IO ()
applyMigration (MigrationDir location) quiet pgconfig mLogFilename logHandle (version, script) = do
    -- This assumes that the credentials for 'psql' are already sorted out.
    -- One way to achive this is via a 'PGPASSFILE' environment variable
    -- as per the PostgreSQL documentation.
    let command =
          List.unwords
            [ "psql"
            , BS.unpack (pgcDbname pgconfig)
            , "--no-password"
            , "--quiet"
            , "--username=" <> BS.unpack (pgcUser pgconfig)
            , "--host=" <> BS.unpack (pgcHost pgconfig)
            , "--port=" <> BS.unpack (pgcPort pgconfig)
            , "--no-psqlrc"                     -- Ignore the ~/.psqlrc file.
            , "--single-transaction"            -- Run the file as a transaction.
            , "--set ON_ERROR_STOP=on"          -- Exit with non-zero on error.
            , "--file='" ++ location </> script ++ "'"
            , "2>&1"                            -- Pipe stderr to stdout.
            ]
    hPutStrLn logHandle $ "Running : " ++ script
    unless quiet $ putStr ("    " ++ script ++ " ... ")
    hFlush stdout
    exitCode <- fst <$> handle (errorExit :: SomeException -> IO a)
                        (runResourceT $ sourceCmdWithConsumer command (sinkHandle logHandle))
    case exitCode of
      ExitSuccess -> do
        unless quiet $ putStrLn "ok"
        runHaskellMigration (PGPassCached pgconfig) logHandle version
      ExitFailure _ -> errorExit exitCode
  where
    errorExit :: Show e => e -> IO a
    errorExit e = do
        print e
        hPrint logHandle e
        case mLogFilename of
          Nothing -> pure ()
          Just logFilename -> putStrLn $ "\nErrors in file: " ++ logFilename ++ "\n"
        exitFailure

-- | Create a database migration (using functionality built into Persistent). If no
-- migration is needed return 'Nothing' otherwise return the migration as 'Text'.
createMigration :: PGPassSource -> MigrationDir -> IO (Maybe FilePath)
createMigration source (MigrationDir migdir) = do
    mt <- runDbNoLogging source create
    case mt of
      Nothing -> pure Nothing
      Just (ver, mig) -> do
        let fname = renderMigrationVersionFile ver
        Text.writeFile (migdir </> fname) mig
        pure $ Just fname
  where
    create :: ReaderT SqlBackend (NoLoggingT IO) (Maybe (MigrationVersion, Text))
    create = do
      ver <- getSchemaVersion
      statements <- getMigration migrateCardanoDb
      if null statements
        then pure Nothing
        else do
          nextVer <- liftIO $ nextMigrationVersion ver
          pure $ Just (nextVer, genScript statements (mvVersion nextVer))

    genScript :: [Text] -> Int -> Text
    genScript statements next_version =
      Text.concat $
        [ "-- Persistent generated migration.\n\n"
        , "CREATE FUNCTION migrate() RETURNS void AS $$\n"
        , "DECLARE\n"
        , "  next_version int ;\n"
        , "BEGIN\n"
        , "  SELECT stage_two + 1 INTO next_version FROM schema_version ;\n"
        , "  IF next_version = " <> textShow next_version <> " THEN\n"
        ]
        ++ concatMap buildStatement statements ++
        [ "    -- Hand written SQL statements can be added here.\n"
        , "    UPDATE schema_version SET stage_two = next_version ;\n"
        , "    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;\n"
        , "  END IF ;\n"
        , "END ;\n"
        , "$$ LANGUAGE plpgsql ;\n\n"
        , "SELECT migrate() ;\n\n"
        , "DROP FUNCTION migrate() ;\n"
        ]

    buildStatement :: Text -> [Text]
    buildStatement sql = [ "    EXECUTE '",  sql,  "' ;\n" ]

    getSchemaVersion :: SqlPersistT (NoLoggingT IO) MigrationVersion
    getSchemaVersion = do
      res <- selectFirst [] []
      case res of
        Nothing -> error "getSchemaVersion failed!"
        Just x -> do
          -- Only interested in the stage2 version because that is the only stage for
          -- which Persistent migrations are generated.
          let (SchemaVersion _ stage2 _) = entityVal x
          pure $ MigrationVersion 2 stage2 0

recreateDB ::  PGPassSource -> IO ()
recreateDB pgpass = do
    runWithConnectionNoLogging pgpass $ do
      rawExecute "drop schema if exists public cascade" []
      rawExecute "create schema public" []

-- This doesn't clean the DOMAIN, so droppping the schema is a better alternative
-- for a proper cleanup
dropTables :: PGPassSource -> IO ()
dropTables pgpass = do
    runWithConnectionNoLogging pgpass $ do
      mstr <- rawSql
        ( mconcat
          [ "select string_agg('drop table \"' || tablename || '\" cascade', '; ')"
          , "from pg_tables where schemaname = 'public'"]
        )
        []
      whenJust (join $ listToMaybe mstr) $ \(Single dropsCommand) ->
        rawExecute dropsCommand []

--------------------------------------------------------------------------------

getMigrationScripts :: MigrationDir -> IO [(MigrationVersion, FilePath)]
getMigrationScripts (MigrationDir location) = do
    files <- listDirectory location
    let xs = map addVersionString (List.sort $ List.filter isMigrationScript files)
    case partitionEithers xs of
      ([], rs) -> pure rs
      (ls, _) -> error $ "getMigrationScripts: Unable to parse " ++ show ls
  where
    isMigrationScript :: FilePath -> Bool
    isMigrationScript fp =
      List.isPrefixOf "migration-" fp && List.isSuffixOf ".sql" fp

    addVersionString :: FilePath -> Either FilePath (MigrationVersion, FilePath)
    addVersionString fp =
      maybe (Left fp) (\mv -> Right (mv, fp)) $ parseMigrationVersionFromFile fp

hashMigrations :: MigrationDir -> IO [MigrationValidate]
hashMigrations migrationDir@(MigrationDir location) = do
    scripts <- getMigrationScripts migrationDir
    forM scripts $ \(_version, filepath) -> do
      file <- BS.readFile (location </> filepath)
      pure $ MigrationValidate (Text.pack . hashToStringAsHex . hashAs $ file) (Text.pack filepath)
  where
    hashAs :: ByteString -> Hash Blake2b_256 ByteString
    hashAs = hashWith id

renderMigrationValidateError :: MigrationValidateError -> Text
renderMigrationValidateError = \case
    UnknownMigrationsFound missing unknown ->
      mconcat
        [ "Inconsistent migrations found: \n"
        , "Migrations missing at runtime that were present at compilation time: "
        , textShow (map mvFilepath missing), "\n"
        , "Migrations found at runtime that were missing at compilation time: "
        , textShow (map mvFilepath unknown)
        ]


