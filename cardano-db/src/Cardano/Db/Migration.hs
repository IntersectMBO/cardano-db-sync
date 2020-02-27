{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Migration
  ( MigrationDir (..)
  , LogFileDir (..)
  , createMigration
  , applyMigration
  , runMigrations
  ) where

import           Control.Exception (SomeException, bracket, handle)
import           Control.Monad (forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Conduit.Binary (sinkHandle)
import           Data.Conduit.Process (sourceCmdWithConsumer)
import           Data.Either (partitionEithers)
import qualified Data.List as List
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

import           Database.Persist.Sql (SqlBackend, SqlPersistT, entityVal, getMigration, selectFirst)

import           Cardano.Db.Migration.Haskell
import           Cardano.Db.Migration.Version
import           Cardano.Db.PGConfig
import           Cardano.Db.Run
import           Cardano.Db.Schema

import           System.Directory (listDirectory)
import           System.Exit (ExitCode (..), exitFailure)
import           System.FilePath ((</>), takeFileName)
import           System.IO (Handle, IOMode (AppendMode), hClose, hFlush, hPrint, hPutStrLn,
                    openFile, stdout)



newtype MigrationDir
  = MigrationDir FilePath

newtype LogFileDir
  = LogFileDir FilePath

-- | Run the migrations in the provided 'MigrationDir' and write date stamped log file
-- to 'LogFileDir'.
runMigrations :: (PGConfig -> PGConfig) -> Bool -> MigrationDir -> LogFileDir -> IO ()
runMigrations cfgOverride quiet migrationDir logfiledir = do
    pgconfig <- cfgOverride <$> readPGPassFileEnv
    logFilename <- genLogFilename logfiledir
    bracket (openFile logFilename AppendMode) hClose $ \logHandle -> do
      unless quiet $ putStrLn "Running:"
      scripts <- getMigrationScripts migrationDir
      forM_ scripts $ applyMigration quiet pgconfig (logFilename, logHandle)
      unless quiet $ putStrLn "Success!"
  where
    genLogFilename :: LogFileDir -> IO FilePath
    genLogFilename (LogFileDir logdir) =
      (logdir </>)
        . formatTime defaultTimeLocale ("migrate-" ++ iso8601DateFormat (Just "%H%M%S") ++ ".log")
        <$> getCurrentTime

applyMigration :: Bool -> PGConfig -> (FilePath, Handle) -> (MigrationVersion, FilePath) -> IO ()
applyMigration quiet pgconfig (logFilename, logHandle) (version, script) = do
    -- This assumes that the credentials for 'psql' are already sorted out.
    -- One way to achive this is via a 'PGPASSFILE' environment variable
    -- as per the PostgreSQL documentation.
    let command =
          List.intercalate " "
            [ "psql"
            , BS.unpack (pgcDbname pgconfig)
            , "--no-password"
            , "--quiet"
            , "--username=" <> BS.unpack (pgcUser pgconfig)
            , "--host=" <> BS.unpack (pgcHost pgconfig)
            , "--no-psqlrc"                     -- Ignore the ~/.psqlrc file.
            , "--single-transaction"            -- Run the file as a transaction.
            , "--set ON_ERROR_STOP=on"          -- Exit with non-zero on error.
            , "--file='" ++ script ++ "'"
            , "2>&1"                            -- Pipe stderr to stdout.
            ]
    hPutStrLn logHandle $ "Running : " ++ takeFileName script
    unless quiet $ putStr ("    " ++ takeFileName script ++ " ... ")
    hFlush stdout
    exitCode <- fst <$> handle (errorExit :: SomeException -> IO a)
                        (runResourceT $ sourceCmdWithConsumer command (sinkHandle logHandle))
    case exitCode of
      ExitSuccess -> do
        unless quiet $ putStrLn "ok"
        runHaskellMigration logHandle version
      ExitFailure _ -> errorExit exitCode
  where
    errorExit :: Show e => e -> IO a
    errorExit e = do
        print e
        hPrint logHandle e
        putStrLn $ "\nErrors in file: " ++ logFilename ++ "\n"
        exitFailure

-- | Create a database migration (using functionality built into Persistent). If no
-- migration is needed return 'Nothing' otherwise return the migration as 'Text'.
createMigration :: MigrationDir -> IO (Maybe FilePath)
createMigration (MigrationDir migdir) = do
    mt <- runDbNoLogging create
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
        , "    UPDATE schema_version SET stage_two = ", textShow next_version, " ;\n"
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
      maybe (Left fp) (\mv -> Right (mv, location </> fp)) $ parseMigrationVersionFromFile fp

textShow :: Show a => a -> Text
textShow = Text.pack . show
