{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Migration (
  MigrationDir (..),
  LogFileDir (..),
  MigrationToRun (..),
  applyMigration,
  createMigration,
  getMigrationScripts,
  runMigrations,
  recreateDB,
  dropTables,
  getMaintenancePsqlConf,
  MigrationValidate (..),
  MigrationValidateError (..),
  validateMigrations,
  hashMigrations,
  renderMigrationValidateError,
  noLedgerMigrations,
  queryPgIndexesCount,
) where

import Cardano.BM.Trace (Trace)
import Cardano.Crypto.Hash (Blake2b_256, ByteString, Hash, hashToStringAsHex, hashWith)
import Cardano.Db.Migration.Haskell
import Cardano.Db.Migration.Version
import Cardano.Db.PGConfig
import Cardano.Db.Query
import Cardano.Db.Run
import Cardano.Db.Schema
import Cardano.Db.Text
import Cardano.Prelude (Typeable)
import Control.Exception (Exception, SomeException, handle)
import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import Data.Conduit.Binary (sinkHandle)
import Data.Conduit.Process (sourceCmdWithConsumer)
import Data.Either (partitionEithers)
import Data.List ((\\))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601
import Database.Persist.Sql (
  Single (..),
  SqlBackend,
  SqlPersistT,
  entityVal,
  getMigration,
  rawExecute,
  rawSql,
  selectFirst,
 )
import GHC.Word (Word64)
import System.Directory (listDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (
  Handle,
  IOMode (AppendMode),
  hFlush,
  hPrint,
  hPutStrLn,
  stdout,
  withFile,
 )
import Text.Read (readMaybe)

newtype MigrationDir
  = MigrationDir FilePath
  deriving (Show)

newtype LogFileDir
  = LogFileDir FilePath

data MigrationValidate = MigrationValidate
  { mvHash :: Text
  , mvFilepath :: Text
  }
  deriving (Eq, Show)

data MigrationValidateError = UnknownMigrationsFound
  { missingMigrations :: [MigrationValidate]
  , extraMigrations :: [MigrationValidate]
  }
  deriving (Eq, Show, Typeable)

instance Exception MigrationValidateError

data MigrationToRun = Initial | Full | Fix | Indexes
  deriving (Show, Eq)

-- | Run the migrations in the provided 'MigrationDir' and write date stamped log file
-- to 'LogFileDir'. It returns a list of file names of all non-official schema migration files.
runMigrations :: PGConfig -> Bool -> MigrationDir -> Maybe LogFileDir -> MigrationToRun -> IO (Bool, [FilePath])
runMigrations pgconfig quiet migrationDir mLogfiledir mToRun = do
  allScripts <- getMigrationScripts migrationDir
  ranAll <- case (mLogfiledir, allScripts) of
    (_, []) ->
      error $ "Empty schema dir " ++ show migrationDir
    (Nothing, schema : scripts) -> do
      putStrLn "Running:"
      applyMigration' Nothing stdout schema
      (scripts', ranAll) <- filterMigrations scripts
      forM_ scripts' $ applyMigration' Nothing stdout
      putStrLn "Success!"
      pure ranAll
    (Just logfiledir, schema : scripts) -> do
      logFilename <- genLogFilename logfiledir
      withFile logFilename AppendMode $ \logHandle -> do
        unless quiet $ putStrLn "Running:"
        applyMigration' (Just logFilename) logHandle schema
        (scripts', ranAll) <- filterMigrations scripts
        forM_ scripts' $ applyMigration' (Just logFilename) logHandle
        unless quiet $ putStrLn "Success!"
        pure ranAll
  pure (ranAll, map (takeFileName . snd) (filter isUnofficialMigration allScripts))
  where
    isUnofficialMigration :: (MigrationVersion, FilePath) -> Bool
    isUnofficialMigration (mv, _) = mvStage mv < 1 || mvStage mv > 4

    genLogFilename :: LogFileDir -> IO FilePath
    genLogFilename (LogFileDir logdir) = do
      tm <- getCurrentTime
      pure $ logdir </> "migrate-" ++ iso8601Show tm ++ ".log"

    applyMigration' = applyMigration migrationDir quiet pgconfig

    filterMigrations :: [(MigrationVersion, FilePath)] -> IO ([(MigrationVersion, FilePath)], Bool)
    filterMigrations scripts = case mToRun of
      Full -> do
        mVersion <- runWithConnectionNoLogging (PGPassCached pgconfig) querySchemaVersion
        case mVersion of
          Just (SchemaVersion _ v _) | v == hardCoded3_0 -> do
            pure (filter (not . filterFix) scripts, False)
          _ -> pure (scripts, True)
      Initial -> do
        mVersion <- runWithConnectionNoLogging (PGPassCached pgconfig) querySchemaVersion
        case mVersion of
          Just (SchemaVersion _ v _) | v == hardCoded3_0 -> do
            pure (filter (\m -> not $ filterFix m || filterIndexes m) scripts, False)
          _ -> pure (filter (not . filterIndexes) scripts, True)
      Fix -> pure (filter filterFix scripts, False)
      Indexes -> pure (filter filterIndexes scripts, False)

    filterFix (mv, _) = mvStage mv == 2 && mvVersion mv > hardCoded3_0
    filterIndexes (mv, _) = mvStage mv == 4

hardCoded3_0 :: Int
hardCoded3_0 = 19

-- Build hash for each file found in a directory.
validateMigrations :: MigrationDir -> [(Text, Text)] -> IO (Maybe (MigrationValidateError, Bool))
validateMigrations migrationDir knownMigrations = do
  let knownMigs = uncurry MigrationValidate <$> knownMigrations
  scripts <- filter (isOfficialMigrationFile . Text.unpack . mvFilepath) <$> liftIO (hashMigrations migrationDir)
  if scripts == knownMigs
    then pure Nothing
    else do
      let missingMigr = knownMigs \\ scripts
      let extraMigr = scripts \\ knownMigs
      let unknown =
            UnknownMigrationsFound
              { missingMigrations = missingMigr -- Migrations missing at runtime that were present at compilation time
              , extraMigrations = extraMigr -- Migrations found at runtime that were missing at compilation time
              }
      pure $ Just (unknown, all stage4 missingMigr && all stage3or4 extraMigr)
  where
    stage4 = (== 4) . readStageFromFilename . Text.unpack . mvFilepath
    stage3or4 = flip elem [3, 4] . readStageFromFilename . Text.unpack . mvFilepath

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
          , "--no-psqlrc" -- Ignore the ~/.psqlrc file.
          , "--single-transaction" -- Run the file as a transaction.
          , "--set ON_ERROR_STOP=on" -- Exit with non-zero on error.
          , "--file='" ++ location </> script ++ "'"
          , "2>&1" -- Pipe stderr to stdout.
          ]
  hPutStrLn logHandle $ "Running : " ++ script
  unless quiet $ putStr ("    " ++ script ++ " ... ")
  hFlush stdout
  exitCode <-
    fst
      <$> handle
        (errorExit :: SomeException -> IO a)
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
          ++ concatMap buildStatement statements
          ++ [ "    -- Hand written SQL statements can be added here.\n"
             , "    UPDATE schema_version SET stage_two = next_version ;\n"
             , "    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;\n"
             , "  END IF ;\n"
             , "END ;\n"
             , "$$ LANGUAGE plpgsql ;\n\n"
             , "SELECT migrate() ;\n\n"
             , "DROP FUNCTION migrate() ;\n"
             ]

    buildStatement :: Text -> [Text]
    buildStatement sql = ["    EXECUTE '", sql, "' ;\n"]

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

recreateDB :: PGPassSource -> IO ()
recreateDB pgpass = do
  runWithConnectionNoLogging pgpass $ do
    rawExecute "drop schema if exists public cascade" []
    rawExecute "create schema public" []

getMaintenancePsqlConf :: PGConfig -> IO Text
getMaintenancePsqlConf pgconfig = runWithConnectionNoLogging (PGPassCached pgconfig) $ do
  mem <- showMaintenanceWorkMem
  workers <- showMaxParallelMaintenanceWorkers
  pure $
    mconcat
      [ "Found maintenance_work_mem="
      , mconcat mem
      , ", "
      , "max_parallel_maintenance_workers="
      , mconcat workers
      ]

showMaintenanceWorkMem :: ReaderT SqlBackend (NoLoggingT IO) [Text]
showMaintenanceWorkMem =
  fmap unSingle <$> rawSql "show maintenance_work_mem" []

showMaxParallelMaintenanceWorkers :: ReaderT SqlBackend (NoLoggingT IO) [Text]
showMaxParallelMaintenanceWorkers =
  fmap unSingle <$> rawSql "show max_parallel_maintenance_workers" []

-- This doesn't clean the DOMAIN, so droppping the schema is a better alternative
-- for a proper cleanup
dropTables :: PGPassSource -> IO ()
dropTables pgpass = do
  runWithConnectionNoLogging pgpass $ do
    mstr <-
      rawSql
        ( mconcat
            [ "select string_agg('drop table \"' || tablename || '\" cascade', '; ')"
            , "from pg_tables where schemaname = 'public'"
            ]
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
      , textShow (map mvFilepath missing)
      , "\n"
      , "Migrations found at runtime that were missing at compilation time: "
      , textShow (map mvFilepath unknown)
      ]

isOfficialMigrationFile :: FilePath -> Bool
isOfficialMigrationFile fn =
  takeExtension fn == ".sql" && stage >= 1 && stage <= 4
  where
    stage = readStageFromFilename fn

-- Reimplement part of `parseMigrationVersionFromFile` because that function is not avaliable
-- here. Defaults to a stage value of `0`.
readStageFromFilename :: FilePath -> Int
readStageFromFilename fn =
  case takeWhile isDigit . drop 1 $ dropWhile (/= '-') (takeFileName fn) of
    stage -> fromMaybe 0 $ readMaybe stage

noLedgerMigrations :: SqlBackend -> Trace IO Text -> IO ()
noLedgerMigrations backend trce = do
  void $ runDbIohkLogging backend trce $ do
    rawExecute "update redeemer set fee = null" []
    rawExecute "delete from reward" []
    rawExecute "delete from epoch_stake" []
    rawExecute "delete from ada_pots" []
    rawExecute "delete from epoch_param" []

queryPgIndexesCount :: MonadIO m => ReaderT SqlBackend m Word64
queryPgIndexesCount = do
  indexesExists :: [Text] <-
    fmap unSingle
      <$> rawSql
        ( mconcat
            [ "SELECT indexname FROM pg_indexes WHERE schemaname = 'public'"
            ]
        )
        []
  pure $ fromIntegral (length indexesExists)
