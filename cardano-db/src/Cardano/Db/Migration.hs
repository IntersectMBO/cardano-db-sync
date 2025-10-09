{-# LANGUAGE FlexibleContexts #-}
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
  getAllTableNames,
  truncateTables,
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

import Cardano.Prelude (textShow)
import Control.Exception (Exception)
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List ((\\))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601
import GHC.Word (Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlStm
import System.Directory (listDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (
  Handle,
  IOMode (AppendMode),
  hPrint,
  hPutStrLn,
  stdout,
  withFile,
 )
import Text.Read (readMaybe)

import Cardano.BM.Trace (Trace)
import Cardano.Crypto.Hash (Blake2b_256, ByteString, Hash, hashToStringAsHex, hashWith)
import Cardano.Db.Error (mkDbCallStack)
import Cardano.Db.Migration.Version
import Cardano.Db.PGConfig
import Cardano.Db.Progress (updateProgress, withProgress)
import Cardano.Db.Run
import Cardano.Db.Schema.Variants (TxOutVariantType (..))
import qualified Cardano.Db.Statement.Function.Core as DB
import qualified Cardano.Db.Types as DB
import System.Process (readProcessWithExitCode)

newtype MigrationDir
  = MigrationDir FilePath
  deriving (Show)

newtype LogFileDir
  = LogFileDir FilePath

data MigrationValidate = MigrationValidate
  { mvHash :: !Text.Text
  , mvFilepath :: !Text.Text
  }
  deriving (Eq, Show)

data MigrationValidateError = UnknownMigrationsFound
  { missingMigrations :: ![MigrationValidate]
  , extraMigrations :: ![MigrationValidate]
  }
  deriving (Eq, Show)

instance Exception MigrationValidateError

data MigrationToRun = Initial | Full | NearTip
  deriving (Show, Eq)

-- | Run the migrations in the provided 'MigrationDir' and write date stamped log file
-- to 'LogFileDir'. It returns a list of file names of all non-official schema migration files.
runMigrations :: Maybe (Trace IO Text.Text) -> PGConfig -> Bool -> MigrationDir -> Maybe LogFileDir -> MigrationToRun -> TxOutVariantType -> IO (Bool, [FilePath])
runMigrations trce pgconfig quiet migrationDir mLogfiledir mToRun txOutVariantType = do
  allScripts <- getMigrationScripts migrationDir
  ranAll <- case (mLogfiledir, allScripts) of
    (_, []) ->
      error $ "Empty schema dir " ++ show migrationDir
    (Nothing, scripts) -> do
      putStrLn "Running:"
      (scripts', ranAll) <- filterMigrations scripts

      withProgress trce (length scripts') "Migration" $ \progressRef -> do
        forM_ (zip [1 :: Integer ..] scripts') $ \(i, script) -> do
          updateProgress trce progressRef (fromIntegral i) "Migration"
          applyMigration' Nothing stdout script

      putStrLn "Success!"
      pure ranAll
    (Just logfiledir, scripts) -> do
      logFilename <- genLogFilename logfiledir
      withFile logFilename AppendMode $ \logHandle -> do
        unless quiet $ putStrLn "Running:"
        (scripts', ranAll) <- filterMigrations scripts

        withProgress trce (length scripts') "Migration" $ \progressRef -> do
          forM_ (zip [1 :: Integer ..] scripts') $ \(i, script) -> do
            updateProgress trce progressRef (fromIntegral i) "Migration"
            applyMigration' (Just logFilename) logHandle script

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
      Full -> pure (filter filterIndexesFull scripts, True)
      Initial -> pure (filter filterInitial scripts, True)
      NearTip -> do
        pure (filter filterIndexes scripts, False)

    filterIndexesFull (mv, _) = do
      case txOutVariantType of
        TxOutVariantCore -> True
        TxOutVariantAddress -> not $ mvStage mv == 4 && mvVersion mv == 1
    filterInitial (mv, _) = mvStage mv < 4
    filterIndexes (mv, _) = do
      case txOutVariantType of
        TxOutVariantCore -> mvStage mv == 4
        TxOutVariantAddress -> mvStage mv == 4 && mvVersion mv > 1

-- Build hash for each file found in a directory.
validateMigrations :: MigrationDir -> [(Text.Text, Text.Text)] -> IO (Maybe (MigrationValidateError, Bool))
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
applyMigration (MigrationDir location) quiet pgconfig mLogFilename logHandle (_, script) = do
  hPutStrLn logHandle $ "Running : " ++ script
  unless quiet $ putStr ("    " ++ script ++ " ... ")
  -- hFlush stdout

  let psqlArgs =
        [ Text.unpack (pgcDbname pgconfig)
        , "--no-password"
        , "--quiet"
        , "--username=" <> Text.unpack (pgcUser pgconfig)
        , "--host=" <> Text.unpack (pgcHost pgconfig)
        , "--port=" <> Text.unpack (pgcPort pgconfig)
        , "--no-psqlrc"
        , "--single-transaction"
        , "--set"
        , "ON_ERROR_STOP=on"
        , "--file=" ++ location </> script
        ]

  (exitCode, stdt, stderr) <- readProcessWithExitCode "psql" psqlArgs ""
  hPutStrLn logHandle $ "Command output: " ++ stdt
  unless (null stderr) $ hPutStrLn logHandle $ "Command stderr: " ++ stderr

  case exitCode of
    ExitSuccess -> do
      unless quiet $ putStrLn "ok"
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

-- | Create a database migration.
createMigration :: PGPassSource -> MigrationDir -> TxOutVariantType -> IO (Maybe FilePath)
createMigration _source (MigrationDir _migdir) _txOutVariantType = do
  -- This would need to be completely rewritten to generate migrations manually
  -- or using a different schema management tool
  putStrLn "Warning: createMigration not implemented for Hasql. Manual migration creation required."
  pure Nothing

recreateDB :: PGPassSource -> IO ()
recreateDB pgpass = do
  runDbStandaloneTransSilent pgpass $ do
    DB.runSession mkDbCallStack $
      HsqlS.statement () $
        HsqlStm.Statement
          "DROP SCHEMA IF EXISTS public CASCADE"
          HsqlE.noParams
          HsqlD.noResult
          True

    DB.runSession mkDbCallStack $
      HsqlS.statement () $
        HsqlStm.Statement
          "CREATE SCHEMA public"
          HsqlE.noParams
          HsqlD.noResult
          True

getAllTableNames :: PGPassSource -> IO [Text.Text]
getAllTableNames pgpass = do
  runDbStandaloneTransSilent pgpass $ do
    DB.runSession mkDbCallStack $
      HsqlS.statement () $
        HsqlStm.Statement
          "SELECT tablename FROM pg_catalog.pg_tables WHERE schemaname = current_schema()"
          HsqlE.noParams
          (HsqlD.rowList $ HsqlD.column (HsqlD.nonNullable HsqlD.text))
          True

truncateTables :: PGPassSource -> [Text.Text] -> IO ()
truncateTables pgpass tables =
  runDbStandaloneTransSilent pgpass $ do
    DB.runSession mkDbCallStack $
      HsqlS.statement () $
        HsqlStm.Statement
          (TextEnc.encodeUtf8 ("TRUNCATE " <> Text.intercalate ", " tables <> " CASCADE"))
          HsqlE.noParams
          HsqlD.noResult
          True

getMaintenancePsqlConf :: PGConfig -> IO Text.Text
getMaintenancePsqlConf pgconfig = runDbStandaloneTransSilent (PGPassCached pgconfig) $ do
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

showMaintenanceWorkMem :: DB.DbM [Text.Text]
showMaintenanceWorkMem =
  DB.runSession mkDbCallStack $
    HsqlS.statement () $
      HsqlStm.Statement
        "SHOW maintenance_work_mem"
        HsqlE.noParams
        (HsqlD.rowList $ HsqlD.column (HsqlD.nonNullable HsqlD.text))
        True

showMaxParallelMaintenanceWorkers :: DB.DbM [Text.Text]
showMaxParallelMaintenanceWorkers =
  DB.runSession mkDbCallStack $
    HsqlS.statement () $
      HsqlStm.Statement
        "SHOW max_parallel_maintenance_workers"
        HsqlE.noParams
        (HsqlD.rowList $ HsqlD.column (HsqlD.nonNullable HsqlD.text))
        True

-- This doesn't clean the DOMAIN, so droppping the schema is a better alternative
-- for a proper cleanup
dropTables :: PGPassSource -> IO ()
dropTables pgpass = do
  runDbStandaloneTransSilent pgpass $ do
    mstr <-
      DB.runSession mkDbCallStack $
        HsqlS.statement () $
          HsqlStm.Statement
            ( mconcat
                [ "SELECT string_agg('drop table \"' || tablename || '\" cascade', '; ')"
                , "FROM pg_tables WHERE schemaname = 'public'"
                ]
            )
            HsqlE.noParams
            (HsqlD.rowMaybe $ HsqlD.column (HsqlD.nonNullable HsqlD.text))
            True

    whenJust mstr $ \dropsCommand ->
      DB.runSession mkDbCallStack $
        HsqlS.statement dropsCommand $
          HsqlStm.Statement
            "$1"
            (HsqlE.param $ HsqlE.nonNullable HsqlE.text)
            HsqlD.noResult
            True

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

renderMigrationValidateError :: MigrationValidateError -> Text.Text
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

noLedgerMigrations :: DB.DbEnv -> Trace IO Text.Text -> IO ()
noLedgerMigrations dbEnv trce = do
  let action :: DB.DbM ()
      action = do
        DB.runSession mkDbCallStack $
          HsqlS.statement () $
            HsqlStm.Statement
              "UPDATE redeemer SET fee = NULL"
              HsqlE.noParams
              HsqlD.noResult
              True

        DB.runSession mkDbCallStack $
          HsqlS.statement () $
            HsqlStm.Statement
              "DELETE FROM reward"
              HsqlE.noParams
              HsqlD.noResult
              True

        DB.runSession mkDbCallStack $
          HsqlS.statement () $
            HsqlStm.Statement
              "DELETE FROM epoch_stake"
              HsqlE.noParams
              HsqlD.noResult
              True

        DB.runSession mkDbCallStack $
          HsqlS.statement () $
            HsqlStm.Statement
              "DELETE FROM ada_pots"
              HsqlE.noParams
              HsqlD.noResult
              True

        DB.runSession mkDbCallStack $
          HsqlS.statement () $
            HsqlStm.Statement
              "DELETE FROM epoch_param"
              HsqlE.noParams
              HsqlD.noResult
              True

  void $ runDbDirectLogged trce dbEnv action

queryPgIndexesCount :: DB.DbM Word64
queryPgIndexesCount = do
  indexesExists <-
    DB.runSession mkDbCallStack $
      HsqlS.statement () $
        HsqlStm.Statement
          "SELECT indexname FROM pg_indexes WHERE schemaname = 'public'"
          HsqlE.noParams
          (HsqlD.rowList $ HsqlD.column (HsqlD.nonNullable HsqlD.text))
          True
  pure $ fromIntegral (length indexesExists)
