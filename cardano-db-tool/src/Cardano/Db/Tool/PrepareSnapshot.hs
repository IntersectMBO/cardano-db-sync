module Cardano.Db.Tool.PrepareSnapshot
  ( PrepareSnapshotArgs (..)
  , runPrepareSnapshot
  ) where

import           Cardano.Prelude (Word64, fromMaybe)

import           Control.Monad

import           Cardano.Db
import           Cardano.DbSync.Config.Types hiding (LogFileDir, MigrationDir)
import           Cardano.DbSync.LedgerState

import qualified Data.ByteString.Base16 as Base16
import           Data.Version (versionBranch)

import           Ouroboros.Network.Block hiding (blockHash)

import           Paths_cardano_db_tool (version)

import           System.IO (hFlush, stdout)

newtype PrepareSnapshotArgs = PrepareSnapshotArgs
  { unPrepareSnapshotArgs :: LedgerStateDir
  }

runPrepareSnapshot :: PrepareSnapshotArgs -> IO ()
runPrepareSnapshot = runPrepareSnapshotAux True

runPrepareSnapshotAux :: Bool -> PrepareSnapshotArgs -> IO ()
runPrepareSnapshotAux firstTry args = do
    ledgerFiles <- listLedgerStateFilesOrdered (unPrepareSnapshotArgs args)
    mblock <- runDbNoLogging queryLatestBlock
    case mblock of
      Just block | Just bSlotNo <- SlotNo <$> blockSlotNo block -> do
        let bHash = blockHash block
        let (newerFiles, mfile, olderFiles) = findLedgerStateFile ledgerFiles (bSlotNo, bHash)
        printNewerSnapshots newerFiles
        case (mfile, olderFiles) of
            (Just file, _) -> do
              let bblockNo = fromMaybe 0 $ blockBlockNo block
              printCreateSnapshot bblockNo (lsfFilePath file)
            (_, file : _) -> do
                -- We couldn't find the tip of the db, so we return a list of
                -- the available ledger files, before this tip.
              putStrLn $ concat
                [ "Ledger and db don't match. DB tip is at "
                , show bSlotNo, " ", show (hashToAnnotation bHash)
                , " (full ", show (Base16.encode bHash), ")"
                , " and the closest ledger state file is at "
                , show (lsfSlotNo file), " ", show (lsfHash file)
                ]
              if firstTry then do
                interactiveRollback $ lsfSlotNo file
                runPrepareSnapshotAux False args
              else
                putStrLn "After a rollback the db is in sync with no ledger state file"
            (_, []) ->
              putStrLn "No ledger state file matches the db tip. You need to run db-sync before creating a snapshot"

      _ -> do
            putStrLn "The db is empty. You need to sync from genesis and then create a snapshot."
  where
    interactiveRollback :: SlotNo -> IO ()
    interactiveRollback slot = do
      putStr $ "Do you want to rollback the db to " ++ show slot  ++ " (Y/n): "
      hFlush stdout
      input <- getLine
      case input of
        "n" -> pure ()
        _ -> do
          putStrLn $ "Rolling back to " ++ show slot
          runRollback slot
          putStrLn "Rolling back done. Revalidating from scratch"
          putStrLn ""

    runRollback :: SlotNo -> IO ()
    runRollback slot = runDbNoLogging $ do
      slots <- querySlotNosGreaterThan $ unSlotNo slot
      mapM_ deleteCascadeSlotNo slots

    printNewerSnapshots :: [LedgerStateFile] -> IO ()
    printNewerSnapshots newerFiles = do
      unless (null newerFiles) $
        putStrLn $ concat
          [ "There are newer ledger state files, which are ignored: "
          , show newerFiles, "\n"
          ]

    printCreateSnapshot :: Word64 -> FilePath -> IO ()
    printCreateSnapshot bblockNo fp = do
      let mMajor = listToMaybe $ versionBranch version
          majorStr = case mMajor of
                        Nothing -> ""
                        Just majorV -> "schema-" ++ show majorV
      putStrLn $ concat
        [ "Create a snapshot with:\n"
        , "    scripts/postgresql-setup.sh --create-snapshot db-sync-snapshot-"
        , majorStr
        , "-block-"
        , show bblockNo
        , "-x86_64 "
        , fp
        ]
