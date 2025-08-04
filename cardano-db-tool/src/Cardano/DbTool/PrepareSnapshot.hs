module Cardano.DbTool.PrepareSnapshot (
  PrepareSnapshotArgs (..),
  runPrepareSnapshot,
) where

import Cardano.Db
import Cardano.DbSync.Config.Types hiding (LogFileDir)
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types (LedgerStateFile (..))
import Cardano.Prelude (Word64, fromMaybe)
import Control.Monad
import qualified Data.ByteString.Base16 as Base16
import Data.Version (makeVersion, showVersion, versionBranch)
import Ouroboros.Network.Block hiding (blockHash)
import Paths_cardano_db_tool (version)
import System.Info (arch, os)

newtype PrepareSnapshotArgs = PrepareSnapshotArgs
  { unPrepareSnapshotArgs :: LedgerStateDir
  }

runPrepareSnapshot :: PrepareSnapshotArgs -> IO ()
runPrepareSnapshot args = do
  ledgerFiles <- listLedgerStateFilesOrdered (unPrepareSnapshotArgs args)
  mblock <- runDbStandaloneSilent queryLatestBlock
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
          putStrLn $
            concat
              [ "Ledger and db don't match. DB tip is at "
              , show bSlotNo
              , " "
              , show (hashToAnnotation bHash)
              , " (full "
              , show (Base16.encode bHash)
              , ")"
              , " and the closest ledger state file is at "
              , show (lsfSlotNo file)
              , " "
              , show (lsfHash file)
              , ". DBSync no longer requires them to match and "
              , "no rollback will be performed."
              ]
          let bblockNo = fromMaybe 0 $ blockBlockNo block
          printCreateSnapshot bblockNo (lsfFilePath file)
        (_, []) ->
          putStrLn "No ledger state file before the tip found. Snapshots without ledger are not supported yet."
    _ -> do
      putStrLn "The db is empty. You need to sync from genesis and then create a snapshot."
  where
    printNewerSnapshots :: [LedgerStateFile] -> IO ()
    printNewerSnapshots newerFiles = do
      unless (null newerFiles) $
        putStrLn $
          concat
            [ "There are newer ledger state files, which are ignored: "
            , show newerFiles
            , "\n"
            ]

    printCreateSnapshot :: Word64 -> FilePath -> IO ()
    printCreateSnapshot bblockNo fp = do
      let schemaVersion = makeVersion $ take 2 $ versionBranch version
          cmdStr =
            "Create a snapshot with:\n"
              ++ case os of
                "freebsd" -> "     cardano-db-sync-pgsql-setup"
                _otherwise -> "     scripts/postgresql-setup.sh"
              ++ " --create-snapshot db-sync-snapshot"
      putStrLn $
        concat
          [ cmdStr
          , "-schema-"
          , showVersion schemaVersion
          , "-block-"
          , show bblockNo
          , "-" ++ arch ++ " "
          , fp
          ]
