module Cardano.DbTool.PrepareSnapshot (
  PrepareSnapshotArgs (..),
  runPrepareSnapshot,
) where

import Cardano.Db
import Cardano.DbSync.Config.Types hiding (LogFileDir)
import Cardano.Prelude (Word64, fromMaybe)
import Control.Monad
import Data.Version (makeVersion, showVersion, versionBranch)
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..), defaultListSnapshots, snapshotToDirName)
import Ouroboros.Network.Block hiding (blockHash)
import Paths_cardano_db_tool (version)
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.FilePath ((</>))
import System.Info (arch, os)

newtype PrepareSnapshotArgs = PrepareSnapshotArgs
  { unPrepareSnapshotArgs :: LedgerStateDir
  }

runPrepareSnapshot :: PrepareSnapshotArgs -> IO ()
runPrepareSnapshot args = do
  let someHasFS = SomeHasFS $ ioHasFS (MountPoint $ unLedgerStateDir (unPrepareSnapshotArgs args))
  snapshots <- defaultListSnapshots someHasFS
  mblock <- runDbStandaloneSilent queryLatestBlock
  case mblock of
    Just block | Just bSlotNo <- SlotNo <$> blockSlotNo block -> do
      let targetSlot = unSlotNo bSlotNo
      -- Find the snapshot matching the DB tip slot, or the closest older one
      let (newer, rest) = span (\ds -> dsNumber ds > targetSlot) snapshots
          (matching, older) = case rest of
            (ds : os') | dsNumber ds == targetSlot -> ([ds], os')
            _ -> ([], rest)
      printNewerSnapshots newer
      case (matching, older) of
        (file : _, _) -> do
          let bblockNo = fromMaybe 0 $ blockBlockNo block
          printCreateSnapshot bblockNo (unLedgerStateDir (unPrepareSnapshotArgs args) </> snapshotToDirName file)
        (_, file : _) -> do
          putStrLn $
            concat
              [ "Ledger and db don't match. DB tip is at slot "
              , show bSlotNo
              , " and the closest snapshot is at slot "
              , show (dsNumber file)
              , ". DBSync no longer requires them to match and "
              , "no rollback will be performed."
              ]
          let bblockNo = fromMaybe 0 $ blockBlockNo block
          printCreateSnapshot bblockNo (unLedgerStateDir (unPrepareSnapshotArgs args) </> snapshotToDirName file)
        (_, []) ->
          putStrLn "No snapshot before the tip found. Snapshots without ledger are not supported yet."
    _ -> do
      putStrLn "The db is empty. You need to sync from genesis and then create a snapshot."
  where
    printNewerSnapshots :: [DiskSnapshot] -> IO ()
    printNewerSnapshots newerFiles = do
      unless (null newerFiles) $
        putStrLn $
          concat
            [ "There are newer snapshots, which are ignored: "
            , show (map snapshotToDirName newerFiles)
            , "\n"
            ]

    printCreateSnapshot :: Word64 -> String -> IO ()
    printCreateSnapshot bblockNo snapshotName = do
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
          , snapshotName
          ]
