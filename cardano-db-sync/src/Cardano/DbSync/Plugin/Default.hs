{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
import           Cardano.DbSync.Rollback (rollbackToSlot)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Cardano.Sync.Api
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Plugin
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Control.Monad.Logger (LoggingT)

import qualified Data.List as List
import           Data.List.Split.Internals (chunksOf)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

-- | The default SyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
defDbSyncNodePlugin backend =
  SyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock backend]
    , plugRollbackBlock = [rollbackToSlot backend]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: SqlBackend -> Trace IO Text -> SyncEnv -> [BlockDetails]
    -> IO (Either SyncNodeError ())
insertDefaultBlock backend tracer env blockDetails = do
    -- Take the list of BlockDetails, split the list into chunks that only contain the same
    -- epoch and then insert the chunks.
    traverseMEither insertEpochChunk $ chunkByEpoch blockDetails
  where
    insertEpochChunk :: [BlockDetails] -> IO (Either SyncNodeError ())
    insertEpochChunk =
      DB.runDbIohkLogging backend tracer . traverseMEither insertBlock

    insertBlock :: BlockDetails -> ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    insertBlock (BlockDetails cblk details) = do
      -- Calculate the new ledger state to pass to the DB insert functions but do not yet
      -- update ledgerStateVar.
      let network = leNetwork (envLedger env)
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk details
      liftIO $ handleLedgerEvents tracer (lssEvents lStateSnap)
      res <- case cblk of
                BlockByron blk ->
                  insertByronBlock tracer blk details
                BlockShelley blk ->
                  insertShelleyBlock tracer network (Generic.fromShelleyBlock blk) lStateSnap details
                BlockAllegra blk ->
                  insertShelleyBlock tracer network (Generic.fromAllegraBlock blk) lStateSnap details
                BlockMary blk ->
                  insertShelleyBlock tracer network (Generic.fromMaryBlock blk) lStateSnap details
      -- Now we update it in ledgerStateVar and (possibly) store it to disk.
      liftIO $ saveLedgerStateMaybe (envLedger env)
                    lStateSnap (isSyncedWithinSeconds details 60)
      pure res

-- -------------------------------------------------------------------------------------------------

chunkByEpoch :: [BlockDetails] -> [[BlockDetails]]
chunkByEpoch ws =
  case ws of
      [] -> []
      xs@(xh:_) ->
        -- Parition is fine here because the list is already ordered, so partioning by
        -- the epoch number will retain the ordering.
        case List.partition (\x -> sdEpochNo (bdSlot x) == sdEpochNo (bdSlot xh)) xs of
          ([], _) -> panic "Cardano.DbSync.Plugin.Default.chunkByEpoch: Impossible"
          (ys, []) -> [ys]
          (ys, zs) -> chunksOf 1500 ys ++ chunkByEpoch zs


handleLedgerEvents :: Trace IO Text -> [LedgerEvent] -> IO ()
handleLedgerEvents tracer =
    mapM_ printer
  where
    printer :: LedgerEvent -> IO ()
    printer ev =
      case ev of
        LedgerNewEpoch en ->
          logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)

        LedgerRewards details rwds -> do
          logInfo tracer $
            mconcat
              [ "LedgerRewards "
              , textShow  ( unEpochSlot (sdEpochSlot details)
                          , unEpochSize (sdEpochSize details)
                          , length (Generic.rwdRewards rwds) + length (Generic.rwdOrphaned rwds)
                          )
              ]
