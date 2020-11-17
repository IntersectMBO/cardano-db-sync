{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.BM.Trace (Trace)
import           Cardano.Prelude

import           Cardano.DbSync.Config
import qualified Cardano.DbSync.Era.Byron.Insert as Byron
import qualified Cardano.DbSync.Era.Shelley.Insert as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Rollback (rollbackToSlot)
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Control.Monad.Logger (LoggingT)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))


-- | The default DbSyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: DbSyncNodePlugin
defDbSyncNodePlugin =
  DbSyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock]
    , plugRollbackBlock = [rollbackToSlot]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: Trace IO Text -> DbSyncEnv -> LedgerStateVar -> BlockDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertDefaultBlock tracer env ledgerStateVar (BlockDetails cblk details) = do
  -- Calculate the new ledger state to pass to the DB insert functions but do not yet
  -- update ledgerStateVar.
  lStateSnap <- liftIO $ applyBlock ledgerStateVar cblk
  res <- case cblk of
            BlockByron blk ->
              Byron.insertByronBlock tracer blk details
            BlockShelley blk ->
              Shelley.insertShelleyBlock tracer env blk lStateSnap details
            BlockAllegra _ ->
              panic "insertDefaultBlock: BlockAllegra"
            BlockMary _ ->
              panic "insertDefaultBlock: BlockMary"
  -- Now we update it in ledgerStateVar and (possibly) store it to disk.
  liftIO $ saveLedgerState (envLedgerStateDir env) ledgerStateVar
                (lssState lStateSnap) (isSyncedWithinSeconds details 60)
  pure res
