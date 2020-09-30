{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.Prelude
import           Cardano.BM.Trace (Trace)

import           Cardano.DbSync.Config
import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Era.Byron.Insert as Byron
import qualified Cardano.DbSync.Era.Shelley.Insert as Shelley
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Rollback (rollbackToSlot)
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockByron, BlockShelley),
                    LedgerState (LedgerStateByron, LedgerStateShelley))


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
  (newLedgerState, mRewards) <- liftIO $ applyBlock ledgerStateVar cblk
  res <- case (cblk, clsState newLedgerState) of
            (BlockByron blk, LedgerStateByron _st) ->
              Byron.insertByronBlock tracer blk details
            (BlockShelley blk, LedgerStateShelley lstate) ->
              Shelley.insertShelleyBlock tracer env blk lstate mRewards details
            -- Should never happen.
            _otherwise -> panic "insertDefaultBlock: Era mismatch on block and ledger state"
  -- Now we update it in ledgerStateVar and (possibly) store it to disk.
  liftIO $ saveLedgerState (LedgerStateDir "ledger-state") ledgerStateVar
                newLedgerState (isSyncedWithinSeconds details 60)
  pure res
