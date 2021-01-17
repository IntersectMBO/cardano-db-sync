{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( mkDefDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.BM.Trace (Trace)
import           Cardano.Prelude

import           Cardano.DbSync.Config
import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
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
mkDefDbSyncNodePlugin :: IO DbSyncNodePlugin
mkDefDbSyncNodePlugin = do
  cache <- newCache
  return $ DbSyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock cache]
    , plugRollbackBlock = [rollbackToSlot]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: Cache -> Trace IO Text -> DbSyncEnv -> LedgerStateVar -> BlockDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertDefaultBlock cache tracer env ledgerStateVar (BlockDetails cblk details) = do
  -- Calculate the new ledger state to pass to the DB insert functions but do not yet
  -- update ledgerStateVar.
  lStateSnap <- liftIO $ applyBlock env ledgerStateVar cblk
  res <- case cblk of
            BlockByron blk ->
              insertByronBlock tracer cache blk details
            BlockShelley blk ->
              insertShelleyBlock tracer cache env (Generic.fromShelleyBlock blk) lStateSnap details
            BlockAllegra blk ->
              insertShelleyBlock tracer cache env (Generic.fromAllegraBlock blk) lStateSnap details
            BlockMary blk ->
              insertShelleyBlock tracer cache env (Generic.fromMaryBlock blk) lStateSnap details
  -- Now we update it in ledgerStateVar and (possibly) store it to disk.
  liftIO $ saveLedgerState (envLedgerStateDir env) ledgerStateVar
                lStateSnap (isSyncedWithinSeconds details 60)
  pure res
