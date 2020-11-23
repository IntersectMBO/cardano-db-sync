{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where


import           Cardano.BM.Trace (Trace, logError)
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

import           Ouroboros.Consensus.Cardano.Block (AllegraEra, HardForkBlock (..), MaryEra,
                   ShelleyEra, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

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
              insertShelleyBlock tracer env blk lStateSnap details
            BlockAllegra blk ->
              insertAllegraBlock tracer env blk lStateSnap details
            BlockMary blk ->
              insertMaryBlock tracer env blk lStateSnap details
  -- Now we update it in ledgerStateVar and (possibly) store it to disk.
  liftIO $ saveLedgerState (envLedgerStateDir env) ledgerStateVar
                (lssState lStateSnap) (isSyncedWithinSeconds details 60)
  pure res

-- -------------------------------------------------------------------------------------------------

insertShelleyBlock
    :: Trace IO Text -> DbSyncEnv -> ShelleyBlock (ShelleyEra StandardCrypto)
    -> LedgerStateSnapshot -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertShelleyBlock = Shelley.insertShelleyBlock

insertAllegraBlock
    :: Trace IO Text -> DbSyncEnv -> ShelleyBlock (AllegraEra StandardCrypto)
    -> LedgerStateSnapshot -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertAllegraBlock trce _ _ _ _ = do
  liftIO $ logError trce "insertAllegraBlock: Not implemented yet."
  pure $ Right ()

insertMaryBlock
    :: Trace IO Text -> DbSyncEnv -> ShelleyBlock (MaryEra StandardCrypto)
    -> LedgerStateSnapshot -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertMaryBlock trce _ _ _ _ = do
  liftIO $ logError trce "insertMaryBlock: Not implemented yet."
  pure $ Right ()
