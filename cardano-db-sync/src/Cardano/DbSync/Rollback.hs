{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Rollback (
  prepareRollback,
  rollbackFromBlockNo,
  unsafeRollback,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)
import Cardano.Prelude
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Short as SBS
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)
import Ouroboros.Network.Block
import Ouroboros.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackFromBlockNo ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  BlockNo ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
rollbackFromBlockNo syncEnv blkNo = do
  nBlocks <- lift $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
  mres <- lift $ DB.queryBlockNoAndEpoch (unBlockNo blkNo)
  whenJust mres $ \(blockId, epochNo) -> do
    liftIO
      . logInfo trce
      $ mconcat
        [ "Deleting "
        , textShow nBlocks
        , " numbered equal to or greater than "
        , textShow blkNo
        ]
    lift $ do
      deletedBlockCount <- DB.deleteBlocksBlockId trce txOutTableType blockId epochNo (Just (DB.pcmConsumedTxOut $ getPruneConsume syncEnv))
      when (deletedBlockCount > 0) $ do
        -- We use custom constraints to improve input speeds when syncing.
        -- If they don't already exists we add them here as once a rollback has happened
        -- we always need a the constraints.
        addConstraintsIfNotExist syncEnv trce

    lift $ rollbackCache cache blockId

    liftIO . logInfo trce $ "Blocks deleted"
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv
    txOutTableType = getTxOutTableType syncEnv

prepareRollback :: SyncEnv -> CardanoPoint -> Tip CardanoBlock -> IO (Either SyncNodeError Bool)
prepareRollback syncEnv point serverTip =
  DB.runDbIohkNoLogging (envBackend syncEnv) $ runExceptT action
  where
    trce = getTrace syncEnv

    action :: MonadIO m => ExceptT SyncNodeError (ReaderT SqlBackend m) Bool
    action = do
      case getPoint point of
        Origin -> do
          nBlocks <- lift DB.queryCountSlotNo
          if nBlocks == 0
            then do
              liftIO . logInfo trce $ "Starting from Genesis"
            else do
              liftIO
                . logInfo trce
                $ mconcat
                  [ "Delaying delete of "
                  , textShow nBlocks
                  , " while rolling back to genesis."
                  , " Applying blocks until a new block is found."
                  , " The node is currently at "
                  , textShow serverTip
                  ]
        At blk -> do
          nBlocks <- lift $ DB.queryCountSlotNosGreaterThan (unSlotNo $ blockPointSlot blk)
          mBlockNo <-
            liftLookupFail "Rollback.prepareRollback" $
              DB.queryBlockHashBlockNo (SBS.fromShort . getOneEraHash $ blockPointHash blk)
          liftIO
            . logInfo trce
            $ mconcat
              [ "Delaying delete of "
              , textShow nBlocks
              , " blocks after "
              , textShow mBlockNo
              , " while rolling back to ("
              , renderPoint point
              , "). Applying blocks until a new block is found. The node is currently at "
              , textShow serverTip
              ]
      pure False

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.TxOutTableType -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce txOutTableType config slotNo = do
  logWarning trce $ "Starting a forced rollback to slot: " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteBlocksSlotNo trce txOutTableType slotNo Nothing)
