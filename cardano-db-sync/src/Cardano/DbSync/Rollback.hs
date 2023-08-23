{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Rollback (
  prepareRollback,
  rollbackFromBlockNo,
  unsafeRollback,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude
import Control.Monad.Extra (whenJust)
import qualified Data.ByteString.Short as SBS
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)
import Ouroboros.Network.Block
import Ouroboros.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackFromBlockNo ::
  MonadIO m =>
  SyncEnv ->
  BlockNo ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
rollbackFromBlockNo syncEnv blkNo = do
  nBlocks <- lift $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
  mres <- lift $ DB.queryBlockNoAndEpoch (unBlockNo blkNo)
  whenJust mres $ \(blockId, epochNo) -> do
    liftIO . logInfo trce $
      mconcat
        [ "Deleting "
        , textShow nBlocks
        , " numbered equal to or greater than "
        , textShow blkNo
        ]
    lift $ do
      (minIds, txInDeleted) <- DB.deleteBlocksBlockId trce blockId
      whenConsumeOrPruneTxOut syncEnv $
        DB.setNullTxOut trce (DB.minTxInId minIds) txInDeleted
      DB.deleteEpochRows epochNo
    lift $ rollbackCache cache blockId

    liftIO . logInfo trce $ "Blocks deleted"
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv

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
              liftIO . logInfo trce $
                mconcat
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
          liftIO . logInfo trce $
            mconcat
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
unsafeRollback :: Trace IO Text -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce config slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteBlocksSlotNo trce slotNo)
