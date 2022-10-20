{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Rollback
  ( rollbackToPoint
  , rollbackFromBlockNo
  , unsafeRollback
  ) where

import           Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import           Cardano.DbSync.Api
import           Cardano.DbSync.Cache
import           Cardano.DbSync.Era.Util
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util
import           Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import           Database.Persist.Sql (SqlBackend)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)
import           Ouroboros.Network.Block
import           Ouroboros.Network.Point


-- | The decision to delete blocks has been taken and this executes it.
deleteBlocks :: MonadIO m => SyncEnv -> BlockNo -> Bool -> Word64 -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
deleteBlocks env blkNo deleteEq nBlocks = do
    unless (nBlocks == 0) $
      liftIO . logInfo trce $
          mconcat
            [ "Deleting ", textShow nBlocks, " blocks "
            , if deleteEq then "starting from " else "after "
            , textShow blkNo
            ]
    -- We need to first cleanup the cache and then delete the blocks from db.
    lift $ rollbackCache cache blkNo deleteEq nBlocks
    deleted <- lift $ DB.deleteAfterBlockNo blkNo deleteEq
    liftIO . logInfo trce $
                if deleted
                  then "Blocks deleted"
                  else "No blocks need to be deleted"
  where
    trce :: Trace IO Text
    trce = getTrace env

    cache :: Cache
    cache = envCache env

rollbackFromBlockNo :: MonadIO m => SyncEnv -> BlockNo -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
rollbackFromBlockNo env blkNo = do
    nBlocks <- lift $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
    deleteBlocks env blkNo True (fromIntegral nBlocks)

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackToPoint :: SyncEnv -> CardanoPoint -> Tip CardanoBlock -> IO (Either SyncNodeError Bool)
rollbackToPoint env point serverTip = do
    backend <- getBackend env
    DB.runDbIohkNoLogging backend $ runExceptT action
  where
    trce :: Trace IO Text
    trce = getTrace env

    action :: MonadIO m => ExceptT SyncNodeError (ReaderT SqlBackend m) Bool
    action = do
        blkNo <- liftLookupFail "Rollback.rollbackToPoint.queryBlockNo" $ queryBlockNo point
        nBlocks <- lift $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) False
        if nBlocks <= 50 || not (hasLedgerState env) then do
          liftIO . logInfo trce $ "Rolling back to " <> renderPoint point
          deleteBlocks env blkNo False (fromIntegral nBlocks)
          pure True
        else do
          liftIO . logInfo trce $
            mconcat
              [ "Delaying delete of ", textShow nBlocks, " blocks after "
              , textShow blkNo, " while rolling back to (" , renderPoint point
              , "). Applying blocks until a new block is found. The node is currently at ", textShow serverTip
              ]
          pure False

    queryBlockNo :: MonadIO m => Point CardanoBlock -> ReaderT SqlBackend m (Either DB.LookupFail BlockNo)
    queryBlockNo pnt =
      case getPoint pnt of
        Origin -> pure $ Right 0
        At blk -> DB.queryBlockHashBlockNo (SBS.fromShort . getOneEraHash $ blockPointHash blk)

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce config slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteCascadeSlotNo slotNo)
