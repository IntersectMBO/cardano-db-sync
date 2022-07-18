{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Cardano.Prelude
import qualified Data.ByteString.Short as SBS

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Api
import           Cardano.DbSync.Cache
import           Cardano.DbSync.Era.Util
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import qualified Data.List as List
import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackToPoint :: SyncEnv -> CardanoPoint -> IO (Either SyncNodeError ())
rollbackToPoint env point = do
    backend <- getBackend env
    DB.runDbIohkNoLogging backend $ runExceptT action
  where
    trce :: Trace IO Text
    trce = getTrace env

    cache :: Cache
    cache = envCache env

    action :: MonadIO m => ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    action = do
        liftIO . logInfo trce $ "Rolling back to " <> renderPoint point
        xs <- lift $ slotsToDelete (pointSlot point)
        unless (null xs) $
          -- there may be more deleted blocks than slots, because ebbs don't have
          -- a slot. We can only make an estimation here.
          liftIO . logInfo trce $
              mconcat
                [ "Deleting ", textShow (length xs), " blocks up to slot "
                , textShow (unSlotNo $ List.head xs)
                ]
        -- We delete the block right after the point we rollback to. This delete
        -- should cascade to the rest of the chain.
        blkNo <- liftLookupFail "Rollback.rollbackToPoint" $ queryBlockNo point
        -- 'length xs' here gives an approximation of the blocks deleted. An approximation
        -- is good enough, since it is only used to decide on the best policy and is not
        -- important for correctness.
        lift $ rollbackCache cache blkNo (fromIntegral $ length xs)
        deleted <- lift $ DB.deleteAfterBlockNo blkNo
        liftIO . logInfo trce $
                    if deleted
                      then "Blocks deleted"
                      else "No blocks need to be deleted"

    slotsToDelete :: MonadIO m => WithOrigin SlotNo -> ReaderT SqlBackend m [SlotNo]
    slotsToDelete wosl =
      case wosl of
        Origin -> DB.querySlotNos
        At sl -> DB.querySlotNosGreaterThan (unSlotNo sl)

    queryBlockNo :: MonadIO m => Point CardanoBlock -> ReaderT SqlBackend m (Either DB.LookupFail BlockNo)
    queryBlockNo pnt =
      case getPoint pnt of
        Origin -> pure $ Right 0
        At blk -> DB.queryBlockHash (SBS.fromShort . getOneEraHash $ blockPointHash blk)

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce config slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteCascadeSlotNo slotNo)
