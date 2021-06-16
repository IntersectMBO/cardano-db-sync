{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.DbSync.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Cardano.Prelude
import qualified Data.ByteString.Short as BSS

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Era.Util

import           Cardano.Sync.Error
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackToPoint :: SqlBackend -> Trace IO Text -> CardanoPoint -> IO (Either SyncNodeError ())
rollbackToPoint backend trce point =
    DB.runDbIohkNoLogging backend $ runExceptT action
  where
    action :: MonadIO m => ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    action = do
        liftIO $ logInfo trce msg
        xs <- lift $ slotsToDelete (pointSlot point)
        if null xs then
          liftIO $ logInfo trce "No Rollback is necessary"
        else do
          liftIO . logInfo trce $
              mconcat
                [ "Deleting ", textShow (length xs), " slots: ", renderSlotList xs
                ]
          mapM_ (lift . DB.deleteCascadeSlotNo) xs
          liftIO $ logInfo trce "Slots deleted"
        -- Deleting by slot does not guarantee that all blocks will be deleted,
        -- because EBBS don't have a slot. In practice most EBBS are still
        -- deleted because their previous block is deleted and deletes are cascaded.
        -- However the previous block won't be deleted if it's the point we
        -- actually roll back to. To catch this case we need to manually delete
        -- the block right after the block we roll back to.
        prevId <- liftLookupFail "rollbackToPoint" $ rollbackToId point
        void $ lift $ DB.deleteCascadeAfter prevId

    slotsToDelete Origin = DB.querySlotNos
    slotsToDelete (At sl) = DB.querySlotNosGreaterThan (unSlotNo sl)

    rollbackToId pnt = case getPoint pnt of
      Origin -> DB.queryGenesis
      At blk -> DB.queryBlockId (BSS.fromShort $ getOneEraHash $ blockPointHash blk)

    msg :: Text
    msg = case getPoint point of
            Origin -> "Rolling back to genesis"
            At blk -> mconcat
                  [ "Rolling back to "
                  , textShow (unSlotNo $ blockPointSlot blk)
                  , ", hash "
                  , renderByteArray $ toRawHash (Proxy @CardanoBlock) $ blockPointHash blk
                  ]

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
