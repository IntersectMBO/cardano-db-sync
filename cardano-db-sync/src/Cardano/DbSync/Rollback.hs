{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Rollback
  ( rollbackToSlot
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logError, logInfo)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Database.Persist.Sql (SqlBackend)

-- Rollbacks are done in an Era generic way based just on the SlotNo we are
-- rolling back to.
rollbackToSlot :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
rollbackToSlot trce slotNo =
    DB.runDbNoLogging $ runExceptT action
  where
    action :: MonadIO m => ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action = do
        mHash <- fmap snd <$> lift (DB.querySlotHash slotNo)
        liftIO . logInfo trce $
            mconcat
              [ "Rolling back to slot ", textShow (unSlotNo slotNo), ", hash "
              , maybe (if unSlotNo slotNo == 0 then "genesis" else "unknown") renderByteArray mHash
              ]
        count <- lift $ DB.queryBlocksAfterSlot (unSlotNo slotNo)
        when (count > 50000) .
          liftIO . logError trce $
            mconcat
              [ "Rollback block count is high and therefore rollback is likely to be incredibly slow. "
              , "Dropping the database and resyncing from scratch will be faster."
              ]
        xs <- lift $ DB.querySlotNosGreaterThan (unSlotNo slotNo)
        liftIO . logInfo trce $
            mconcat
              [ "Deleting slots numbered: ", renderSlotList xs
              ]
        mapM_ (lift . DB.deleteCascadeSlotNo) xs
        liftIO $ logInfo trce "Slots deleted"


-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
unsafeRollback trce slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow slotNo
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
