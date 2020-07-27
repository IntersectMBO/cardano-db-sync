{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Rollback
  ( rollbackToSlot
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Data.Text (Text)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Network.Block (BlockNo (..))

-- Rollbacks are done in an Era generic way based just on the SlotNo we are
-- rolling back to.
rollbackToSlot :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
rollbackToSlot trce slotNo =
    DB.runDbNoLogging $ runExceptT (action slotNo)
  where
    action :: MonadIO m => SlotNo -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action (SlotNo slot) = do
        mHash <- lift $ DB.querySlotHash slot
        liftIO . logInfo trce $
            mconcat
              [ "Rolling back to slot ", textShow slot, ", hash "
              , maybe (if slot == 0 then "genesis" else "unknown") renderByteArray mHash
              ]
        xs <- lift $ DB.queryBlockNosWithSlotNoGreater slot
        liftIO . logInfo trce $
            mconcat
              [ "Deleting blocks numbered: ", textShow (map unBlockNo xs)
              ]
        mapM_ (void . lift . DB.deleteCascadeBlockNo) xs


-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
unsafeRollback trce (SlotNo slotNo) = do
  logInfo trce $ "Forced rollback to slot " <> textShow slotNo
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
