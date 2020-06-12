{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import           Data.Text (Text)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Plugin.Default.Byron.Rollback as Byron
import qualified Cardano.DbSync.Plugin.Default.Shelley.Rollback as Shelley
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))


rollbackToPoint :: Trace IO Text -> CardanoPoint -> IO (Either DbSyncNodeError ())
rollbackToPoint trce cpnt =
  case cpnt of
    ByronPoint point ->
      Byron.rollbackToPoint trce point
    ShelleyPoint point ->
      Shelley.rollbackToPoint trce point

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
unsafeRollback trce (SlotNo slotNo) = do
  logInfo trce $ "Forced rollback to slot " <> textShow slotNo
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
