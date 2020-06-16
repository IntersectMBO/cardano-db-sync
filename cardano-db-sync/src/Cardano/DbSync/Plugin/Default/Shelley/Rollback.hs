{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Shelley.Rollback
  ( rollbackToPoint
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Network.Block (BlockNo (..), Point (..))


rollbackToPoint :: Trace IO Text -> Point ShelleyBlock -> IO (Either DbSyncNodeError ())
rollbackToPoint trce point =
    case Shelley.pointToSlotHash point of
      Nothing -> pure $ Right ()
      Just (slot, hash) ->
          DB.runDbNoLogging $ runExceptT (action slot hash)
  where
    action :: MonadIO m => SlotNo -> ShelleyHash -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        liftIO . logInfo trce $
            mconcat
              [ "Shelley: Rolling back to slot ", textShow (unSlotNo slot)
              , ", hash ", Shelley.renderHash hash
              ]
        xs <- lift $ DB.queryBlockNosWithSlotNoGreater (unSlotNo slot)
        liftIO . logInfo trce $
            mconcat
              [ "Shelley: Deleting blocks numbered: ", textShow (map unBlockNo xs)
              ]
        mapM_ (void . lift . DB.deleteCascadeBlockNo) xs
