{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Shelley.Rollback
  ( rollbackToPoint
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)
import           Cardano.Chain.Slotting (SlotNumber (..))

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Network.Block (Point (..))


rollbackToPoint :: Trace IO Text -> Point ShelleyBlock -> IO (Either DbSyncNodeError ())
rollbackToPoint trce point =
    case Shelley.pointToSlotHash point of
      Nothing -> pure $ Right ()
      Just (slot, hash) ->
          DB.runDbNoLogging $ runExceptT (action slot hash)
  where
    action :: MonadIO m => SlotNumber -> ShelleyHash -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        blk <- liftLookupFail "rollbackToPoint" $ DB.queryMainBlock (Shelley.unHeaderHash hash)
        case DB.blockSlotNo blk of
          Nothing -> dbSyncNodeError "rollbackToPoint: slot number is Nothing"
          Just slotNo -> do
            if slotNo <= unSlotNumber slot
              then liftIO . logInfo trce $ mconcat
                            [ "Shelley: No rollback required: db tip slot is ", textShow slotNo
                            , " ledger tip slot is ", textShow (unSlotNumber slot)
                            ]
              else do
                liftIO . logInfo trce $ Text.concat
                            [ "Rollbacking to slot ", textShow (unSlotNumber slot)
                            , ", hash ", Shelley.renderHash hash
                            ]
                void . lift $ DB.deleteCascadeSlotNo slotNo
