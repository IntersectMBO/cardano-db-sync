{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Slotting as Byron

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Network.Block (Point, SlotNo (..))


rollbackToPoint :: Trace IO Text -> Point ByronBlock -> IO (Either DbSyncNodeError ())
rollbackToPoint trce point =
  case Byron.pointToSlotHash point of
    Nothing -> pure $ Right ()
    Just (slot, hash) ->
      DB.runDbNoLogging $ runExceptT (action slot hash)
  where
    action :: MonadIO m => Byron.SlotNumber -> Byron.HeaderHash -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        blk <- liftLookupFail "rollbackToPoint" $ DB.queryMainBlock (Byron.unHeaderHash hash)
        case DB.blockSlotNo blk of
          Nothing -> dbSyncNodeError "rollbackToPoint: slot number is Nothing"
          Just slotNo -> do
            if slotNo <= Byron.unSlotNumber slot
              then liftIO . logInfo trce $ mconcat
                            [ "No rollback required: db tip slot is ", textShow slotNo
                            , " ledger tip slot is ", textShow (Byron.unSlotNumber slot)
                            ]
              else do
                liftIO . logInfo trce $ Text.concat
                            [ "Rollbacking to slot ", textShow (Byron.unSlotNumber slot)
                            , ", hash ", Byron.renderAbstractHash hash
                            ]
                void . lift $ DB.deleteCascadeSlotNo slotNo

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())
unsafeRollback trce (SlotNo slotNo) = do
  logInfo trce $ "Forced rollback to slot " <> textShow slotNo
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
