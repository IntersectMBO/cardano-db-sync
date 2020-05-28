{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default.Rollback
  ( rollbackToPoint
  , unsafeRollback
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Slotting as Ledger

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Network.Block (Point)


rollbackToPoint :: Trace IO Text -> Point ByronBlock -> IO (Either DbSyncNodeError ())
rollbackToPoint trce point =
  case pointToSlotHash point of
    Nothing -> pure $ Right ()
    Just (slot, hash) ->
      DB.runDbNoLogging $ runExceptT (action slot hash)
  where
    action :: MonadIO m => Ledger.SlotNumber -> Ledger.HeaderHash -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        blk <- liftLookupFail "rollbackToPoint" $ DB.queryMainBlock (unHeaderHash hash)
        case DB.blockSlotNo blk of
          Nothing -> dbSyncNodeError "rollbackToPoint: slot number is Nothing"
          Just slotNo -> do
            if slotNo <= Ledger.unSlotNumber slot
              then liftIO . logInfo trce $ mconcat
                            [ "No rollback required: db tip slot is ", textShow slotNo
                            , " ledger tip slot is ", textShow (Ledger.unSlotNumber slot)
                            ]
              else do
                liftIO . logInfo trce $ Text.concat
                            [ "Rollbacking to slot ", textShow (Ledger.unSlotNumber slot)
                            , ", hash ", renderAbstractHash hash
                            ]
                void . lift $ DB.deleteCascadeSlotNo slotNo

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> Word64 -> IO (Either DbSyncNodeError ())
unsafeRollback _trce slotNo =
  Right <$> DB.runDbNoLogging (void $ DB.deleteCascadeSlotNo slotNo)
