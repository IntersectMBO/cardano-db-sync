{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Explorer.Node.Plugin.Default.Rollback
  ( rollbackToPoint
  ) where

import           Cardano.BM.Trace (Trace, logInfo)

import           Cardano.Prelude

import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Slotting as Ledger

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Explorer.DB as DB
import           Explorer.Node.Error
import           Explorer.Node.Util

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import           Ouroboros.Network.Block (Point)


rollbackToPoint :: Trace IO Text -> Point ByronBlock -> IO (Either ExplorerNodeError ())
rollbackToPoint trce point =
  case pointToSlotHash point of
    Nothing -> pure $ Right ()
    Just (slot, hash) ->
      DB.runDbNoLogging $ runExceptT (action slot hash)
  where
    action :: MonadIO m => Ledger.SlotNumber -> Ledger.HeaderHash -> ExceptT ExplorerNodeError (ReaderT SqlBackend m) ()
    action slot hash = do
        blk <- liftLookupFail "rollbackToPoint" $ DB.queryMainBlock (unHeaderHash hash)
        case (DB.blockSlotNo blk, DB.blockBlockNo blk) of
          (Nothing, _) -> explorerError "rollbackToPoint: slot number is Nothing"
          (_, Nothing) -> explorerError "rollbackToPoint: block number is Nothing"
          (Just slotNo, Just blkNo) -> do
            if slotNo <= Ledger.unSlotNumber slot
              then liftIO . logInfo trce $ mconcat
                            [ "No rollback required: chain tip slot is ", textShow slotNo ]
              else do
                liftIO . logInfo trce $ Text.concat
                            [ "Rollbacking to slot ", textShow (Ledger.unSlotNumber slot)
                            , ", hash ", renderAbstractHash hash
                            ]
                unless (Just (Ledger.unSlotNumber slot) == DB.blockSlotNo blk) $
                  explorerError ("rollbackToPoint: slot mismatch " <> textShow (slotNo, blkNo))
                -- This will be a cascading delete.
                void . lift $ DB.deleteCascadeBlockNo blkNo
