{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Explorer.Node.Rollback
  ( rollbackToPoint
  ) where

import           Cardano.BM.Trace (Trace, logInfo)

import           Cardano.Prelude

import qualified Cardano.Chain.Slotting as Ledger

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Explorer.DB as DB
import           Explorer.Node.Util

import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB (..))
import           Ouroboros.Network.Block (Point)


rollbackToPoint :: Trace IO Text -> Point (ByronBlockOrEBB cfg) -> IO ()
rollbackToPoint trce point =
  case pointToSlotHash point of
    Nothing -> pure ()
    Just (slot, hash) -> do
      DB.runDbNoLogging $ do
        blk <- leftPanic "rollbackToPoint: " <$> DB.queryMainBlock (unHeaderHash hash)
        case (DB.blockSlotNo blk, DB.blockBlockNo blk) of
          (Nothing, _) -> panic "rollbackToPoint: slot number is Nothing"
          (_, Nothing) -> panic "rollbackToPoint: block number is Nothing"
          (Just slotNo, Just blkNo) -> do
            if slotNo <= Ledger.unSlotNumber slot
              then liftIO $ logInfo trce "No rollback required"
              else do
                liftIO . logInfo trce $ Text.concat
                            [ "Rollbacking to slot ", textShow (Ledger.unSlotNumber slot)
                            , ", hash ", renderAbstractHash hash
                            ]
                unless (Just (Ledger.unSlotNumber slot) == DB.blockSlotNo blk) $
                  panic $ "rollbackToPoint: slot mismatch " <> textShow (slotNo, blkNo)
                -- This will be a cascading delete.
                void $ DB.deleteCascadeBlockNo blkNo
