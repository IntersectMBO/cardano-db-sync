{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Plugin.Shelley.Rollback
  ( rollbackToPoint
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)
import           Cardano.Crypto.Hash.Class as Crypto

import qualified Cardano.Chain.Slotting as Ledger
import           Cardano.Slotting.Slot (WithOrigin (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)

import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util (textShow, renderByteArray)

import           Shelley.Spec.Ledger.BlockChain (HashHeader (..))
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..), unShelleyHash)
import           Ouroboros.Network.Block (Point (..), unSlotNo)
import qualified Ouroboros.Network.Point as Point


rollbackToPoint
    :: Trace IO Text
    -> Point (ShelleyBlock crypto)
    -> IO (Either DbSyncNodeError ())
rollbackToPoint trce point =
  case pointToSlotHash point of
    Nothing -> pure $ Right ()
    Just (slot, hash') ->
      DB.runDbNoLogging $ runExceptT (action slot hash')
  where
    action :: MonadIO m => Ledger.SlotNumber -> ByteString -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
    action slot hash' = do
        blk <- liftLookupFail "rollbackToPoint" $ DB.queryMainBlock hash'
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
                            , ", hash ", renderByteArray hash'
                            ]
                void . lift $ DB.deleteCascadeSlotNo slotNo

-- | Convert from Ouroboros 'Point' to 'Ledger' types.
-- Thanks to Thomas, here we can drop to @ByteString@ since the underlying hashes use the same algorithm.
pointToSlotHash :: Point (ShelleyBlock crypto) -> Maybe (Ledger.SlotNumber, ByteString)
pointToSlotHash (Point x) =
  case x of
    Origin -> Nothing
    At blk -> Just (Ledger.SlotNumber . unSlotNo $ Point.blockPointSlot blk, Crypto.getHash . unHashHeader . unShelleyHash $ Point.blockPointHash blk)

