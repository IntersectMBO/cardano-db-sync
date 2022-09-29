module Cardano.Db.Delete
  ( deleteCascadeBlock
  , deleteCascadeAfter
  , deleteCascadeSlotNo
  , deleteDelistedPool
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend, delete, selectKeysList, (!=.), (==.))

import           Data.ByteString (ByteString)

import           Cardano.Db.Schema

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  keys <- selectKeysList [ BlockHash ==. blockHash block ] []
  mapM_ delete keys
  pure $ not (null keys)

-- | Delete a block after the specified 'BlockId'. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeAfter :: MonadIO m => BlockId -> Bool -> ReaderT SqlBackend m Bool
deleteCascadeAfter bid deleteEq = do
  -- Genesis artificial blocks are not deleted (Byron or Shelley) since they have null epoch
  keys <- 
    if deleteEq 
      then selectKeysList [ BlockId ==. bid, BlockEpochNo !=. Nothing ] []
      else selectKeysList [ BlockPreviousId ==. Just bid, BlockEpochNo !=. Nothing ] []
  mapM_ delete keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeSlotNo :: MonadIO m => SlotNo -> ReaderT SqlBackend m Bool
deleteCascadeSlotNo (SlotNo slotNo) = do
  keys <- selectKeysList [ BlockSlotNo ==. Just slotNo ] []
  mapM_ delete keys
  pure $ not (null keys)

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
deleteDelistedPool poolHash = do
  keys <- selectKeysList [ DelistedPoolHashRaw ==. poolHash ] []
  mapM_ delete keys
  pure $ not (null keys)
