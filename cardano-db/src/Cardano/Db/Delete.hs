module Cardano.Db.Delete
  ( deleteCascadeBlock
  , deleteCascadeBlockNo
  , deleteCascadeSlotNo
  ) where


import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend, (==.), deleteCascade, selectKeysList)

import           Cardano.Db.Schema

import           Ouroboros.Network.Block (BlockNo (..))


-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  keys <- selectKeysList [ BlockHash ==. blockHash block ] []
  mapM_ deleteCascade keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlockNo :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteCascadeBlockNo (BlockNo blockNo) = do
  keys <- selectKeysList [ BlockBlockNo ==. Just blockNo ] []
  mapM_ deleteCascade keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Bool
deleteCascadeSlotNo slotNo = do
  keys <- selectKeysList [ BlockSlotNo ==. Just slotNo ] []
  liftIO $ print keys
  mapM_ deleteCascade keys
  pure $ not (null keys)

