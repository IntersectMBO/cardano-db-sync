module Cardano.Db.Delete
  ( deleteCascadeBlock
  , deleteCascadeBlockNo
  , deleteCascadeSlotNo
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend, (==.), deleteCascade, selectList)
import           Database.Persist.Types (entityKey)

import           Cardano.Db.Schema

import           Ouroboros.Network.Block (BlockNo (..))


-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  keys <- selectList [ BlockHash ==. blockHash block ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlockNo :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteCascadeBlockNo (BlockNo blockNo) = do
  keys <- selectList [ BlockBlockNo ==. Just blockNo ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeSlotNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Bool
deleteCascadeSlotNo slotNo = do
  keys <- selectList [ BlockSlotNo ==. Just slotNo ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)

