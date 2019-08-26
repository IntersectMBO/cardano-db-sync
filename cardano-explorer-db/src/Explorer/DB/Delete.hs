module Explorer.DB.Delete
  ( deleteCascadeBlock
  , deleteCascadeBlockId
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend, (==.), deleteCascade, selectList)
import           Database.Persist.Types (entityKey)

import           Explorer.DB.Schema


-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  keys <- selectList [ BlockHash ==. blockHash block ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlockId :: MonadIO m => BlockId -> ReaderT SqlBackend m Bool
deleteCascadeBlockId blkId = do
  keys <- selectList [ BlockId ==. blkId ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)
