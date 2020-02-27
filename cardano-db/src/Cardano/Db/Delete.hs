module Cardano.Db.Delete
  ( deleteCascadeBlock
  , deleteCascadeBlockNo
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend, (==.), deleteCascade, selectList)
import           Database.Persist.Types (entityKey)

import           Cardano.Db.Schema


-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  keys <- selectList [ BlockHash ==. blockHash block ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlockNo :: MonadIO m => Word64 -> ReaderT SqlBackend m Bool
deleteCascadeBlockNo blkNo = do
  keys <- selectList [ BlockBlockNo ==. Just blkNo ] []
  mapM_ (deleteCascade . entityKey) keys
  pure $ not (null keys)
