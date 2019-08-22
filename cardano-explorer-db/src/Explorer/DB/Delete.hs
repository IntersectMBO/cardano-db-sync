{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.DB.Delete
  ( deleteBlock
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend, (==.), delete, selectList)
import           Database.Persist.Types (entityKey)

import           Explorer.DB.Schema


-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteBlock block = do
  keys <- selectList [ BlockHash ==. blockHash block ] []
  mapM_ (delete. entityKey) keys
  pure $ not (null keys)
