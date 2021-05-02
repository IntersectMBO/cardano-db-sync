{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SMASH.DBSync.Db.Delete
  ( deleteDelistedPool
  , deleteRetiredPool
  , deleteAdminUser
  ) where

import           Cardano.Prelude hiding (Meta)

import           Database.Persist.Sql (SqlBackend, delete, selectKeysList, (==.))

import           Cardano.Db

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => PoolIdentifier -> ReaderT SqlBackend m Bool
deleteDelistedPool poolId = do
  keys <- selectKeysList [ DelistedPoolPoolId ==. poolId ] []
  mapM_ delete keys
  pure $ not (null keys)

-- | Delete a retired pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteRetiredPool :: MonadIO m => PoolIdentifier -> ReaderT SqlBackend m Bool
deleteRetiredPool poolId = do
  keys <- selectKeysList [ RetiredPoolPoolId ==. poolId ] []
  mapM_ delete keys
  pure $ not (null keys)

deleteAdminUser :: MonadIO m => AdminUser -> ReaderT SqlBackend m Bool
deleteAdminUser adminUser = do
  keys <- selectKeysList [ AdminUserUsername ==. adminUserUsername adminUser, AdminUserPassword ==. adminUserPassword adminUser ] []
  mapM_ delete keys
  pure $ not (null keys)

