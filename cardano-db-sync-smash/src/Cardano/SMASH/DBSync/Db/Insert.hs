
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SMASH.DBSync.Db.Insert
  ( insertPool
  , insertPoolMetadata
  , insertPoolMetadataRef
  , insertReservedTicker
  , insertDelistedPool
  , insertRetiredPool
  , insertAdminUser
  , insertPoolMetadataFetchError

  -- Export mainly for testing.
  , insertByReturnKey
  ) where

import           Cardano.Prelude hiding (Meta, replace)

import           Control.Monad.Trans.Reader (mapReaderT)

import           Database.Persist.Class (AtLeastOneUniqueKey, PersistEntityBackend, checkUnique,
                   getByValue, insert)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (entityKey)
import           Database.PostgreSQL.Simple (SqlError)

import           Cardano.Db
import           Cardano.SMASH.Db.Error

insertPool :: (MonadIO m) => Pool -> ReaderT SqlBackend m (Either DBFail PoolId)
insertPool = insertByReturnKey

insertPoolMetadata :: (MonadIO m) => PoolMetadata -> ReaderT SqlBackend m (Either DBFail PoolMetadataId)
insertPoolMetadata = insertByReturnKey

insertPoolMetadataRef
    :: MonadIO m
    => PoolMetadataRef
    -> ReaderT SqlBackend m (Either DBFail PoolMetadataRefId)
insertPoolMetadataRef = insertByReturnKey

insertReservedTicker :: (MonadIO m) => ReservedTicker -> ReaderT SqlBackend m (Either DBFail ReservedTickerId)
insertReservedTicker reservedTicker = do
    isUnique <- checkUnique reservedTicker
    -- If there is no unique constraint violated, insert, otherwise return error.
    case isUnique of
        Nothing -> insertByReturnKey reservedTicker
        Just _key -> return . Left . ReservedTickerAlreadyInserted $ show reservedTicker

insertDelistedPool :: (MonadIO m) => DelistedPool -> ReaderT SqlBackend m (Either DBFail DelistedPoolId)
insertDelistedPool delistedPool = do
    isUnique <- checkUnique delistedPool
    -- If there is no unique constraint violated, insert, otherwise return error.
    case isUnique of
        Nothing -> insertByReturnKey delistedPool
        Just _key -> return . Left . DbInsertError $ "Delisted pool already exists!"

insertRetiredPool :: (MonadIO m) => RetiredPool -> ReaderT SqlBackend m (Either DBFail RetiredPoolId)
insertRetiredPool = insertByReturnKey

insertAdminUser :: (MonadIO m) => AdminUser -> ReaderT SqlBackend m (Either DBFail AdminUserId)
insertAdminUser adminUser = do
    isUnique <- checkUnique adminUser
    -- If there is no unique constraint violated, insert, otherwise return error.
    case isUnique of
        Nothing -> insertByReturnKey adminUser
        Just _key -> return . Left . DbInsertError $ "Admin user already exists!"

insertPoolMetadataFetchError
    :: (MonadIO m)
    => PoolMetadataFetchError
    -> ReaderT SqlBackend m (Either DBFail PoolMetadataFetchErrorId)
insertPoolMetadataFetchError pmfe = do
    isUnique <- checkUnique pmfe
    -- If there is no unique constraint violated, insert, otherwise delete and insert.
    case isUnique of
        Nothing -> insertByReturnKey pmfe
        Just _key -> return . Left . DbInsertError $ "Pool metadata fetch error already exists!"

-------------------------------------------------------------------------------

-- | Insert a record (with a Unique constraint), and return 'Right key' if the
-- record is inserted and 'Left key' if the record already exists in the DB.
-- TODO(KS): This needs to be tested, not sure if it's actually working.
insertByReturnKey
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => record -> ReaderT SqlBackend m (Either DBFail (Key record))
insertByReturnKey value = do
    res <- getByValue value
    case res of
        -- handle :: Exception e => (e -> IO a) -> IO a -> IO a
        Nothing -> mapReaderT (liftIO . handle exceptionHandler) (Right <$> insert value)
        Just r  -> pure . Right $ entityKey r
  where
    exceptionHandler :: MonadIO m => SqlError -> m (Either DBFail a)
    exceptionHandler e =
        liftIO . pure . Left . DbInsertError . show $ e
