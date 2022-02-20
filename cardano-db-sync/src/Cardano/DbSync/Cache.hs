module Cardano.DbSync.Cache
    ( Cache
    , newEmptyCache
    , queryMAWithCache
    ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Reader
import           Data.ByteString.Internal (ByteString)
import           Data.Map.Strict
import qualified Data.Map.Strict as Map
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)

import           Cardano.Ledger.Mary.Value (AssetName (..))

import qualified Cardano.Db as DB

import           Database.Persist.Postgresql (SqlBackend)

newtype Cache = MACache (IORef (Map (ByteString, AssetName) DB.MultiAssetId))

newEmptyCache :: MonadIO m => m Cache
newEmptyCache = liftIO $ MACache <$> newIORef Map.empty

queryMAWithCache :: MonadIO m => Cache -> ByteString -> AssetName
                 -> ReaderT SqlBackend m (Maybe DB.MultiAssetId)
queryMAWithCache (MACache ref) policyId a@(AssetName aName) = do
    mp <- liftIO $ readIORef ref 
    case Map.lookup (policyId, a) mp of
      Just maId -> pure $ Just maId
      Nothing -> do
        maId <- DB.queryMultiAssetId policyId aName
        case maId of
          Nothing -> pure Nothing
          Just mId -> do
            liftIO $ modifyIORef ref $ Map.insert (policyId, a) mId
            pure maId 
