module Cardano.DbSync.Cache.Util where

import Cardano.DbSync.Cache.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO)

withCacheOptimisationCheck ::
  MonadIO m =>
  CacheInternal ->
  m a -> -- Action to perform if cache is optimised
  m a -> -- Action to perform if cache is not optimised
  m a
withCacheOptimisationCheck ci ifOptimised ifNotOptimised = do
  isCachedOptimised <- liftIO $ readTVarIO (cIsCacheOptimised ci)
  if isCachedOptimised
    then ifOptimised
    else ifNotOptimised
