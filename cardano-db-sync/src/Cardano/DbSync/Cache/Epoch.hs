{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.Epoch (
  readCacheEpoch,
  readEpochBlockDiffFromCache,
  readLastMapEpochFromCache,
  rollbackMapEpochInCache,
  writeEpochBlockDiffToCache,
  writeToMapEpochCache,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, HasLedgerEnv (..), LedgerEnv (..), NoLedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Cache.Types (CacheEpoch (..), CacheInternal (..), CacheStatus (..), EpochBlockDiff (..))
import Cardano.DbSync.Era.Shelley.Generic.StakeDist (getSecurityParameter)
import Cardano.DbSync.Error.Types (SyncNodeError (..))
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Data.Map.Strict (deleteMin, insert, lookupMax, size, split)

-------------------------------------------------------------------------------------
-- Epoch Cache
-------------------------------------------------------------------------------------
readCacheEpoch :: CacheStatus -> App (Maybe CacheEpoch)
readCacheEpoch cache =
  case cache of
    NoCache -> pure Nothing
    ActiveCache ci -> do
      cacheEpoch <- liftIO $ readTVarIO (cEpoch ci)
      pure $ Just cacheEpoch

readEpochBlockDiffFromCache :: CacheStatus -> App (Maybe EpochBlockDiff)
readEpochBlockDiffFromCache cache =
  case cache of
    NoCache -> pure Nothing
    ActiveCache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochBlockDiff cE) of
        (_, epochInternal) -> pure epochInternal

readLastMapEpochFromCache :: CacheStatus -> App (Maybe DB.Epoch)
readLastMapEpochFromCache cache =
  case cache of
    NoCache -> pure Nothing
    ActiveCache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      let mapEpoch = ceMapEpoch cE
      -- making sure db sync wasn't restarted on the last block in epoch
      if length mapEpoch == 1
        then pure Nothing
        else case lookupMax mapEpoch of
          Nothing -> pure Nothing
          Just (_, ep) -> pure $ Just ep

rollbackMapEpochInCache :: CacheInternal -> DB.BlockId -> App (Either SyncNodeError ())
rollbackMapEpochInCache cache blockId = do
  cE <- liftIO $ readTVarIO (cEpoch cache)
  -- split the map and delete anything after blockId including it self as new blockId might be
  -- given when inserting the block again when doing rollbacks.
  let (newMapEpoch, _) = split blockId (ceMapEpoch cE)
  writeToCache cache (CacheEpoch newMapEpoch (ceEpochBlockDiff cE))

writeEpochBlockDiffToCache ::
  EpochBlockDiff ->
  App (Either SyncNodeError ())
writeEpochBlockDiffToCache epCurrent = do
  cache <- asks envCache
  case cache of
    NoCache -> pure $ Left $ SNErrDefault "writeEpochBlockDiffToCache: CacheStatus is NoCache"
    ActiveCache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochBlockDiff cE) of
        (epochLatest, _) -> writeToCache ci (CacheEpoch epochLatest (Just epCurrent))

-- | Insert an epoch into Map Epoch cache. A blockId is used as the key which was generated when inserting the block
-- | into the db. This is so we have a historic representation of an epoch after every block is inserted.
-- | This becomes usefull when syncing and doing rollbacks and saves on expensive db queries to calculte an epoch.
writeToMapEpochCache ::
  CacheStatus ->
  DB.Epoch ->
  App (Either SyncNodeError ())
writeToMapEpochCache cache latestEpoch = do
  SyncEnv {..} <- ask
  -- this can also be tought of as max rollback number
  let securityParam =
        case envLedgerEnv of
          HasLedger hle -> getSecurityParameter $ leProtocolInfo hle
          NoLedger nle -> getSecurityParameter $ nleProtocolInfo nle
  case cache of
    NoCache -> pure $ Left $ SNErrDefault "writeToMapEpochCache: CacheStatus is NoCache"
    ActiveCache ci -> do
      -- get EpochBlockDiff so we can use the BlockId we stored when inserting blocks
      epochInternalCE <- readEpochBlockDiffFromCache cache
      case epochInternalCE of
        Nothing -> pure $ Left $ SNErrDefault "writeToMapEpochCache: No epochInternalEpochCache"
        Just ei -> do
          cE <- liftIO $ readTVarIO (cEpoch ci)
          let currentBlockId = ebdBlockId ei
              mapEpoch = ceMapEpoch cE
              -- To make sure our Map Epoch doesn't get too large so we use something slightly bigger than K value "securityParam"
              -- and once the map gets larger than that number we delete the first inserted item making room for another Epoch.
              scaledMapEpoch =
                if size mapEpoch > fromEnum securityParam
                  then deleteMin mapEpoch
                  else mapEpoch

          let updatedMapEpoch = insert currentBlockId latestEpoch scaledMapEpoch
          writeToCache ci (CacheEpoch updatedMapEpoch (ceEpochBlockDiff cE))

------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------

writeToCache :: CacheInternal -> CacheEpoch -> App (Either SyncNodeError ())
writeToCache ci newCacheEpoch = do
  void $ liftIO $ atomically $ writeTVar (cEpoch ci) newCacheEpoch
  pure $ Right ()
