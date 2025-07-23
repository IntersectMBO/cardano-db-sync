{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.Epoch (
  readEpochBlockDiffFromCache,
  readLastMapEpochFromCache,
  rollbackMapEpochInCache,
  writeEpochBlockDiffToCache,
  writeToMapEpochCache,
  withNoCache,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types (LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Cache.Types (CacheEpoch (..), CacheInternal (..), CacheStatus (..), EpochBlockDiff (..))
import Cardano.DbSync.Era.Shelley.Generic.StakeDist (getSecurityParameter)
import Cardano.DbSync.Ledger.Types (HasLedgerEnv (..))
import Cardano.DbSync.LocalStateQuery (NoLedgerEnv (..))
import Cardano.Ledger.BaseTypes.NonZero (NonZero (..))
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Data.Map.Strict (deleteMin, insert, lookupMax, size, split)

-------------------------------------------------------------------------------------
-- Epoch Cache
-------------------------------------------------------------------------------------
readEpochBlockDiffFromCache :: MonadIO m => CacheStatus -> m (Maybe EpochBlockDiff)
readEpochBlockDiffFromCache cache =
  case cache of
    NoCache -> pure Nothing
    ActiveCache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochBlockDiff cE) of
        (_, epochInternal) -> pure epochInternal

readLastMapEpochFromCache :: CacheStatus -> IO (Maybe DB.Epoch)
readLastMapEpochFromCache cache =
  case cache of
    NoCache -> pure Nothing
    ActiveCache ci -> do
      cE <- readTVarIO (cEpoch ci)
      let mapEpoch = ceMapEpoch cE
      -- making sure db sync wasn't restarted on the last block in epoch
      if length mapEpoch == 1
        then pure Nothing
        else case lookupMax mapEpoch of
          Nothing -> pure Nothing
          Just (_, ep) -> pure $ Just ep

rollbackMapEpochInCache :: MonadIO m => CacheInternal -> DB.BlockId -> m ()
rollbackMapEpochInCache cacheInternal blockId = do
  cE <- liftIO $ readTVarIO (cEpoch cacheInternal)
  -- split the map and delete anything after blockId including it self as new blockId might be
  -- given when inserting the block again when doing rollbacks.
  let (newMapEpoch, _) = split blockId (ceMapEpoch cE)
  writeToCache cacheInternal (CacheEpoch newMapEpoch (ceEpochBlockDiff cE))

writeEpochBlockDiffToCache ::
  MonadIO m =>
  CacheStatus ->
  EpochBlockDiff ->
  DB.DbAction m ()
writeEpochBlockDiffToCache cache epCurrent =
  case cache of
    NoCache -> throwError $ DB.DbError (DB.mkDbCallStack "writeEpochBlockDiffToCache") "Cache is NoCache" Nothing
    ActiveCache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochBlockDiff cE) of
        (epochLatest, _) -> writeToCache ci (CacheEpoch epochLatest (Just epCurrent))

-- | Insert an epoch into Map Epoch cache. A blockId is used as the key which was generated when inserting the block
-- | into the db. This is so we have a historic representation of an epoch after every block is inserted.
-- | This becomes usefull when syncing and doing rollbacks and saves on expensive db queries to calculte an epoch.
writeToMapEpochCache ::
  MonadIO m =>
  SyncEnv ->
  CacheStatus ->
  DB.Epoch ->
  DB.DbAction m ()
writeToMapEpochCache syncEnv cache latestEpoch = do
  -- this can also be tought of as max rollback number
  let securityParam =
        case envLedgerEnv syncEnv of
          HasLedger hle -> getSecurityParameter $ leProtocolInfo hle
          NoLedger nle -> getSecurityParameter $ nleProtocolInfo nle
  case cache of
    NoCache -> throwError $ DB.DbError (DB.mkDbCallStack "writeToMapEpochCache") "Cache is NoCache" Nothing
    ActiveCache ci -> do
      -- get EpochBlockDiff so we can use the BlockId we stored when inserting blocks
      epochInternalCE <- readEpochBlockDiffFromCache cache
      case epochInternalCE of
        Nothing -> throwError $ DB.DbError (DB.mkDbCallStack "writeToMapEpochCache") "No epochInternalEpochCache" Nothing
        Just ei -> do
          cE <- liftIO $ readTVarIO (cEpoch ci)
          let currentBlockId = ebdBlockId ei
              mapEpoch = ceMapEpoch cE
              -- To make sure our Map Epoch doesn't get too large so we use something slightly bigger than K value "securityParam"
              -- and once the map gets larger than that number we delete the first inserted item making room for another Epoch.
              scaledMapEpoch =
                if size mapEpoch > fromEnum (unNonZero securityParam)
                  then deleteMin mapEpoch
                  else mapEpoch

          let updatedMapEpoch = insert currentBlockId latestEpoch scaledMapEpoch
          writeToCache ci (CacheEpoch updatedMapEpoch (ceEpochBlockDiff cE))

------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------

writeToCache :: MonadIO m => CacheInternal -> CacheEpoch -> m ()
writeToCache ci newCacheEpoch = do
  void $ liftIO $ atomically $ writeTVar (cEpoch ci) newCacheEpoch

withNoCache :: SyncEnv -> SyncEnv
withNoCache syncEnv = syncEnv {envCache = NoCache}
