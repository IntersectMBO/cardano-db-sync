{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Epoch (
  epochStartup,
  epochInsert,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Chain.Block as Byron
import Cardano.Db (EntityField (..), Epoch (..), EpochId)
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache (Cache, CacheEpoch (..), readCacheEpoch, writeCacheEpoch)
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (from, on, replace)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Esqueleto.Experimental (
  Entity (entityVal),
  SqlBackend,
  desc,
  from,
  orderBy,
  replace,
  selectOne,
  table,
  unValue,
  val,
  where_,
  (==.),
  (^.),
 )
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

-- Populating the Epoch table has two mode:
--  * SyncLagging: when the node is far behind the chain tip and is just updating the DB. In this
--    mode, the row for an epoch is only calculated and inserted when at the end of the epoch.
--  * Following: When the node is at or close to the chain tip, the row for a given epoch is
--    updated on each new block.
--
-- When in syncing mode, the row for the current epoch being synced may be incorrect.
epochStartup :: Cache -> Bool -> Trace IO Text -> SqlBackend -> IO ()
epochStartup cache isExtended trce backend =
  when isExtended $ do
    DB.runDbIohkLogging backend trce $ do
      liftIO . logInfo trce $ "epochStartup: Checking"
      mLatestEpoch <- queryLatestEpoch
      case mLatestEpoch of
        Nothing ->
          pure ()
        Just latestEpoch -> do
          let eNum = epochNo latestEpoch
              backOne = if eNum == 0 then 0 else eNum - 1
          mEpoch <- queryEpochFromNum backOne
          -- putting the epoch into cache but not a blockId as we don't have that yet
          writeCacheEpoch
            cache
            ( CacheEpoch
                { ceEpoch = mEpoch
                , ceLastKnownBlockId = Nothing
                }
            )

epochInsert ::
  Trace IO Text ->
  Cache ->
  BlockDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
epochInsert trce cache (BlockDetails cblk details) = do
  case cblk of
    BlockByron bblk ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary {} ->
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era.
          pure $ Right ()
        Byron.ABOBBlock _blk ->
          insertEpoch trce cache details
    BlockShelley {} -> epochUpdate
    BlockAllegra {} -> epochUpdate
    BlockMary {} -> epochUpdate
    BlockAlonzo {} -> epochUpdate
    BlockBabbage {} -> epochUpdate
  where
    -- What we do here is completely independent of Shelley/Allegra/Mary eras.
    epochUpdate :: ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    epochUpdate = do
      when (sdSlotTime details > sdCurrentTime details) $
        liftIO . logError trce $
          mconcat
            ["Slot time '", textShow (sdSlotTime details), "' is in the future"]
      insertEpoch trce cache details

-- -------------------------------------------------------------------------------------------------

insertEpoch ::
  Trace IO Text ->
  Cache ->
  SlotDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
insertEpoch trce cache details = do
  -- read the chache Epoch
  latestCachedEpoch <- liftIO $ readCacheEpoch cache
  let maybeEpoch = ceEpoch latestCachedEpoch

  -- if there isn't cache epock number default it to 0
  let lastCachedEpochNo = maybe 0 epochNo maybeEpoch
      slotEpochNum = unEpochNo (sdEpochNo details)

  -- These cases are listed from the least likey to occur to the most
  -- likley to keep the logic sane.
  if
      | slotEpochNum > 0 && isNothing maybeEpoch ->
        updateEpochNum cache 0 trce
      | slotEpochNum >= lastCachedEpochNo + 2 ->
        updateEpochNum cache (lastCachedEpochNo + 1) trce
      | getSyncStatus details == SyncFollowing ->
        updateEpochNum cache slotEpochNum trce
      | otherwise ->
        pure $ Right ()

-- -------------------------------------------------------------------------------------------------

updateEpochNum ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Word64 ->
  Trace IO Text ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochNum cache slotEpochNum trce = do
  mid <- queryForEpochId slotEpochNum
  maybe
    (insertEpochDB cache trce slotEpochNum)
    (updateEpochDB cache slotEpochNum)
    mid

updateEpochDB ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Word64 ->
  EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochDB cache slotEpochNum epochId = do
  cachedEpoch <- liftIO $ readCacheEpoch cache
  case ceEpoch cachedEpoch of
    -- only call queryCalc if we don't already have an epoch in cache
    Nothing -> queryCalc
    Just cEpoch ->
      case ceLastKnownBlockId cachedEpoch of
        Nothing -> queryCalc
        Just lastKnowBlockId -> do
          -- if we have both a block and epoch then lets use them
          -- to do a new less expensive query of updating the epoch
          calculatedEpoch <- DB.queryCalcEpochUsingLastBlockId lastKnowBlockId slotEpochNum

          -- sum calculated epoch with cached epoch
          let newEpochOutSum = epochOutSum calculatedEpoch + epochOutSum cEpoch
              calcFee = fromIntegral $ DB.unDbLovelace $ epochFees calculatedEpoch
              cacheFee = fromIntegral $ DB.unDbLovelace $ epochFees cEpoch
              newEpochFees = DB.DbLovelace $ calcFee + cacheFee
              newEpochTxCount = epochTxCount calculatedEpoch + epochTxCount cEpoch
              newEpochBlkCount = epochBlkCount calculatedEpoch + epochBlkCount cEpoch
              newEpoch =
                Epoch
                  { epochOutSum = newEpochOutSum
                  , epochFees = newEpochFees
                  , epochTxCount = newEpochTxCount
                  , epochBlkCount = newEpochBlkCount
                  , epochNo = epochNo calculatedEpoch
                  , epochStartTime = epochStartTime cEpoch
                  , epochEndTime = epochEndTime calculatedEpoch
                  }
          -- get the latest blockId
          lattestBlockId <- DB.queryLatestBlockId
          -- put the new results into cache and on the DB
          void $
            writeCacheEpoch
              cache
              ( CacheEpoch
                  { ceEpoch = Just newEpoch
                  , ceLastKnownBlockId = lattestBlockId
                  }
              )
          Right <$> replace epochId newEpoch
  where
    queryCalc ::
      MonadIO m =>
      ReaderT SqlBackend m (Either SyncNodeError ())
    queryCalc = do
      -- this is an expensive query which we should only call when
      -- starting from epoch 0 or the first time we're in following mode
      newEpoch <- DB.queryCalcEpochEntry slotEpochNum
      lattestBlockId <- DB.queryLatestBlockId
      -- update our epochChache with new values
      _ <-
        writeCacheEpoch
          cache
          ( CacheEpoch
              { ceEpoch = Just newEpoch
              , ceLastKnownBlockId = lattestBlockId
              }
          )
      -- replace the current epoch in the DB with our new one
      Right <$> replace epochId newEpoch

insertEpochDB ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Trace IO Text ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertEpochDB _ trce slotEpochNum = do
  epoch <- DB.queryCalcEpochEntry slotEpochNum
  liftIO . logInfo trce $ "epochPluginInsertBlockDetails: epoch " <> textShow slotEpochNum
  void $ DB.insertEpoch epoch
  pure $ Right ()

-- -------------------------------------------------------------------------------------------------

-- | Get the PostgreSQL row index (EpochId) that matches the given epoch number.
queryForEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe EpochId)
queryForEpochId epochNum = do
  res <- selectOne $ do
    epoch <- from $ table @DB.Epoch
    where_ (epoch ^. DB.EpochNo ==. val epochNum)
    pure (epoch ^. EpochId)
  pure $ unValue <$> res

-- | Get an epoch given it's number.
queryEpochFromNum :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe DB.Epoch)
queryEpochFromNum epochNum = do
  res <- selectOne $ do
    epoch <- from $ table @DB.Epoch
    where_ (epoch ^. DB.EpochNo ==. val epochNum)
    pure epoch
  pure $ entityVal <$> res

-- | Get the most recent epoch in the Epoch table.
queryLatestEpoch :: MonadIO m => ReaderT SqlBackend m (Maybe DB.Epoch)
queryLatestEpoch = do
  res <- selectOne $ do
    epoch <- from $ table @DB.Epoch
    orderBy [desc (epoch ^. DB.EpochNo)]
    pure epoch
  pure $ entityVal <$> res
