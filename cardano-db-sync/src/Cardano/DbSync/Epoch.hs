{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.DbSync.Epoch (
  epochStartup,
  epochInsert,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache (Cache, CacheEpoch (..), readCacheEpoch, writeBlockToCacheEpoch, writeCacheEpoch, writeEpochToCacheEpoch)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Query (queryResolveInput)
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.Coin (Coin (..))
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
          let eNum = DB.epochNo latestEpoch
              backOne = if eNum == 0 then 0 else eNum - 1
          mEpoch <- queryEpochFromNum backOne
          -- putting the epoch into cache but not a blockId as we don't have that yet
          writeCacheEpoch
            cache
            ( CacheEpoch
                { ceEpoch = mEpoch
                , ceLastKnownBlock = Nothing
                }
            )

epochInsert ::
  Trace IO Text ->
  Cache ->
  BlockDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
epochInsert trce cache (BlockDetails cblk details) = do
  -- put the current block into the cache so we have access to it downstream
  -- when we need to calculate the new epoch from cache.
  void $ writeBlockToCacheEpoch cache cblk

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
insertEpoch trce cache slotDetails = do
  -- read the chache Epoch
  latestCachedEpoch <- liftIO $ readCacheEpoch cache
  let maybeEpoch = ceEpoch latestCachedEpoch

  -- if there isn't cache epock number default it to 0
  let lastCachedEpochNo = maybe 0 DB.epochNo maybeEpoch
      slotEpochNum = unEpochNo (sdEpochNo slotDetails)

  -- These cases are listed from the least likey to occur to the most
  -- likley to keep the logic sane.
  if
      | slotEpochNum > 0 && isNothing maybeEpoch ->
        updateEpochNum cache 0 trce slotDetails
      | slotEpochNum >= lastCachedEpochNo + 2 ->
        updateEpochNum cache (lastCachedEpochNo + 1) trce slotDetails
      | getSyncStatus slotDetails == SyncFollowing ->
        updateEpochNum cache slotEpochNum trce slotDetails
      | otherwise ->
        pure $ Right ()

-- -------------------------------------------------------------------------------------------------

updateEpochNum ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Word64 ->
  Trace IO Text ->
  SlotDetails ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochNum cache slotEpochNum trce slotDetails = do
  -- get the epoch id given a slot number
  mEpochID <- queryForEpochId slotEpochNum
  -- if the slot doesn't exist then we insert
  -- otherwise we update
  maybe
    (insertEpochDB cache trce slotEpochNum)
    (updateEpochDB cache slotEpochNum slotDetails)
    mEpochID

updateEpochDB ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Word64 ->
  SlotDetails ->
  DB.EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochDB cache slotEpochNum slotDetails epochId = do
  -- get the cache epoch
  cachedEpoch <- liftIO $ readCacheEpoch cache
  case ceEpoch cachedEpoch of
    -- only call queryCalc if we don't already have an epoch in cache
    Nothing -> queryCalc
    Just cEpoch ->
      case ceLastKnownBlock cachedEpoch of
        Nothing -> queryCalc
        Just clastKnowBlock -> do
          -- if we have both a block and epoch in cache let's use them to calculate the next epoch
          newCalculatedEpoch <- calculateEpochUsingCache clastKnowBlock cEpoch slotDetails
          case newCalculatedEpoch of
            -- TODO: report what went wrong? before running expensive query
            Left _ -> queryCalc
            Right ep -> do
              -- put the new results into cache and on the DB
              void $ writeEpochToCacheEpoch cache ep
              Right <$> replace epochId ep
  where
    queryCalc ::
      MonadIO m =>
      ReaderT SqlBackend m (Either SyncNodeError ())
    queryCalc = do
      -- this is an expensive query which we should only call when
      -- starting from epoch 0 or the first time we're in following mode
      newEpoch <- DB.queryCalcEpochEntry slotEpochNum

      -- We've now got our new epoch let's write it to the cache.
      -- The assumption is the current block was put into cache upstream inside
      -- `epochInsert` function.
      void $ writeEpochToCacheEpoch cache newEpoch

      -- replace the current epoch in the DB with our newly calculated epoch
      Right <$> replace epochId newEpoch

calculateEpochUsingCache ::
  (MonadBaseControl IO m, MonadIO m) =>
  CardanoBlock ->
  DB.Epoch ->
  SlotDetails ->
  ReaderT SqlBackend m (Either SyncNodeError DB.Epoch)
calculateEpochUsingCache cBlock cEpoch slotDetails = do
  result <- runExceptT $
    case cBlock of
      BlockByron blk -> undefined
      -- calculateEpochByronBlock blk cEpoch slotDetails
      BlockShelley blk ->
        calculateEpochGeneric
          (Generic.fromShelleyBlock blk)
          cEpoch
          slotDetails
      BlockAllegra blk ->
        calculateEpochGeneric
          (Generic.fromAllegraBlock blk)
          cEpoch
          slotDetails
      BlockMary blk ->
        calculateEpochGeneric
          (Generic.fromMaryBlock blk)
          cEpoch
          slotDetails
      BlockAlonzo blk -> undefined
      -- calculateEpochGeneric $
      --   Generic.fromAlonzoBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
      BlockBabbage blk -> undefined

  -- calculateEpochGeneric $
  --   Generic.fromBabbageBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
  pure result

-- Calculating a new epoch by taking the current block getting all the values we need from it.
-- We then the epoch in cache and add these new values to it, thus giving us a new updated epoch.
calculateEpochGeneric ::
  (MonadBaseControl IO m, MonadIO m) =>
  Generic.Block ->
  DB.Epoch ->
  SlotDetails ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.Epoch
calculateEpochGeneric block cEpoch slotDetails = do
  let newEpochTxCount = fromIntegral (DB.epochTxCount cEpoch) + length (Generic.blkTxs block)
      newBlkCount = fromIntegral $ DB.epochBlkCount cEpoch + 1
      txs = Generic.blkTxs block
      outSum = sum $ map (fromIntegral . unCoin . Generic.txOutSum) txs

  newFees <- mapM (`calNewFees` outSum) txs
  let sumNewFees = sum newFees

  pure
    DB.Epoch
      { DB.epochOutSum = fromIntegral (outSum + fromIntegral (DB.epochOutSum cEpoch))
      , DB.epochFees = DB.DbLovelace (DB.unDbLovelace (DB.epochFees cEpoch) + sumNewFees)
      , DB.epochTxCount = fromIntegral newEpochTxCount
      , DB.epochBlkCount = fromIntegral newBlkCount
      , DB.epochNo = unEpochNo (sdEpochNo slotDetails)
      , DB.epochStartTime = DB.epochStartTime cEpoch
      , DB.epochEndTime = sdSlotTime slotDetails
      }
  where
    -- calculating the fees for current block using Tx's
    calNewFees ::
      (MonadBaseControl IO m, MonadIO m) =>
      Generic.Tx ->
      Word64 ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) Word64
    calNewFees tx outS = do
      -- taking all the txInputs and getting their values from a DB lookup.
      -- Errors are ignored but might actually need to handle this gracefully
      qResInputRes <- lift $ mapM queryResolveInput (Generic.txInputs tx)

      let cacheFee = fromIntegral $ DB.unDbLovelace $ DB.epochFees cEpoch
          -- get all of the values that returned and sum them up
          inSum = sum [DB.unDbLovelace dbl | Right (_, dbl) <- qResInputRes]
          diffSum = if inSum >= outS then inSum - outS else 0
          -- not all fees are present so we handle the Nothings by using diffSum
          newFees = maybe diffSum (fromIntegral . unCoin) (Generic.txFees tx)
      pure newFees

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
queryForEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe DB.EpochId)
queryForEpochId epochNum = do
  res <- selectOne $ do
    epoch <- from $ table @DB.Epoch
    where_ (epoch ^. DB.EpochNo ==. val epochNum)
    pure (epoch ^. DB.EpochId)
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
