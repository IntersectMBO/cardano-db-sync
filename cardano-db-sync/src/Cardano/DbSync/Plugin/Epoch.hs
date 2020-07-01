{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Plugin.Epoch
  ( epochPluginOnStartup
  , epochPluginInsertBlock
  , epochPluginRollbackBlock
  ) where

import           Cardano.BM.Trace (Trace, logError, logInfo)

import qualified Cardano.Chain.Block as Byron
import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Text (Text)
import           Data.Word (Word64)

import           Database.Esqueleto (Value (..), (^.), (==.),
                    desc, from, limit, orderBy, select, val, where_)

import           Database.Persist.Class (replace)
import           Database.Persist.Sql (IsolationLevel (Serializable), SqlBackend,
                    transactionSaveWithIsolation)

import           Cardano.Db (EpochId, EntityField (..), listToMaybe)
import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))

import           System.IO.Unsafe (unsafePerformIO)

-- Populating the Epoch table has two mode:
--  * Syncing: when the node is far behind the chain tip and is just updating the DB. In this
--    mode, the row for an epoch is only calculated and inserted when at the end of the epoch.
--  * Following: When the node is at or close to the chain tip, the row for a given epoch is
--    updated on each new block.
--
-- When in syncing mode, the row for the current epoch being synced may be incorrect.



epochPluginOnStartup :: Trace IO Text -> ReaderT SqlBackend (LoggingT IO) ()
epochPluginOnStartup trce = do
    liftIO . logInfo trce $ "epochPluginOnStartup: Checking"
    eMeta <- DB.queryMeta
    case eMeta of
      Left err ->
        liftIO . logError trce $ "epochPluginInsertBlock: " <> renderDbSyncNodeError (NELookup "epochPluginInsertBlock" err)
      Right meta ->
        liftIO $ atomicWriteIORef slotsPerEpochVar (DB.metaSlotsPerEpoch meta)
    mlbe <- queryLatestEpochNo
    case mlbe of
      Nothing ->
        pure ()
      Just lbe -> do
        let backOne = if lbe == 0 then 0 else lbe - 1
        liftIO $ atomicWriteIORef latestCachedEpochVar (Just backOne)

epochPluginInsertBlock :: Trace IO Text -> DbSyncEnv -> CardanoBlockTip -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
epochPluginInsertBlock trce _env blkTip = do
  slotsPerEpoch <- liftIO $ readIORef slotsPerEpochVar
  case blkTip of
    ByronBlockTip bblk _tip ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary _ ->
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era.
          pure $ Right ()

        Byron.ABOBBlock blk ->
          insertBlock trce (Byron.epochNumber blk slotsPerEpoch) (SlotNo $ Byron.slotNumber blk)
    ShelleyBlockTip sblk _tip ->
      insertBlock trce (Shelley.epochNumber sblk slotsPerEpoch) (SlotNo $ Shelley.slotNumber sblk)

-- Nothing to be done here.
-- Rollback will take place in the Default plugin and the epoch table will be recalculated.
epochPluginRollbackBlock :: Trace IO Text -> CardanoPoint -> IO (Either DbSyncNodeError ())
epochPluginRollbackBlock _ _ = pure $ Right ()

-- -------------------------------------------------------------------------------------------------

insertBlock
    :: Trace IO Text
    -> Word64 -> SlotNo
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertBlock trce epochNum tipSlot = do
  mLatestCachedEpoch <- liftIO $ readIORef latestCachedEpochVar
  let lastCachedEpoch = fromMaybe 0 mLatestCachedEpoch
  estTipSlot <- queryEstimatedTipSlotNo trce

  -- These cases are listed from the least likey to occur to the most
  -- likley to keep the logic sane.

  if  | epochNum > 0 && mLatestCachedEpoch == Nothing ->
          updateEpochNum 0 trce
      | epochNum >= lastCachedEpoch + 2 ->
          updateEpochNum (lastCachedEpoch + 1) trce
      | unSlotNo estTipSlot - unSlotNo tipSlot < 50 ->
          -- Following the chain very closely.
          updateEpochNum epochNum trce
      | otherwise ->
          pure $ Right ()

-- -------------------------------------------------------------------------------------------------

{-# NOINLINE slotsPerEpochVar #-}
slotsPerEpochVar :: IORef Word64
slotsPerEpochVar = unsafePerformIO $ newIORef 1 -- Gets updated later.

{-# NOINLINE latestCachedEpochVar #-}
latestCachedEpochVar :: IORef (Maybe Word64)
latestCachedEpochVar = unsafePerformIO $ newIORef Nothing -- Gets updated later.

updateEpochNum :: (MonadBaseControl IO m, MonadIO m) => Word64 -> Trace IO Text -> ReaderT SqlBackend m (Either DbSyncNodeError ())
updateEpochNum epochNum trce = do
    transactionSaveWithIsolation Serializable
    mid <- queryEpochId epochNum
    res <- maybe insertEpoch updateEpoch mid
    transactionSaveWithIsolation Serializable
    liftIO $ atomicWriteIORef latestCachedEpochVar (Just epochNum)
    pure res
  where
    updateEpoch :: MonadIO m => EpochId -> ReaderT SqlBackend m (Either DbSyncNodeError ())
    updateEpoch epochId = do
      eEpoch <- DB.queryCalcEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left (NELookup "updateEpochNum.updateEpoch" err)
        Right epoch -> Right <$> replace epochId epoch

    insertEpoch :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m (Either DbSyncNodeError ())
    insertEpoch = do
      eEpoch <- DB.queryCalcEpochEntry epochNum
      liftIO . logInfo trce $ "epochPluginInsertBlock: Inserting row in epoch table for epoch " <> textShow epochNum
      case eEpoch of
        Left err -> pure $ Left (NELookup "updateEpochNum.insertEpoch" err)
        Right epoch -> do
          void $ DB.insertEpoch epoch
          pure $ Right ()

-- -------------------------------------------------------------------------------------------------

-- | Get the PostgreSQL row index (EpochId) that matches the given epoch number.
queryEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe EpochId)
queryEpochId epochNum = do
  res <- select . from $ \ epoch -> do
            where_ (epoch ^. EpochNo ==. val epochNum)
            pure $ (epoch ^. EpochId)
  pure $ unValue <$> (listToMaybe res)

-- | Get the epoch number of the most recent epoch in the Epoch table.
queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [desc (epoch ^. EpochNo)]
            limit 1
            pure $ (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res

queryEstimatedTipSlotNo :: MonadIO m => Trace IO Text -> ReaderT SqlBackend m SlotNo
queryEstimatedTipSlotNo _trce = do
  eMeta <- DB.queryMeta
  liftIO $ do
    case eMeta of
      Left _ -> pure (SlotNo 0)
      Right meta -> SlotNo <$> liftIO (calcSlotNo meta)
  where
    calcSlotNo :: DB.Meta -> IO Word64
    calcSlotNo meta = do
      currentTime <- getCurrentTime
      pure $ floor (diffUTCTime currentTime (DB.metaStartTime meta)
                    / (0.001 * fromIntegral (DB.metaSlotDuration meta)))

