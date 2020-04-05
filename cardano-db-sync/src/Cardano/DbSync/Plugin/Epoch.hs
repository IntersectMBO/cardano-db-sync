{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Plugin.Epoch
  ( epochPluginOnStartup
  , epochPluginInsertBlock
  , epochPluginRollbackBlock
  ) where

import           Cardano.BM.Trace (Trace, logError, logInfo)

import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Slotting as Ledger

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Word (Word64)

import           Database.Esqueleto (Value (..), (^.), (==.), (>=.),
                    delete, desc, from, limit, orderBy, select, val, where_)

import           Database.Persist.Class (replace)
import           Database.Persist.Sql (IsolationLevel (Serializable), SqlBackend,
                    transactionSaveWithIsolation)

import           Cardano.Db (EpochId, EntityField (..), listToMaybe)
import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Network.Block (Point, Tip)

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
        liftIO . logError trce $ "epochPluginInsertBlock: " <> renderDbSyncNodeError (ENELookup "epochPluginInsertBlock" err)
      Right meta ->
        liftIO $ atomicWriteIORef slotsPerEpochVar (10 * DB.metaProtocolConst meta)
    mlbe <- queryLatestEpochNo
    case mlbe of
      Nothing ->
        pure ()
      Just lbe -> do
        let backOne = if lbe == 0 then 0 else lbe - 1
        liftIO $ atomicWriteIORef latestCachedEpochVar (Just backOne)

    updateChainTipEpochVar trce

epochPluginInsertBlock :: Trace IO Text -> ByronBlock -> Tip ByronBlock -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
epochPluginInsertBlock trce rawBlk _tip =
  case byronBlockRaw rawBlk of
    Ledger.ABOBBoundary _ ->
      -- For the OBFT era there are no boundary blocks so we ignore them even in
      -- the Ouroboros Classic era.
      pure $ Right ()

    Ledger.ABOBBlock blk -> do
      slotsPerEpoch <- liftIO $ readIORef slotsPerEpochVar
      mLatestCachedEpoch <- liftIO $ readIORef latestCachedEpochVar
      chainTipEpoch <- liftIO $ readIORef latestChainTipEpochVar
      let epochNum = epochNumber blk slotsPerEpoch
          lastCachedEpoch = fromMaybe 0 mLatestCachedEpoch

      if  | epochNum == chainTipEpoch && lastCachedEpoch == chainTipEpoch ->
              -- Following the chain quite closely.
              updateEpochNum epochNum trce
          | epochNum > 0 && mLatestCachedEpoch == Nothing ->
              updateEpochNum 0 trce
          | epochNum >= lastCachedEpoch + 2 ->
              updateEpochNum (lastCachedEpoch + 1) trce
          | epochNum == chainTipEpoch && lastCachedEpoch < chainTipEpoch ->
              updateEpochNum (lastCachedEpoch + 1) trce
          | epochNum > chainTipEpoch ->
              -- Must just have started a new epoch, so call this which will
              -- update chainTipEpoch.
              updateEpochNum epochNum trce
          | otherwise ->
              pure $ Right ()

epochPluginRollbackBlock :: Trace IO Text -> Point ByronBlock -> IO (Either DbSyncNodeError ())
epochPluginRollbackBlock _trce point =
    case pointToSlotHash point of
      Nothing -> pure $ Right ()
      Just (slot, _hash) -> DB.runDbAction Nothing $ action (Ledger.unSlotNumber slot)
  where
    action :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either DbSyncNodeError ())
    action slot = do
      slotsPerEpoch <- liftIO $ readIORef slotsPerEpochVar
      delete . from $ \ epoch ->
        where_ (epoch ^. EpochNo >=. val (slot `div` slotsPerEpoch))
      transactionSaveWithIsolation Serializable
      pure $ Right ()

-- -------------------------------------------------------------------------------------------------

{-# NOINLINE slotsPerEpochVar #-}
slotsPerEpochVar :: IORef Word64
slotsPerEpochVar = unsafePerformIO $ newIORef 1 -- Gets updated later.

{-# NOINLINE latestCachedEpochVar #-}
latestCachedEpochVar :: IORef (Maybe Word64)
latestCachedEpochVar = unsafePerformIO $ newIORef Nothing -- Gets updated later.

{-# NOINLINE latestChainTipEpochVar #-}
latestChainTipEpochVar :: IORef Word64
latestChainTipEpochVar = unsafePerformIO $ newIORef 0 -- Gets updated later.

updateEpochNum :: MonadIO m => Word64 -> Trace IO Text -> ReaderT SqlBackend m (Either DbSyncNodeError ())
updateEpochNum epochNum trce = do
    transactionSaveWithIsolation Serializable
    mid <- queryEpochId epochNum
    res <- maybe insertEpoch updateEpoch mid
    transactionSaveWithIsolation Serializable
    liftIO $ atomicWriteIORef latestCachedEpochVar (Just epochNum)
    updateChainTipEpochVar trce
    pure res
  where
    updateEpoch :: MonadIO m => EpochId -> ReaderT SqlBackend m (Either DbSyncNodeError ())
    updateEpoch epochId = do
      eEpoch <- DB.queryCalcEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left (ENELookup "updateEpochNum.updateEpoch" err)
        Right epoch -> Right <$> replace epochId epoch

    insertEpoch :: MonadIO m => ReaderT SqlBackend m (Either DbSyncNodeError ())
    insertEpoch = do
      eEpoch <- DB.queryCalcEpochEntry epochNum
      liftIO . logInfo trce $ "epochPluginInsertBlock: Inserting row in epoch table for epoch " <> textShow epochNum
      case eEpoch of
        Left err -> pure $ Left (ENELookup "updateEpochNum.insertEpoch" err)
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


updateChainTipEpochVar :: MonadIO m => Trace IO Text -> ReaderT SqlBackend m ()
updateChainTipEpochVar _trce = do
  eMeta <- DB.queryMeta
  liftIO $ do
    currentTime <- getCurrentTime
    case eMeta of
      Left _ -> do
        atomicWriteIORef latestChainTipEpochVar 0
      Right meta -> do
        let epoch = diffUTCTime currentTime (DB.metaStartTime meta)
                    / (0.01 * fromIntegral (DB.metaSlotDuration meta * DB.metaProtocolConst meta))
        atomicWriteIORef latestChainTipEpochVar $ floor epoch
