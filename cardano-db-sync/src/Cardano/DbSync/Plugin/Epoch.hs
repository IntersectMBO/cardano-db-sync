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

import           Control.Monad (join, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe (fromMaybe)
import           Data.Ratio (numerator)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.), (>=.),
                    count, delete, desc, from, just, limit, max_, min_, on, orderBy, select, sum_,
                    val, where_)

import           Database.Persist.Class (replace)
import           Database.Persist.Sql (IsolationLevel (Serializable), SqlBackend,
                    transactionSaveWithIsolation)

import           Cardano.Db (Epoch (..), EpochId, EntityField (..), isJust, listToMaybe)
import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Network.Block (BlockNo (..), Point, Tip, getTipBlockNo)
import           Ouroboros.Network.Point (withOrigin)

import           System.IO.Unsafe (unsafePerformIO)

epochPluginOnStartup :: Trace IO Text -> ReaderT SqlBackend (LoggingT IO) ()
epochPluginOnStartup trce = do
    liftIO . logInfo trce $ "epochPluginOnStartup: Checking"
    eMeta <- DB.queryMeta
    case eMeta of
      Left err ->
        liftIO . logError trce $ "epochPluginInsertBlock: " <> renderDbSyncNodeError (ENELookup "epochPluginInsertBlock" err)
      Right meta ->
        liftIO $ writeIORef slotsPerEpochVar (10 * DB.metaProtocolConst meta)
    maybe (pure ()) (insertPastEpochs trce) =<< queryLatestBlockEpochNo

epochPluginInsertBlock :: Trace IO Text -> ByronBlock -> Tip ByronBlock -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
epochPluginInsertBlock trce rawBlk tip =
  if False
  then pure $ Right ()
  else
    case byronBlockRaw rawBlk of
      Ledger.ABOBBoundary _ ->
        -- For the OBFT era there are no boundary blocks so we ignore them even in
        -- the Ouroboros Classic era.
        pure $ Right ()

      Ledger.ABOBBlock blk -> do
        mLatestCachedEpoch <- liftIO $ readIORef latestCachedEpochVar
        slotsPerEpoch <- liftIO $ readIORef slotsPerEpochVar
        let epochNum = epochNumber blk slotsPerEpoch
            latestCachedEpoch = fromMaybe epochNum mLatestCachedEpoch

        -- Tip here is the tip block of the local node, not the tip of the chain.
        if  | blockNumber blk > withOrigin 0 unBlockNo (getTipBlockNo tip) - 10 -> do
                -- Following the chain quite closely.
                updateEpochNum epochNum trce
            | epochNum == 1 && mLatestCachedEpoch == Nothing ->
                updateEpochNum 0 trce
            | epochNum > latestCachedEpoch + 1 ->
                updateEpochNum (latestCachedEpoch + 1) trce
            | otherwise -> pure $ Right ()

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

type ValMay a = Value (Maybe a)

{-# NOINLINE slotsPerEpochVar #-}
slotsPerEpochVar :: IORef Word64
slotsPerEpochVar = unsafePerformIO $ newIORef 1 -- Gets updated later.

{-# NOINLINE latestCachedEpochVar #-}
latestCachedEpochVar :: IORef (Maybe Word64)
latestCachedEpochVar = unsafePerformIO $ newIORef Nothing -- Gets updated later.

insertPastEpochs :: MonadIO m => Trace IO Text -> Word64 -> ReaderT SqlBackend m ()
insertPastEpochs trce maxEpochNum =
    maybe (loop 0) (liftIO . writeIORef latestCachedEpochVar . Just) =<< queryLatestEpochNo
  where
    loop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    loop currentEpoch
      | currentEpoch >= maxEpochNum =
          liftIO $ writeIORef latestCachedEpochVar (Just currentEpoch)
      | otherwise = do
          transactionSaveWithIsolation Serializable
          liftIO . logInfo trce $ "epochPluginOnStartup: Inserting epoch table for epoch " <> textShow currentEpoch
          either (liftIO . reportError) (const $ loop (currentEpoch + 1)) =<< updateEpochNum currentEpoch trce

    reportError :: DbSyncNodeError -> IO ()
    reportError err =
      logError trce $ "epochPluginOnStartup: " <> renderDbSyncNodeError err


updateEpochNum :: MonadIO m => Word64 -> Trace IO Text -> ReaderT SqlBackend m (Either DbSyncNodeError ())
updateEpochNum epochNum trce = do
    mid <- queryEpochId epochNum
    res <- maybe insertEpoch updateEpoch mid
    transactionSaveWithIsolation Serializable
    liftIO $ writeIORef latestCachedEpochVar (Just epochNum)
    pure res
  where
    updateEpoch :: MonadIO m => EpochId -> ReaderT SqlBackend m (Either DbSyncNodeError ())
    updateEpoch epochId = do
      eEpoch <- queryEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left err
        Right epoch -> Right <$> replace epochId epoch

    insertEpoch :: MonadIO m => ReaderT SqlBackend m (Either DbSyncNodeError ())
    insertEpoch = do
      eEpoch <- queryEpochEntry epochNum
      liftIO . logInfo trce $ "epochPluginInsertBlock: Inserting row in epoch table for epoch " <> textShow epochNum
      case eEpoch of
        Left err -> pure $ Left err
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

-- | Calculate the Epoch table entry for the specified epoch.
-- When syncing the chain or filling an empty table, this is called at each epoch boundary to
-- calculate the Epcoh entry for the last epoch.
-- When following the chain, this is called for each new block of the current epoch.
queryEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either DbSyncNodeError Epoch)
queryEpochEntry epochNum = do
    res <- select . from $ \ (tx `InnerJoin` blk) -> do
              on (tx ^. TxBlock ==. blk ^. BlockId)
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure $ (sum_ (tx ^. TxOutSum), count (tx ^. TxOutSum), min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
    case listToMaybe res of
      Nothing -> queryEmptyEpoch
      Just x -> convert x
  where
    convert :: MonadIO m
            => (ValMay Rational, Value Word64, ValMay UTCTime, ValMay UTCTime)
            -> ReaderT SqlBackend m (Either DbSyncNodeError Epoch)
    convert tuple =
      case tuple of
        (Value (Just outSum), Value txCount, Value (Just start), Value (Just end)) ->
            pure (Right $ Epoch (fromIntegral $ numerator outSum) txCount epochNum start end)
        _otherwise -> queryEmptyEpoch

    queryEmptyEpoch :: MonadIO m => ReaderT SqlBackend m (Either DbSyncNodeError Epoch)
    queryEmptyEpoch = do
      res <- select . from $ \ blk -> do
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure (count (blk ^. BlockId), min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
      case listToMaybe res of
        Nothing -> pure $ Left (ENEEpochLookup epochNum)
        Just x -> pure $ convert2 x

    convert2 :: (Value Word64, ValMay UTCTime, ValMay UTCTime) -> Either DbSyncNodeError Epoch
    convert2 tuple =
      case tuple of
        (Value blkCount, Value (Just start), Value (Just end)) | blkCount > 0 ->
            Right (Epoch 0 0 epochNum start end)
        _otherwise -> Left (ENEEpochLookup epochNum)

-- | Get the epoch number of the most recent epoch in the Block table.
queryLatestBlockEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestBlockEpochNo = do
  res <- select . from $ \ blk -> do
            where_ (isJust (blk ^. BlockEpochNo))
            orderBy [desc (blk ^. BlockEpochNo)]
            limit 1
            pure $ (blk ^. BlockEpochNo)
  pure $ join (unValue <$> listToMaybe res)

-- | Get the epoch number of the most recent epoch in the Epoch table.
queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [desc (epoch ^. EpochNo)]
            limit 1
            pure $ (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res
