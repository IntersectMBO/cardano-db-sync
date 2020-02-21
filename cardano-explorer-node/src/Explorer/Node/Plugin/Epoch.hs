{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Plugin.Epoch
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

import           Explorer.DB (Epoch (..), EpochId, EntityField (..), isJust, listToMaybe)
import qualified Explorer.DB as DB
import           Explorer.Node.Error
import           Explorer.Node.Util

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..))
import           Ouroboros.Network.Block (BlockNo (..), Point, Tip, getTipBlockNo)
import           Ouroboros.Network.Point (withOrigin)

epochPluginOnStartup :: Trace IO Text -> ReaderT SqlBackend (LoggingT IO) ()
epochPluginOnStartup trce = do
    liftIO . logInfo trce $ "epochPluginOnStartup: Checking"
    maybe (pure ()) (updatePastEpochs trce) =<< queryLatestBlockEpochNo

epochPluginInsertBlock :: Trace IO Text -> ByronBlock -> Tip ByronBlock -> ReaderT SqlBackend (LoggingT IO) (Either ExplorerNodeError ())
epochPluginInsertBlock trce rawBlk tip =
    case byronBlockRaw rawBlk of
      Ledger.ABOBBoundary bblk -> do
        -- Commit the current transaction and start a new one.
        transactionSaveWithIsolation Serializable
        let newEpoch = Ledger.boundaryEpoch (Ledger.boundaryHeader bblk)
        if newEpoch >= 1
          then do
            res <- updateEpochNum (newEpoch - 1) -- Update the last epoch.
            liftIO . logInfo trce $ "epochPluginInsertBlock: Epoch table updated for epoch " <> textShow (newEpoch - 1)
            -- Commit the transaction again after the epoch table has been updated.
            transactionSaveWithIsolation Serializable
            pure res
          else pure $ Right ()

      Ledger.ABOBBlock blk -> do
        if blockNumber blk > withOrigin 0 unBlockNo (getTipBlockNo tip) - 10
          then do
            eMeta <- DB.queryMeta
            case eMeta of
              Left err -> do
                logErr $ Left (ENELookup "epochPluginInsertBlock" err)
              Right meta -> do
                let slotsPerEpoch = 10 * DB.metaProtocolConst meta
                updateEpochNum (slotNumber blk `div` slotsPerEpoch)
          else pure $ Right ()
  where
    logErr :: MonadIO m => Either ExplorerNodeError () -> ReaderT SqlBackend m (Either ExplorerNodeError ())
    logErr res = do
      case res of
        Left err -> liftIO . logError trce $ "epochPluginInsertBlock: " <> renderExplorerNodeError err
        Right () -> pure ()
      pure res

epochPluginRollbackBlock :: Trace IO Text -> Point ByronBlock -> IO (Either ExplorerNodeError ())
epochPluginRollbackBlock trce point =
    case pointToSlotHash point of
      Nothing -> pure $ Right ()
      Just (slot, _hash) -> DB.runDbAction Nothing $ action (Ledger.unSlotNumber slot)
  where
    action :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError ())
    action slot = do
      eMeta <- DB.queryMeta
      case eMeta of
        Left err -> do
          let left = ENELookup "epochPluginInsertBlock" err
          liftIO . logError trce $ "epochPluginInsertBlock: " <> renderExplorerNodeError left
          pure $ Left left
        Right meta -> do
          let slotsPerEpoch = 10 * DB.metaProtocolConst meta
          -- Simply delete all epoch rows from the rollback epoch forward.
          -- The Epoch table will be repopulated in roll forward.
          delete . from $ \ epoch ->
            where_ (epoch ^. EpochNo >=. val (slot `div` slotsPerEpoch))
          pure $ Right ()

-- -------------------------------------------------------------------------------------------------

type ValMay a = Value (Maybe a)

updatePastEpochs :: MonadIO m => Trace IO Text -> Word64 -> ReaderT SqlBackend m ()
updatePastEpochs trce maxEpochNum =
    loop =<< fromMaybe 0 <$> queryLatestEpochNo
  where
    loop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    loop currentEpoch
      | currentEpoch >= maxEpochNum = pure ()
      | otherwise = do
          transactionSaveWithIsolation Serializable
          liftIO . logInfo trce $ "epochPluginOnStartup: Inserting epoch table for epoch " <> textShow currentEpoch
          either (liftIO . reportError) (const $ loop (currentEpoch + 1)) =<< updateEpochNum currentEpoch

    reportError :: ExplorerNodeError -> IO ()
    reportError err =
      logError trce $ "epochPluginOnStartup: " <> renderExplorerNodeError err


updateEpochNum :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError ())
updateEpochNum epochNum = do
    maybe insertEpoch updateEpoch =<< queryEpochId epochNum
  where
    updateEpoch :: MonadIO m => EpochId -> ReaderT SqlBackend m (Either ExplorerNodeError ())
    updateEpoch epochId = do
      eEpoch <- queryEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left err
        Right epoch -> Right <$> replace epochId epoch

    insertEpoch :: MonadIO m => ReaderT SqlBackend m (Either ExplorerNodeError ())
    insertEpoch = do
      eEpoch <- queryEpochEntry epochNum
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
queryEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError Epoch)
queryEpochEntry epochNum = do
    res <- select . from $ \ (tx `InnerJoin` blk) -> do
              on (tx ^. TxBlock ==. blk ^. BlockId)
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure $ (sum_ (tx ^. TxOutSum), count (tx ^. TxOutSum), min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
    case listToMaybe res of
      Nothing -> pure $ Left (ENEEpochLookup epochNum)
      Just x -> pure $ convert x
  where
    convert :: (ValMay Rational, Value Word64, ValMay UTCTime, ValMay UTCTime) -> Either ExplorerNodeError Epoch
    convert tuple =
      case tuple of
        (Value (Just outSum), Value txCount, Value (Just start), Value (Just end)) ->
            Right $ Epoch (fromIntegral $ numerator outSum) txCount epochNum start end
        _other -> Left $ ENEEpochLookup epochNum


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

