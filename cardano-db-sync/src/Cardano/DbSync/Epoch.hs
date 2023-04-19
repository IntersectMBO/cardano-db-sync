{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.DbSync.Epoch (
  EpochPlutusAndPrices (..),
  epochHandler,
  queryLatestEpoch,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache (Cache, CacheEpoch (..), readCacheEpoch, writeEpochToCacheEpoch)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
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

data EpochPlutusAndPrices = EpochPlutusAndPrices
  { epochIsPlutusExtra :: !Bool
  , epochPrices :: !(Maybe Ledger.Prices)
  }
epochHandler ::
  Trace IO Text ->
  Cache ->
  EpochPlutusAndPrices ->
  BlockDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
epochHandler trce cache ePlutusPrices (BlockDetails cblk details) = do
  -- Update cache with the current block, we use the block in downstream functions when calculating the new epoch.
  -- void $ writeBlockToCacheEpoch cache cblk
  case cblk of
    BlockByron bblk ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary {} ->
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era.
          pure $ Right ()
        Byron.ABOBBlock _blk ->
          checkSlotAndEpochNum trce cache ePlutusPrices details
    BlockShelley {} -> epochSlotTimecheck
    BlockAllegra {} -> epochSlotTimecheck
    BlockMary {} -> epochSlotTimecheck
    BlockAlonzo {} -> epochSlotTimecheck
    BlockBabbage {} -> epochSlotTimecheck
  where
    -- What we do here is completely independent of Shelley/Allegra/Mary eras.
    epochSlotTimecheck :: ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    epochSlotTimecheck = do
      when (sdSlotTime details > sdCurrentTime details) $
        liftIO . logError trce $
          mconcat
            ["Slot time '", textShow (sdSlotTime details), "' is in the future"]
      checkSlotAndEpochNum trce cache ePlutusPrices details

-- -------------------------------------------------------------------------------------------------

checkSlotAndEpochNum ::
  Trace IO Text ->
  Cache ->
  EpochPlutusAndPrices ->
  SlotDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
checkSlotAndEpochNum trce cache ePlutusPrices slotDetails = do
  -- read the chached Epoch
  (Just cacheEpoch) <- liftIO $ readCacheEpoch cache

  -- if there isn't a cached epoch default it's number to 0 as this is the first block.
  let mEpoch = ceEpoch cacheEpoch
      epochNum = maybe 0 DB.epochNo mEpoch

      slotEpochNum = unEpochNo (sdEpochNo slotDetails)

  -- These cases are listed from the least likey to occur to the most
  -- likley to keep the logic sane.
  if
      | slotEpochNum > 0 && isNothing mEpoch -> insertOrReplaceEpoch cache ePlutusPrices 0 trce slotDetails
      | slotEpochNum >= epochNum + 2 -> insertOrReplaceEpoch cache ePlutusPrices (epochNum + 1) trce slotDetails
      | getSyncStatus slotDetails == SyncFollowing -> insertOrReplaceEpoch cache ePlutusPrices slotEpochNum trce slotDetails
      | otherwise -> pure $ Right ()

-- -------------------------------------------------------------------------------------------------

insertOrReplaceEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  EpochPlutusAndPrices ->
  Word64 ->
  Trace IO Text ->
  SlotDetails ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertOrReplaceEpoch cache ePlutusPrices slotEpochNum trce slotDetails = do
  -- get the epoch id using a slot number
  mEpochID <- queryForEpochId slotEpochNum
  -- if the epoch id doesn't exist this means we don't have it yet so we
  -- calculate and insert a new epoch otherwise we replace existing epoch.
  maybe
    (insertEpochIntoDB trce slotEpochNum)
    (replaceEpoch trce cache ePlutusPrices slotEpochNum slotDetails)
    mEpochID

insertEpochIntoDB ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertEpochIntoDB trce slotEpochNum = do
  epoch <- DB.queryCalcEpochEntry slotEpochNum
  liftIO . logInfo trce $ "epochPluginInsertBlockDetails: epoch " <> textShow slotEpochNum
  void $ DB.insertEpoch epoch
  pure $ Right ()

-- | When replacing an epoch we have the opertunity to try and use the cacheEpoch values
--   to calculate our new epoch all from cache rather than querying the db which is expensive.
replaceEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  EpochPlutusAndPrices ->
  Word64 ->
  SlotDetails ->
  DB.EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpoch trce cache ePlutusPrices slotEpochNum slotDetails epochId = do
  cacheEpoch <- liftIO $ readCacheEpoch cache
  -- do we have a cacheEpoch
  case cacheEpoch of
    Nothing -> do
      liftIO . logInfo trce $ "replaceEpoch: using cacheEpoch Nothing "
      calculateFromDbAndReplaceEpoch
    Just cEpoch -> do
      case ceEpoch cEpoch of
        Nothing -> do
          liftIO . logInfo trce $ "replaceEpoch: ceEpoch Nothing "
          calculateFromDbAndReplaceEpoch
        Just ceE -> do
          let newCalculatedEpoch = calculateEpochUsingCache ePlutusPrices cEpoch ceE slotDetails
          liftIO . logInfo trce $ "replaceEpoch: calculated epoch from cache: " <> textShow newCalculatedEpoch
          void $ writeEpochToCacheEpoch cache newCalculatedEpoch
          Right <$> replace epochId newCalculatedEpoch
  where
    calculateFromDbAndReplaceEpoch ::
      MonadIO m =>
      ReaderT SqlBackend m (Either SyncNodeError ())
    calculateFromDbAndReplaceEpoch = do
      -- this query is expensive and what we are trying to avoid
      newEpoch <- DB.queryCalcEpochEntry slotEpochNum
      -- add the newly calculated epoch to cache.
      void $ writeEpochToCacheEpoch cache newEpoch
      -- replace the current epoch in the DB with our newly calculated epoch
      Right <$> replace epochId newEpoch

calculateEpochUsingCache ::
  EpochPlutusAndPrices ->
  CacheEpoch ->
  DB.Epoch ->
  SlotDetails ->
  DB.Epoch
calculateEpochUsingCache ePlutusPrices cacheEpoch ceE slotDetails = do
  let eIsEplutus = epochIsPlutusExtra ePlutusPrices
      ePrices = epochPrices ePlutusPrices
  case ceBlock cacheEpoch of
    BlockByron blk ->
      calculateEpochByron
        blk
        ceE
        cacheEpoch
        slotDetails
    BlockShelley blk ->
      calculateEpochGeneric
        (Generic.fromShelleyBlock blk)
        ceE
        cacheEpoch
        slotDetails
    BlockAllegra blk ->
      calculateEpochGeneric
        (Generic.fromAllegraBlock blk)
        ceE
        cacheEpoch
        slotDetails
    BlockMary blk ->
      calculateEpochGeneric
        (Generic.fromMaryBlock blk)
        ceE
        cacheEpoch
        slotDetails
    BlockAlonzo blk ->
      calculateEpochGeneric
        (Generic.fromAlonzoBlock eIsEplutus ePrices blk)
        ceE
        cacheEpoch
        slotDetails
    BlockBabbage blk ->
      calculateEpochGeneric
        (Generic.fromBabbageBlock eIsEplutus ePrices blk)
        ceE
        cacheEpoch
        slotDetails

-- Calculating a new epoch by taking the current block getting all the values we need from it.
-- We then the epoch in cache and add these new values to it, thus giving us a new updated epoch.
calculateEpochGeneric ::
  Generic.Block ->
  DB.Epoch ->
  CacheEpoch ->
  SlotDetails ->
  DB.Epoch
calculateEpochGeneric block ceE cacheEpoch slotDetails = do
  let txs = Generic.blkTxs block
      newEpochTxCount = length txs
      outSum = sum $ map (fromIntegral . unCoin . Generic.txOutSum) txs
  convertToEpoch ceE cacheEpoch slotDetails newEpochTxCount outSum

calculateEpochByron ::
  ByronBlock ->
  DB.Epoch ->
  CacheEpoch ->
  SlotDetails ->
  DB.Epoch
calculateEpochByron block ceE cacheEpoch slotDetails = do
  -- we handle Tx differently with Byron blocks
  let (newEpochTxCount, newOutSum) =
        case byronBlockRaw block of
          Byron.ABOBBlock aBlock -> do
                -- the txs in the block
            let txs = Byron.blockPayload aBlock
                newTxCount = length txs
                -- getting all the TxOut values in Lovelace
                byronTxOutValues = concatMap (toList . (\tx -> map Byron.txOutValue (Byron.txOutputs $ Byron.taTx tx))) txs
                -- sum the TxOut values
                outSum = sum $ map Byron.lovelaceToInteger byronTxOutValues
            (newTxCount, outSum)
          Byron.ABOBBoundary _ ->
            -- it doesn't seem like ABOBBoundary handles tx's?
            (0, 0)
  convertToEpoch ceE cacheEpoch slotDetails newEpochTxCount newOutSum

convertToEpoch ::
  DB.Epoch ->
  CacheEpoch ->
  SlotDetails ->
  -- | tx count
  Int ->
  -- | sum of outTx values
  Integer ->
  DB.Epoch
convertToEpoch ceE cacheEpoch slotDetails newEpochTxCount outSum = do
  let cFees = ceFees cacheEpoch
      newBlkCount = fromIntegral $ DB.epochBlkCount ceE + 1
  DB.Epoch
    { DB.epochOutSum = fromIntegral (outSum + fromIntegral (DB.epochOutSum ceE))
    , DB.epochFees = DB.DbLovelace (DB.unDbLovelace (DB.epochFees ceE) + cFees)
    , DB.epochTxCount = fromIntegral (fromIntegral (DB.epochTxCount ceE) + newEpochTxCount)
    , DB.epochBlkCount = fromIntegral newBlkCount
    , DB.epochNo = unEpochNo (sdEpochNo slotDetails)
    , DB.epochStartTime = DB.epochStartTime ceE
    , DB.epochEndTime = sdSlotTime slotDetails
    }

-- -------------------------------------------------------------------------------------------------

-- | Get the PostgreSQL row index (EpochId) that matches the given epoch number.
queryForEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe DB.EpochId)
queryForEpochId epochNum = do
  res <- selectOne $ do
    epoch <- from $ table @DB.Epoch
    where_ (epoch ^. DB.EpochNo ==. val epochNum)
    pure (epoch ^. DB.EpochId)
  pure $ unValue <$> res

-- -- | Get an epoch given it's number.
-- queryEpochFromNum :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe DB.Epoch)
-- queryEpochFromNum epochNum = do
--   res <- selectOne $ do
--     epoch <- from $ table @DB.Epoch
--     where_ (epoch ^. DB.EpochNo ==. val epochNum)
--     pure epoch
--   pure $ entityVal <$> res

-- | Get the most recent epoch in the Epoch table.
queryLatestEpoch :: MonadIO m => ReaderT SqlBackend m (Maybe DB.Epoch)
queryLatestEpoch = do
  res <- selectOne $ do
    epoch <- from $ table @DB.Epoch
    orderBy [desc (epoch ^. DB.EpochNo)]
    pure epoch
  pure $ entityVal <$> res
