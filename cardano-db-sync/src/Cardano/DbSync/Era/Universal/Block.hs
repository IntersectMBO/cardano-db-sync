{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Block (
  insertBlockUniversal,
) where

import Cardano.BM.Trace (Trace, logDebug, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (
  insertBlockAndCache,
  queryPoolKeyWithCache,
  queryPrevBlockWithCache,
 )
import Cardano.DbSync.Cache.Epoch (writeEpochBlockDiffToCache)
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheStatus (..), EpochBlockDiff (..))

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Universal.Insert.Tx (insertTx)
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.OffChain
import Cardano.DbSync.Types
import Cardano.DbSync.Util

import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember)
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Keys
import Cardano.Prelude

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except.Extra (newExceptT)
import Data.Either.Extra (eitherToMaybe)
import Database.Persist.Sql (SqlBackend)

--------------------------------------------------------------------------------------------
-- Insert a universal Block.
-- This is the entry point for inserting a block into the database, used for all eras appart from Byron.
--------------------------------------------------------------------------------------------
insertBlockUniversal ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  -- | Should log
  Bool ->
  -- | Within two minutes
  Bool ->
  -- | Within half hour
  Bool ->
  Generic.Block ->
  SlotDetails ->
  IsPoolMember ->
  ApplyResult ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertBlockUniversal syncEnv shouldLog withinTwoMins withinHalfHour blk details isMember applyResult = do
  runExceptT $ do
    pbid <- case Generic.blkPreviousHash blk of
      Nothing -> liftLookupFail (renderErrorMessage (Generic.blkEra blk)) DB.queryGenesis -- this is for networks that fork from Byron on epoch 0.
      Just pHash -> queryPrevBlockWithCache (renderErrorMessage (Generic.blkEra blk)) cache pHash
    mPhid <- lift $ queryPoolKeyWithCache cache UpdateCache $ coerceKeyRole $ Generic.blkSlotLeader blk
    let epochNo = sdEpochNo details

    slid <- lift . DB.insertSlotLeader $ Generic.mkSlotLeader (ioShelley iopts) (Generic.unKeyHashRaw $ Generic.blkSlotLeader blk) (eitherToMaybe mPhid)
    blkId <-
      lift . insertBlockAndCache cache $
        DB.Block
          { DB.blockHash = Generic.blkHash blk
          , DB.blockEpochNo = Just $ unEpochNo epochNo
          , DB.blockSlotNo = Just $ unSlotNo (Generic.blkSlotNo blk)
          , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
          , DB.blockBlockNo = Just $ unBlockNo (Generic.blkBlockNo blk)
          , DB.blockPreviousId = Just pbid
          , DB.blockSlotLeaderId = slid
          , DB.blockSize = Generic.blkSize blk
          , DB.blockTime = sdSlotTime details
          , DB.blockTxCount = fromIntegral $ length (Generic.blkTxs blk)
          , DB.blockProtoMajor = getVersion $ Ledger.pvMajor (Generic.blkProto blk)
          , DB.blockProtoMinor = fromIntegral $ Ledger.pvMinor (Generic.blkProto blk)
          , -- Shelley specific
            DB.blockVrfKey = Just $ Generic.blkVrfKey blk
          , DB.blockOpCert = Just $ Generic.blkOpCert blk
          , DB.blockOpCertCounter = Just $ Generic.blkOpCertCounter blk
          }

    let zippedTx = zip [0 ..] (Generic.blkTxs blk)
    let txInserter = insertTx syncEnv isMember blkId (sdEpochNo details) (Generic.blkSlotNo blk) applyResult
    blockGroupedData <- foldM (\gp (idx, tx) -> txInserter idx tx gp) mempty zippedTx
    minIds <- insertBlockGroupedData syncEnv blockGroupedData

    -- now that we've inserted the Block and all it's txs lets cache what we'll need
    -- when we later update the epoch values.
    -- if have --dissable-epoch && --dissable-cache then no need to cache data.
    when (soptEpochAndCacheEnabled $ envOptions syncEnv)
      . newExceptT
      $ writeEpochBlockDiffToCache
        cache
        EpochBlockDiff
          { ebdBlockId = blkId
          , ebdTime = sdSlotTime details
          , ebdFees = groupedTxFees blockGroupedData
          , ebdEpochNo = unEpochNo (sdEpochNo details)
          , ebdOutSum = fromIntegral $ groupedTxOutSum blockGroupedData
          , ebdTxCount = fromIntegral $ length (Generic.blkTxs blk)
          }

    when withinHalfHour $
      insertReverseIndex blkId minIds

    liftIO $ do
      let epoch = unEpochNo epochNo
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)

      when (withinTwoMins && slotWithinEpoch /= 0 && unBlockNo (Generic.blkBlockNo blk) `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ renderInsertName (Generic.blkEra blk)
            , ": continuing epoch "
            , textShow epoch
            , " (slot "
            , textShow slotWithinEpoch
            , "/"
            , textShow (unEpochSize $ sdEpochSize details)
            , ")"
            ]
      logger tracer $
        mconcat
          [ renderInsertName (Generic.blkEra blk)
          , ": epoch "
          , textShow (unEpochNo epochNo)
          , ", slot "
          , textShow (unSlotNo $ Generic.blkSlotNo blk)
          , ", block "
          , textShow (unBlockNo $ Generic.blkBlockNo blk)
          , ", hash "
          , renderByteArray (Generic.blkHash blk)
          ]

    whenStrictJust (apNewEpoch applyResult) $ \newEpoch -> do
      insertOnNewEpoch syncEnv blkId (Generic.blkSlotNo blk) epochNo newEpoch

    insertStakeSlice syncEnv $ apStakeSlice applyResult

    when (ioGov iopts && (withinHalfHour || unBlockNo (Generic.blkBlockNo blk) `mod` 10000 == 0))
      . lift
      $ insertOffChainVoteResults tracer (envOffChainVoteResultQueue syncEnv)

    when (ioOffChainPoolData iopts && (withinHalfHour || unBlockNo (Generic.blkBlockNo blk) `mod` 10000 == 0))
      . lift
      $ insertOffChainPoolResults tracer (envOffChainPoolResultQueue syncEnv)
  where
    iopts = getInsertOptions syncEnv

    logger :: Trace IO a -> a -> IO ()
    logger
      | shouldLog = logInfo
      | withinTwoMins = logInfo
      | unBlockNo (Generic.blkBlockNo blk) `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

    renderInsertName :: Generic.BlockEra -> Text
    renderInsertName eraText =
      mconcat ["Insert ", textShow eraText, " Block"]

    renderErrorMessage :: Generic.BlockEra -> Text
    renderErrorMessage eraText =
      case eraText of
        Generic.Shelley -> "insertBlockForEra"
        other -> mconcat ["insertBlockForEra(", textShow other, ")"]

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    cache :: CacheStatus
    cache = envCache syncEnv
