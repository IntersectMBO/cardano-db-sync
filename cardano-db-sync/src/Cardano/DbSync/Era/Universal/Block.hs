{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Block (
  prepareBlock,
  insertBlockUniversal,
) where

import Cardano.BM.Trace (logDebug, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (
  insertBlockAndCache,
  optimiseCaches,
  queryPoolKeyWithCache,
  queryPrevBlockWithCache,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.OffChain
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Keys
import Cardano.Prelude
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either.Extra (eitherToMaybe)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Database.Persist.Sql (SqlBackend)

--------------------------------------------------------------------------------------------
-- Insert a universal Block.
-- This is the entry point for inserting a block into the database, used for all eras appart from Byron.
--------------------------------------------------------------------------------------------

prepareBlock ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Generic.Block ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.Block
prepareBlock syncEnv blk = do
  pbid <- case Generic.blkPreviousHash blk of
    Nothing -> liftLookupFail (renderErrorMessage (Generic.blkEra blk)) DB.queryGenesis -- this is for networks that fork from Byron on epoch 0.
    Just pHash -> queryPrevBlockWithCache (renderErrorMessage (Generic.blkEra blk)) cache pHash
  mPhid <- lift $ queryPoolKeyWithCache cache UpdateCache $ coerceKeyRole $ Generic.blkSlotLeader blk
  slid <- lift . DB.insertSlotLeader $ Generic.mkSlotLeader (ioShelley iopts) (Generic.unKeyHashRaw $ Generic.blkSlotLeader blk) (eitherToMaybe mPhid)
  pure $
    DB.Block
      { DB.blockHash = Generic.blkHash blk
      , DB.blockEpochNo = Nothing
      , DB.blockSlotNo = Just $ unSlotNo (Generic.blkSlotNo blk)
      , DB.blockEpochSlotNo = Nothing
      , DB.blockBlockNo = Just $ unBlockNo (Generic.blkBlockNo blk)
      , DB.blockPreviousId = Just pbid
      , DB.blockSlotLeaderId = slid
      , DB.blockSize = Generic.blkSize blk
      , DB.blockTime = dummyUTCTime
      , DB.blockTxCount = fromIntegral $ length (Generic.blkTxs blk)
      , DB.blockProtoMajor = getVersion $ Ledger.pvMajor (Generic.blkProto blk)
      , DB.blockProtoMinor = fromIntegral $ Ledger.pvMinor (Generic.blkProto blk)
      , -- Shelley specific
        DB.blockVrfKey = Just $ Generic.blkVrfKey blk
      , DB.blockOpCert = Just $ Generic.blkOpCert blk
      , DB.blockOpCertCounter = Just $ Generic.blkOpCertCounter blk
      }
  where
    dummyUTCTime = UTCTime (ModifiedJulianDay 0) 0
    cache = envCache syncEnv
    iopts = getInsertOptions syncEnv

insertBlockUniversal ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  -- | Should log
  DB.BlockId ->
  Generic.Block ->
  DB.Block ->
  ApplyResult ->
  Bool ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertBlockUniversal syncEnv blkId genericBlock blk applyResult shouldLog = do
  when (isSyncedWithintwoMinutes details) $ lift $ optimiseCaches $ envCache syncEnv
  lift $
    insertBlockAndCache cache blkId $
      blk
        { DB.blockEpochNo = Just $ unEpochNo epochNo
        , DB.blockEpochSlotNo = Just $ unEpochSlot $ sdEpochSlot details
        , DB.blockTime = sdSlotTime details
        }

  {-} TODO
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
          , ebdEpochNo = unEpochNo epochNo
          , ebdOutSum = fromIntegral $ groupedTxOutSum blockGroupedData
          , ebdTxCount = fromIntegral $ length (Generic.blkTxs genericBlock)
          }

    when withinHalfHour $
      insertReverseIndex blkId minIds
  -}
  liftIO (logBlockMaybe >> logEpochProgressMaybe)

  whenStrictJust (apNewEpoch applyResult) $ \newEpoch -> do
    insertOnNewEpoch syncEnv blkId (Generic.blkSlotNo genericBlock) epochNo newEpoch

  when (ioGov iopts && (withinHalfHour || unBlockNo (Generic.blkBlockNo genericBlock) `mod` 10000 == 0))
    . lift
    $ insertOffChainVoteResults tracer (envOffChainVoteResultQueue syncEnv)

  when (ioOffChainPoolData iopts && (withinHalfHour || unBlockNo (Generic.blkBlockNo genericBlock) `mod` 10000 == 0))
    . lift
    $ insertOffChainPoolResults tracer (envOffChainPoolResultQueue syncEnv)
  where
    iopts = getInsertOptions syncEnv
    details = apSlotDetails applyResult
    epochNo = sdEpochNo details
    epoch = unEpochNo epochNo
    slotWithinEpoch = unEpochSlot (sdEpochSlot details)
    withinTwoMins = isSyncedWithinSeconds details 120 == SyncFollowing
    withinHalfHour = isSyncedWithinSeconds details 1800 == SyncFollowing

    tracer = getTrace syncEnv
    cache = envCache syncEnv

    logBlockMaybe :: IO ()
    logBlockMaybe
      | shouldLog = logInfo tracer logBlockMsg
      | withinTwoMins = logInfo tracer logBlockMsg
      | unBlockNo (Generic.blkBlockNo genericBlock) `mod` 5000 == 0 = logInfo tracer logBlockMsg
      | otherwise = logDebug tracer logBlockMsg

    logEpochProgressMaybe :: IO ()
    logEpochProgressMaybe =
      when (withinTwoMins && slotWithinEpoch /= 0 && unBlockNo (Generic.blkBlockNo genericBlock) `mod` 20 == 0) $ do
        logInfo tracer logContinueMsg

    logBlockMsg =
      mconcat
        [ renderInsertName (Generic.blkEra genericBlock)
        , ": epoch "
        , textShow (unEpochNo epochNo)
        , ", slot "
        , textShow (unSlotNo $ Generic.blkSlotNo genericBlock)
        , ", block "
        , textShow (unBlockNo $ Generic.blkBlockNo genericBlock)
        , ", hash "
        , renderByteArray (Generic.blkHash genericBlock)
        ]

    logContinueMsg =
      mconcat
        [ renderInsertName (Generic.blkEra genericBlock)
        , ": continuing epoch "
        , textShow epoch
        , " (slot "
        , textShow slotWithinEpoch
        , "/"
        , textShow (unEpochSize $ sdEpochSize details)
        , ")"
        ]

    renderInsertName :: Generic.BlockEra -> Text
    renderInsertName eraText =
      mconcat ["Insert ", textShow eraText, " Block"]

renderErrorMessage :: Generic.BlockEra -> Text
renderErrorMessage eraText =
  case eraText of
    Generic.Shelley -> "insertBlockForEra"
    other -> mconcat ["insertBlockForEra(", textShow other, ")"]
