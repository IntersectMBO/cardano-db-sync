{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Insert (
  insertShelleyBlock,
) where

import Cardano.BM.Trace (Trace, logDebug, logInfo)
import Cardano.Db (DbWord64 (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (
  insertBlockAndCache,
  queryPoolKeyWithCache,
  queryPrevBlockWithCache,
 )
import Cardano.DbSync.Cache.Epoch (writeEpochBlockDiffToCache)
import Cardano.DbSync.Cache.Types (Cache (..), CacheNew (..), EpochBlockDiff (..))

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Insert.Epoch
import Cardano.DbSync.Era.Shelley.Insert.Grouped
import Cardano.DbSync.Era.Shelley.Insert.Tx (insertTx)
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.OffChain
import Cardano.DbSync.Types
import Cardano.DbSync.Util

import Cardano.DbSync.Era.Shelley.Insert.Certificate (insertPots)
import Cardano.DbSync.Era.Shelley.Insert.GovAction (insertCostModel, insertDrepDistr, updateEnacted)
import Cardano.DbSync.Era.Shelley.Insert.Other (toDouble)
import Cardano.DbSync.Era.Shelley.Insert.Pool (IsPoolMember)
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Conway.Core (DRepVotingThresholds (..), PoolVotingThresholds (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Keys
import Cardano.Prelude

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except.Extra (newExceptT)
import Data.Either.Extra (eitherToMaybe)
import Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Reduce duplication" -}

--------------------------------------------------------------------------------------------
-- Insert Block
--------------------------------------------------------------------------------------------
insertShelleyBlock ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Bool ->
  Bool ->
  Bool ->
  Generic.Block ->
  SlotDetails ->
  IsPoolMember ->
  ApplyResult ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertShelleyBlock syncEnv shouldLog withinTwoMins withinHalfHour blk details isMember applyResult = do
  runExceptT $ do
    pbid <- case Generic.blkPreviousHash blk of
      Nothing -> liftLookupFail (renderErrorMessage (Generic.blkEra blk)) DB.queryGenesis -- this is for networks that fork from Byron on epoch 0.
      Just pHash -> queryPrevBlockWithCache (renderErrorMessage (Generic.blkEra blk)) cache pHash
    mPhid <- lift $ queryPoolKeyWithCache cache CacheNew $ coerceKeyRole $ Generic.blkSlotLeader blk
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
      insertOnNewEpoch tracer iopts blkId (Generic.blkSlotNo blk) epochNo newEpoch

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
        Generic.Shelley -> "insertShelleyBlock"
        other -> mconcat ["insertShelleyBlock(", textShow other, ")"]

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    cache :: Cache
    cache = envCache syncEnv

--------------------------------------------------------------------------------------------
-- Insert Epoch
--------------------------------------------------------------------------------------------
insertOnNewEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  InsertOptions ->
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Generic.NewEpoch ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOnNewEpoch tracer iopts blkId slotNo epochNo newEpoch = do
  whenStrictJust (Generic.euProtoParams epochUpdate) $ \params ->
    lift $ insertEpochParam tracer blkId epochNo params (Generic.euNonce epochUpdate)
  whenStrictJust (Generic.neAdaPots newEpoch) $ \pots ->
    insertPots blkId slotNo epochNo pots
  whenStrictJust (Generic.neDRepState newEpoch) $ \dreps -> when (ioGov iopts) $ do
    let (drepSnapshot, ratifyState) = finishDRepPulser dreps
    lift $ insertDrepDistr epochNo drepSnapshot
    updateEnacted False epochNo (rsEnactState ratifyState)
  whenStrictJust (Generic.neEnacted newEpoch) $ \enactedSt ->
    when (ioGov iopts) $
      updateEnacted True epochNo enactedSt
  where
    epochUpdate :: Generic.EpochUpdate
    epochUpdate = Generic.neEpochUpdate newEpoch

insertEpochParam ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.BlockId ->
  EpochNo ->
  Generic.ProtoParams ->
  Ledger.Nonce ->
  ReaderT SqlBackend m ()
insertEpochParam _tracer blkId (EpochNo epoch) params nonce = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (Generic.ppCostmdls params)
  void
    . DB.insertEpochParam
    $ DB.EpochParam
      { DB.epochParamEpochNo = epoch
      , DB.epochParamMinFeeA = fromIntegral (Generic.ppMinfeeA params)
      , DB.epochParamMinFeeB = fromIntegral (Generic.ppMinfeeB params)
      , DB.epochParamMaxBlockSize = fromIntegral (Generic.ppMaxBBSize params)
      , DB.epochParamMaxTxSize = fromIntegral (Generic.ppMaxTxSize params)
      , DB.epochParamMaxBhSize = fromIntegral (Generic.ppMaxBHSize params)
      , DB.epochParamKeyDeposit = Generic.coinToDbLovelace (Generic.ppKeyDeposit params)
      , DB.epochParamPoolDeposit = Generic.coinToDbLovelace (Generic.ppPoolDeposit params)
      , DB.epochParamMaxEpoch = fromIntegral $ unEpochInterval (Generic.ppMaxEpoch params)
      , DB.epochParamOptimalPoolCount = fromIntegral (Generic.ppOptialPoolCount params)
      , DB.epochParamInfluence = fromRational (Generic.ppInfluence params)
      , DB.epochParamMonetaryExpandRate = toDouble (Generic.ppMonetaryExpandRate params)
      , DB.epochParamTreasuryGrowthRate = toDouble (Generic.ppTreasuryGrowthRate params)
      , DB.epochParamDecentralisation = toDouble (Generic.ppDecentralisation params)
      , DB.epochParamExtraEntropy = Generic.nonceToBytes $ Generic.ppExtraEntropy params
      , DB.epochParamProtocolMajor = getVersion $ Ledger.pvMajor (Generic.ppProtocolVersion params)
      , DB.epochParamProtocolMinor = fromIntegral $ Ledger.pvMinor (Generic.ppProtocolVersion params)
      , DB.epochParamMinUtxoValue = Generic.coinToDbLovelace (Generic.ppMinUTxOValue params)
      , DB.epochParamMinPoolCost = Generic.coinToDbLovelace (Generic.ppMinPoolCost params)
      , DB.epochParamNonce = Generic.nonceToBytes nonce
      , DB.epochParamCoinsPerUtxoSize = Generic.coinToDbLovelace <$> Generic.ppCoinsPerUtxo params
      , DB.epochParamCostModelId = cmId
      , DB.epochParamPriceMem = realToFrac <$> Generic.ppPriceMem params
      , DB.epochParamPriceStep = realToFrac <$> Generic.ppPriceStep params
      , DB.epochParamMaxTxExMem = DbWord64 <$> Generic.ppMaxTxExMem params
      , DB.epochParamMaxTxExSteps = DbWord64 <$> Generic.ppMaxTxExSteps params
      , DB.epochParamMaxBlockExMem = DbWord64 <$> Generic.ppMaxBlockExMem params
      , DB.epochParamMaxBlockExSteps = DbWord64 <$> Generic.ppMaxBlockExSteps params
      , DB.epochParamMaxValSize = DbWord64 . fromIntegral <$> Generic.ppMaxValSize params
      , DB.epochParamCollateralPercent = fromIntegral <$> Generic.ppCollateralPercentage params
      , DB.epochParamMaxCollateralInputs = fromIntegral <$> Generic.ppMaxCollateralInputs params
      , DB.epochParamPvtMotionNoConfidence = toDouble . pvtMotionNoConfidence <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtCommitteeNormal = toDouble . pvtCommitteeNormal <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtCommitteeNoConfidence = toDouble . pvtCommitteeNoConfidence <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtHardForkInitiation = toDouble . pvtHardForkInitiation <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamDvtMotionNoConfidence = toDouble . dvtMotionNoConfidence <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtCommitteeNormal = toDouble . dvtCommitteeNormal <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtCommitteeNoConfidence = toDouble . dvtCommitteeNoConfidence <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtUpdateToConstitution = toDouble . dvtUpdateToConstitution <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtHardForkInitiation = toDouble . dvtHardForkInitiation <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtPPNetworkGroup = toDouble . dvtPPNetworkGroup <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtPPEconomicGroup = toDouble . dvtPPEconomicGroup <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtPPTechnicalGroup = toDouble . dvtPPTechnicalGroup <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtPPGovGroup = toDouble . dvtPPGovGroup <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamDvtTreasuryWithdrawal = toDouble . dvtTreasuryWithdrawal <$> Generic.ppDRepVotingThresholds params
      , DB.epochParamCommitteeMinSize = DbWord64 . fromIntegral <$> Generic.ppCommitteeMinSize params
      , DB.epochParamCommitteeMaxTermLength = DbWord64 . fromIntegral . unEpochInterval <$> Generic.ppCommitteeMaxTermLength params
      , DB.epochParamGovActionLifetime = fromIntegral . unEpochInterval <$> Generic.ppGovActionLifetime params
      , DB.epochParamGovActionDeposit = DbWord64 . fromIntegral <$> Generic.ppGovActionDeposit params
      , DB.epochParamDrepDeposit = DbWord64 . fromIntegral <$> Generic.ppDRepDeposit params
      , DB.epochParamDrepActivity = fromIntegral . unEpochInterval <$> Generic.ppDRepActivity params
      , DB.epochParamBlockId = blkId
      }
