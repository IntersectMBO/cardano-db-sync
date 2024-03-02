{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Epoch (
  insertOnNewEpoch,
  insertRewards,
  hasNewEpochEvent,
  hasEpochStartEvent,
  insertInstantRewards,
  insertPoolDepositRefunds,
  insertStakeSlice,
  sumRewardTotal,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (queryOrInsertStakeAddress, queryPoolKeyOrInsert)
import Cardano.DbSync.Cache.Types (Cache, CacheNew (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.Certificate (insertPots)
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCostModel, insertDrepDistr, updateEnacted)
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Event (LedgerEvent (..))
import Cardano.DbSync.Types
import Cardano.DbSync.Util (whenStrictJust)
import Cardano.DbSync.Util.Constraint (constraintNameEpochStake, constraintNameReward)
import Cardano.Ledger.BaseTypes (Network, unEpochInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Binary.Version (getVersion)
import qualified Cardano.Ledger.Coin as Shelley
import Cardano.Ledger.Conway.Core (PoolVotingThresholds (..))
import Cardano.Ledger.Conway.Governance (finishDRepPulser)
import Cardano.Ledger.Conway.PParams (DRepVotingThresholds (..))
import Cardano.Ledger.Conway.Rules (RatifyState (..))
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Use readTVarIO" -}

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
      , DB.epochParamMaxTxExMem = DB.DbWord64 <$> Generic.ppMaxTxExMem params
      , DB.epochParamMaxTxExSteps = DB.DbWord64 <$> Generic.ppMaxTxExSteps params
      , DB.epochParamMaxBlockExMem = DB.DbWord64 <$> Generic.ppMaxBlockExMem params
      , DB.epochParamMaxBlockExSteps = DB.DbWord64 <$> Generic.ppMaxBlockExSteps params
      , DB.epochParamMaxValSize = DB.DbWord64 . fromIntegral <$> Generic.ppMaxValSize params
      , DB.epochParamCollateralPercent = fromIntegral <$> Generic.ppCollateralPercentage params
      , DB.epochParamMaxCollateralInputs = fromIntegral <$> Generic.ppMaxCollateralInputs params
      , DB.epochParamPvtMotionNoConfidence = toDouble . pvtMotionNoConfidence <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtCommitteeNormal = toDouble . pvtCommitteeNormal <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtCommitteeNoConfidence = toDouble . pvtCommitteeNoConfidence <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtHardForkInitiation = toDouble . pvtHardForkInitiation <$> Generic.ppPoolVotingThresholds params
      , DB.epochParamPvtppSecurityGroup = toDouble . pvtPPSecurityGroup <$> Generic.ppPoolVotingThresholds params
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
      , DB.epochParamCommitteeMinSize = DB.DbWord64 . fromIntegral <$> Generic.ppCommitteeMinSize params
      , DB.epochParamCommitteeMaxTermLength = DB.DbWord64 . fromIntegral . unEpochInterval <$> Generic.ppCommitteeMaxTermLength params
      , DB.epochParamGovActionLifetime = fromIntegral . unEpochInterval <$> Generic.ppGovActionLifetime params
      , DB.epochParamGovActionDeposit = DB.DbWord64 . fromIntegral <$> Generic.ppGovActionDeposit params
      , DB.epochParamDrepDeposit = DB.DbWord64 . fromIntegral <$> Generic.ppDRepDeposit params
      , DB.epochParamDrepActivity = fromIntegral . unEpochInterval <$> Generic.ppDRepActivity params
      , DB.epochParamBlockId = blkId
      }

hasNewEpochEvent :: [LedgerEvent] -> Bool
hasNewEpochEvent = any isNewEpoch
  where
    isNewEpoch :: LedgerEvent -> Bool
    isNewEpoch le =
      case le of
        LedgerNewEpoch {} -> True
        _otherwise -> False

hasEpochStartEvent :: [LedgerEvent] -> Bool
hasEpochStartEvent = any isNewEpoch
  where
    isNewEpoch :: LedgerEvent -> Bool
    isNewEpoch le =
      case le of
        LedgerNewEpoch {} -> True
        LedgerStartAtEpoch {} -> True
        _otherwise -> False

insertStakeSlice ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Generic.StakeSliceRes ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeSlice _ Generic.NoSlices = pure ()
insertStakeSlice syncEnv (Generic.Slice slice finalSlice) = do
  insertEpochStake syncEnv network (Generic.sliceEpochNo slice) (Map.toList $ Generic.sliceDistr slice)
  when finalSlice $ do
    lift $ DB.updateSetComplete $ unEpochNo $ Generic.sliceEpochNo slice
    size <- lift $ DB.queryEpochStakeCount (unEpochNo $ Generic.sliceEpochNo slice)
    liftIO
      . logInfo tracer
      $ mconcat ["Inserted ", show size, " EpochStake for ", show (Generic.sliceEpochNo slice)]
  where
    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    network :: Network
    network = getNetwork syncEnv

insertEpochStake ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Network ->
  EpochNo ->
  [(StakeCred, (Shelley.Coin, PoolKeyHash))] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake syncEnv nw epochNo stakeChunk = do
  let cache = envCache syncEnv
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbStakes <- mapM (mkStake cache) stakeChunk
  let chunckDbStakes = splittRecordsEvery 100000 dbStakes
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbStakes $ \dbs -> lift $ DB.insertManyEpochStakes dbConstraintEpochStake constraintNameEpochStake dbs
  where
    mkStake ::
      (MonadBaseControl IO m, MonadIO m) =>
      Cache ->
      (StakeCred, (Shelley.Coin, PoolKeyHash)) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake cache (saddr, (coin, pool)) = do
      saId <- lift $ queryOrInsertStakeAddress cache CacheNew nw saddr
      poolId <- lift $ queryPoolKeyOrInsert "insertEpochStake" trce cache CacheNew (ioShelley iopts) pool
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = unEpochNo epochNo -- The epoch where this delegation becomes valid.
          }

    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

insertRewards ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Network ->
  EpochNo ->
  EpochNo ->
  Cache ->
  [(StakeCred, Set Generic.Reward)] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards syncEnv nw earnedEpoch spendableEpoch cache rewardsChunk = do
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = splittRecordsEvery 100000 dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbRewards $ \rws -> lift $ DB.insertManyRewards dbConstraintRewards constraintNameReward rws
  where
    mkRewards ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Set Generic.Reward) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- lift $ queryOrInsertStakeAddress cache CacheNew nw saddr
      mapM (prepareReward saId) (Set.toList rset)

    prepareReward ::
      (MonadBaseControl IO m, MonadIO m) =>
      DB.StakeAddressId ->
      Generic.Reward ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.Reward
    prepareReward saId rwd = do
      poolId <- queryPool (Generic.rewardPool rwd)
      pure $
        DB.Reward
          { DB.rewardAddrId = saId
          , DB.rewardType = Generic.rewardSource rwd
          , DB.rewardAmount = Generic.coinToDbLovelace (Generic.rewardAmount rwd)
          , DB.rewardEarnedEpoch = unEpochNo earnedEpoch
          , DB.rewardSpendableEpoch = unEpochNo spendableEpoch
          , DB.rewardPoolId = poolId
          }

    queryPool ::
      (MonadBaseControl IO m, MonadIO m) =>
      PoolKeyHash ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolHashId
    queryPool poolHash =
      lift (queryPoolKeyOrInsert "insertRewards" trce cache CacheNew (ioShelley iopts) poolHash)

    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

insertInstantRewards ::
  (MonadBaseControl IO m, MonadIO m) =>
  Network ->
  EpochNo ->
  EpochNo ->
  Cache ->
  [(StakeCred, Set Generic.InstantReward)] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertInstantRewards nw earnedEpoch spendableEpoch cache rewardsChunk = do
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = splittRecordsEvery 100000 dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbRewards $ \rws -> lift $ DB.insertManyInstantRewards rws
  where
    mkRewards ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Set Generic.InstantReward) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.InstantReward]
    mkRewards (saddr, rset) = do
      saId <- lift $ queryOrInsertStakeAddress cache CacheNew nw saddr
      pure $ map (prepareReward saId) (Set.toList rset)

    prepareReward ::
      DB.StakeAddressId ->
      Generic.InstantReward ->
      DB.InstantReward
    prepareReward saId rwd =
      DB.InstantReward
        { DB.instantRewardAddrId = saId
        , DB.instantRewardType = Generic.irSource rwd
        , DB.instantRewardAmount = Generic.coinToDbLovelace (Generic.irAmount rwd)
        , DB.instantRewardEarnedEpoch = unEpochNo earnedEpoch
        , DB.instantRewardSpendableEpoch = unEpochNo spendableEpoch
        }

splittRecordsEvery :: Int -> [a] -> [[a]]
splittRecordsEvery val = go
  where
    go [] = []
    go ys =
      let (as, bs) = splitAt val ys
       in as : go bs

insertPoolDepositRefunds ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  EpochNo ->
  Generic.Rewards ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolDepositRefunds syncEnv epochNo refunds = do
  insertRewards syncEnv nw epochNo epochNo (envCache syncEnv) (Map.toList rwds)
  liftIO . logInfo tracer $ "Inserted " <> show (Generic.rewardsCount refunds) <> " deposit refund rewards"
  where
    tracer = getTrace syncEnv
    rwds = Generic.unRewards refunds
    nw = getNetwork syncEnv

sumRewardTotal :: Map StakeCred (Set Generic.Reward) -> Shelley.Coin
sumRewardTotal =
  Shelley.Coin . Map.foldl' sumCoin 0
  where
    sumCoin :: Integer -> Set Generic.Reward -> Integer
    sumCoin !acc sr =
      acc + sum (map (Shelley.unCoin . Generic.rewardAmount) $ Set.toList sr)
