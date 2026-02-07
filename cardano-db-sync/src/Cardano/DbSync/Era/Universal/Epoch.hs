{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Epoch (
  insertOnNewEpoch,
  insertRewards,
  hasNewEpochEvent,
  hasEpochStartEvent,
  insertRewardRests,
  insertProposalRefunds,
  insertPoolDepositRefunds,
  insertStakeSlice,
) where

import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (Network, unEpochInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Binary.Version (getVersion)
import qualified Cardano.Ledger.Coin as Shelley
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Core (PoolVotingThresholds (..))
import Cardano.Ledger.Conway.Governance (finishDRepPulser)
import qualified Cardano.Ledger.Conway.Governance.DRepPulser as Ledger
import Cardano.Ledger.Conway.PParams (DRepVotingThresholds (..))
import Cardano.Ledger.Conway.Rules (RatifyState (..))
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo)
import System.Mem (performMinorGC)

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (queryOrInsertStakeAddress, queryPoolKeyOrInsert)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.Certificate (insertPots)
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCostModel, insertDrepDistr, insertUpdateEnacted, updateExpired, updateRatified)
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Error (SyncNodeError)
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types
import Cardano.DbSync.Util (whenDefault, whenStrictJust, whenStrictJustDefault)

{- HLINT ignore "Use readTVarIO" -}

--------------------------------------------------------------------------------------------
-- Insert Epoch
--------------------------------------------------------------------------------------------
insertOnNewEpoch ::
  SyncEnv ->
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Generic.NewEpoch ->
  ExceptT SyncNodeError DB.DbM ()
insertOnNewEpoch syncEnv blkId slotNo epochNo newEpoch = do
  whenStrictJust (Generic.euProtoParams epochUpdate) $ \params ->
    insertEpochParam tracer blkId epochNo params (Generic.euNonce epochUpdate)
  whenStrictJust (Generic.neAdaPots newEpoch) $ \pots ->
    insertPots blkId slotNo epochNo pots
  spoVoting <- whenStrictJustDefault Map.empty (Generic.neDRepState newEpoch) $ \dreps -> whenDefault Map.empty (ioGov iopts) $ do
    let (drepSnapshot, ratifyState) = finishDRepPulser dreps
    insertDrepDistr epochNo drepSnapshot
    updateRatified syncEnv epochNo (toList $ rsEnacted ratifyState)
    updateExpired syncEnv epochNo (toList $ rsExpired ratifyState)
    pure (Ledger.psPoolDistr drepSnapshot)
  whenStrictJust (Generic.neEnacted newEpoch) $ \enactedSt -> do
    when (ioGov iopts) $ do
      insertUpdateEnacted syncEnv blkId epochNo enactedSt
  whenStrictJust (Generic.nePoolDistr newEpoch) $ \(poolDistrDeleg, poolDistrNBlocks) ->
    when (ioPoolStats iopts) $ do
      let nothingMap = Map.fromList $ (,Nothing) <$> (Map.keys poolDistrNBlocks <> Map.keys spoVoting)
      let mapWithAllKeys = Map.union (Map.map Just poolDistrDeleg) nothingMap
      let poolStats = Map.mapWithKey (mkPoolStats poolDistrNBlocks spoVoting) mapWithAllKeys
      insertPoolStats syncEnv epochNo poolStats
  where
    epochUpdate :: Generic.EpochUpdate
    epochUpdate = Generic.neEpochUpdate newEpoch

    mkPoolStats :: Map PoolKeyHash Natural -> Map PoolKeyHash (Shelley.CompactForm Shelley.Coin) -> PoolKeyHash -> Maybe (Shelley.Coin, Word64) -> Generic.PoolStats
    mkPoolStats blocks voting pkh deleg =
      Generic.PoolStats
        { Generic.nBlocks = fromMaybe 0 (Map.lookup pkh blocks)
        , Generic.nDelegators = maybe 0 snd deleg
        , Generic.stake = maybe (Shelley.Coin 0) fst deleg
        , Generic.votingPower = fromCompact <$> Map.lookup pkh voting
        }
    tracer = getTrace syncEnv
    iopts = getInsertOptions syncEnv

insertEpochParam ::
  Trace IO Text ->
  DB.BlockId ->
  EpochNo ->
  Generic.ProtoParams ->
  Ledger.Nonce ->
  ExceptT SyncNodeError DB.DbM ()
insertEpochParam _tracer blkId (EpochNo epoch) params nonce = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (Generic.ppCostmdls params)
  void
    . lift
    $ DB.insertEpochParam
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
      , DB.epochParamOptimalPoolCount = fromIntegral (Generic.ppOptimalPoolCount params)
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
      , DB.epochParamMinFeeRefScriptCostPerByte = fromRational <$> Generic.ppMinFeeRefScriptCostPerByte params
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
  SyncEnv ->
  Generic.StakeSliceRes ->
  ExceptT SyncNodeError DB.DbM ()
insertStakeSlice _ Generic.NoSlices = pure ()
insertStakeSlice syncEnv (Generic.Slice slice finalSlice) = do
  insertEpochStake syncEnv network (Generic.sliceEpochNo slice) (Map.toList $ Generic.sliceDistr slice)
  when finalSlice $ do
    lift $ DB.updateStakeProgressCompleted $ unEpochNo $ Generic.sliceEpochNo slice
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
  SyncEnv ->
  Network ->
  EpochNo ->
  [(StakeCred, (Shelley.Coin, PoolKeyHash))] ->
  ExceptT SyncNodeError DB.DbM ()
insertEpochStake syncEnv nw epochNo stakeChunk = do
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbStakes <- mapM mkStake stakeChunk
  let chunckDbStakes = DB.chunkForBulkQuery (Proxy @DB.EpochStake) Nothing dbStakes

  -- minimising the bulk inserts into hundred thousand chunks to improve performance with pipeline
  lift $ DB.insertBulkEpochStakePiped dbConstraintEpochStake chunckDbStakes

  liftIO performMinorGC
  where
    mkStake ::
      (StakeCred, (Shelley.Coin, PoolKeyHash)) ->
      ExceptT SyncNodeError DB.DbM DB.EpochStake
    mkStake (saddr, (coin, pool)) = do
      saId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong nw saddr
      poolId <- queryPoolKeyOrInsert syncEnv "insertEpochStake" UpdateCache (ioShelley iopts) pool
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = unEpochNo epochNo -- The epoch where this delegation becomes valid.
          }

    iopts = getInsertOptions syncEnv

insertRewards ::
  SyncEnv ->
  Network ->
  EpochNo ->
  EpochNo ->
  [(StakeCred, Set Generic.Reward)] ->
  ExceptT SyncNodeError DB.DbM ()
insertRewards syncEnv nw earnedEpoch spendableEpoch rewardsChunk = do
  dbRewards <- concatMapM mkRewards rewardsChunk
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  let chunckDbRewards = DB.chunkForBulkQuery (Proxy @DB.Reward) Nothing dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance with pipeline
  lift $ DB.insertBulkRewardsPiped dbConstraintRewards chunckDbRewards

  liftIO performMinorGC
  where
    mkRewards ::
      (StakeCred, Set Generic.Reward) ->
      ExceptT SyncNodeError DB.DbM [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong nw saddr
      mapM (prepareReward saId) (Set.toList rset)

    prepareReward ::
      DB.StakeAddressId ->
      Generic.Reward ->
      ExceptT SyncNodeError DB.DbM DB.Reward
    prepareReward saId rwd = do
      poolId <- queryPool (Generic.rewardPool rwd)
      pure $
        DB.Reward
          { DB.rewardAddrId = saId
          , DB.rewardType = Generic.rewardSource rwd
          , DB.rewardAmount = DB.DbLovelace (Generic.rewardAmount rwd)
          , DB.rewardEarnedEpoch = unEpochNo earnedEpoch
          , DB.rewardSpendableEpoch = unEpochNo spendableEpoch
          , DB.rewardPoolId = poolId
          }

    queryPool ::
      PoolKeyHash ->
      ExceptT SyncNodeError DB.DbM DB.PoolHashId
    queryPool =
      queryPoolKeyOrInsert syncEnv "insertRewards" UpdateCache (ioShelley iopts)

    iopts = getInsertOptions syncEnv

insertRewardRests ::
  SyncEnv ->
  Network ->
  EpochNo ->
  EpochNo ->
  [(StakeCred, Set Generic.RewardRest)] ->
  ExceptT SyncNodeError DB.DbM ()
insertRewardRests syncEnv nw earnedEpoch spendableEpoch rewardsChunk = do
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = DB.chunkForBulkQuery (Proxy @DB.RewardRest) Nothing dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance with pipeline
  lift $ DB.insertBulkRewardRestsPiped chunckDbRewards
  where
    mkRewards ::
      (StakeCred, Set Generic.RewardRest) ->
      ExceptT SyncNodeError DB.DbM [DB.RewardRest]
    mkRewards (saddr, rset) = do
      saId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong nw saddr
      pure $ map (prepareReward saId) (Set.toList rset)

    prepareReward ::
      DB.StakeAddressId ->
      Generic.RewardRest ->
      DB.RewardRest
    prepareReward saId rwd =
      DB.RewardRest
        { DB.rewardRestAddrId = saId
        , DB.rewardRestType = Generic.irSource rwd
        , DB.rewardRestAmount = Generic.coinToDbLovelace (Generic.irAmount rwd)
        , DB.rewardRestEarnedEpoch = unEpochNo earnedEpoch
        , DB.rewardRestSpendableEpoch = unEpochNo spendableEpoch
        }

insertProposalRefunds ::
  SyncEnv ->
  Network ->
  EpochNo ->
  EpochNo ->
  [GovActionRefunded] ->
  ExceptT SyncNodeError DB.DbM ()
insertProposalRefunds syncEnv nw earnedEpoch spendableEpoch refunds = do
  dbRewards <- mapM mkReward refunds
  lift $ DB.insertBulkRewardRests dbRewards
  where
    mkReward ::
      GovActionRefunded ->
      ExceptT SyncNodeError DB.DbM DB.RewardRest
    mkReward refund = do
      saId <- queryOrInsertStakeAddress syncEnv UpdateCacheStrong nw (raCredential $ garReturnAddr refund)
      pure $
        DB.RewardRest
          { DB.rewardRestAddrId = saId
          , DB.rewardRestType = DB.RwdProposalRefund
          , DB.rewardRestAmount = Generic.coinToDbLovelace (garDeposit refund)
          , DB.rewardRestEarnedEpoch = unEpochNo earnedEpoch
          , DB.rewardRestSpendableEpoch = unEpochNo spendableEpoch
          }

insertPoolDepositRefunds ::
  SyncEnv ->
  EpochNo ->
  Generic.Rewards ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolDepositRefunds syncEnv epochNo refunds = do
  insertRewards syncEnv nw epochNo epochNo (Map.toList rwds)
  liftIO . logInfo tracer $ "Inserted " <> show (Generic.rewardsCount refunds) <> " deposit refund rewards"
  where
    tracer = getTrace syncEnv
    rwds = Generic.unRewards refunds
    nw = getNetwork syncEnv

insertPoolStats ::
  SyncEnv ->
  EpochNo ->
  Map PoolKeyHash Generic.PoolStats ->
  ExceptT SyncNodeError DB.DbM ()
insertPoolStats syncEnv epochNo mp = do
  poolStats <- mapM preparePoolStat $ Map.toList mp
  lift $ DB.insertBulkPoolStat poolStats
  where
    preparePoolStat :: (PoolKeyHash, Generic.PoolStats) -> ExceptT SyncNodeError DB.DbM DB.PoolStat
    preparePoolStat (pkh, ps) = do
      poolId <- queryPoolKeyOrInsert syncEnv "insertPoolStats" UpdateCache True pkh
      pure
        DB.PoolStat
          { DB.poolStatPoolHashId = poolId
          , DB.poolStatEpochNo = unEpochNo epochNo
          , DB.poolStatNumberOfBlocks = fromIntegral $ Generic.nBlocks ps
          , DB.poolStatNumberOfDelegators = fromIntegral $ Generic.nDelegators ps
          , DB.poolStatStake = fromIntegral . Shelley.unCoin $ Generic.stake ps
          , DB.poolStatVotingPower = fromIntegral . Shelley.unCoin <$> Generic.votingPower ps
          }
