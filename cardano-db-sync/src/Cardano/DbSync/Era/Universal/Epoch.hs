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
  insertRewardRests,
  insertProposalRefunds,
  insertPoolDepositRefunds,
  insertStakeSlice,
  sumRewardTotal,
) where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, InsertOptions (..), MonadAppDB (..), SyncEnv (..), askInsertOptions, askNetwork, askTrace)
import Cardano.DbSync.Cache (queryOrInsertStakeAddress, queryPoolKeyOrInsert)
import Cardano.DbSync.Cache.Types (UpdateCache (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Insert.Certificate (insertPots)
import Cardano.DbSync.Era.Universal.Insert.GovAction (insertCostModel, insertDrepDistr, insertUpdateEnacted, updateExpired, updateRatified)
import Cardano.DbSync.Era.Universal.Insert.Other (toDouble)
import Cardano.DbSync.Ledger.Event
import Cardano.DbSync.Types
import Cardano.DbSync.Util (whenStrictJust)
import Cardano.DbSync.Util.Constraint (constraintNameEpochStake, constraintNameReward)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (unEpochInterval)
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{- HLINT ignore "Use readTVarIO" -}

--------------------------------------------------------------------------------------------
-- Insert Epoch
--------------------------------------------------------------------------------------------
insertOnNewEpoch ::
  InsertOptions ->
  DB.BlockId ->
  SlotNo ->
  EpochNo ->
  Generic.NewEpoch ->
  App ()
insertOnNewEpoch iopts blkId slotNo epochNo newEpoch = do
  whenStrictJust (Generic.euProtoParams epochUpdate) $ \params ->
    insertEpochParam blkId epochNo params (Generic.euNonce epochUpdate)
  whenStrictJust (Generic.neAdaPots newEpoch) $ \pots ->
    insertPots blkId slotNo epochNo pots
  whenStrictJust (Generic.neDRepState newEpoch) $ \dreps -> when (ioGov iopts) $ do
    let (drepSnapshot, ratifyState) = finishDRepPulser dreps
    insertDrepDistr epochNo drepSnapshot
    updateRatified epochNo (toList $ rsEnacted ratifyState)
    updateExpired epochNo (toList $ rsExpired ratifyState)
  whenStrictJust (Generic.neEnacted newEpoch) $ \enactedSt -> do
    when (ioGov iopts) $ do
      insertUpdateEnacted blkId epochNo enactedSt
  where
    epochUpdate :: Generic.EpochUpdate
    epochUpdate = Generic.neEpochUpdate newEpoch

insertEpochParam ::
  DB.BlockId ->
  EpochNo ->
  Generic.ProtoParams ->
  Ledger.Nonce ->
  App ()
insertEpochParam blkId (EpochNo epoch) params nonce = do
  cmId <- maybe (pure Nothing) (fmap Just . insertCostModel blkId) (Generic.ppCostmdls params)
  void
    $ dbQueryToApp
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
  Generic.StakeSliceRes ->
  App ()
insertStakeSlice Generic.NoSlices = pure ()
insertStakeSlice (Generic.Slice slice finalSlice) = do
  tracer <- askTrace
  insertEpochStake (Generic.sliceEpochNo slice) (Map.toList $ Generic.sliceDistr slice)
  when finalSlice $ do
    dbQueryToApp $ DB.updateSetComplete $ unEpochNo $ Generic.sliceEpochNo slice
    size <- dbQueryToApp $ DB.queryEpochStakeCount (unEpochNo $ Generic.sliceEpochNo slice)
    liftIO
      . logInfo tracer
      $ mconcat ["Inserted ", show size, " EpochStake for ", show (Generic.sliceEpochNo slice)]

insertEpochStake ::
  EpochNo ->
  [(StakeCred, (Shelley.Coin, PoolKeyHash))] ->
  App ()
insertEpochStake epochNo stakeChunk = do
  syncEnv <- ask
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbStakes <- mapM mkStake stakeChunk
  let chunckDbStakes = splittRecordsEvery 100000 dbStakes
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbStakes $ \dbs -> dbQueryToApp $ DB.insertManyEpochStakes dbConstraintEpochStake constraintNameEpochStake dbs
  where
    mkStake ::
      (StakeCred, (Shelley.Coin, PoolKeyHash)) ->
      App DB.EpochStake
    mkStake (saddr, (coin, pool)) = do
      iopts <- askInsertOptions
      network <- askNetwork
      cache <- asks envCache
      saId <- queryOrInsertStakeAddress cache UpdateCache network saddr
      poolId <- queryPoolKeyOrInsert "insertEpochStake" cache UpdateCache (ioShelley iopts) pool
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = unEpochNo epochNo -- The epoch where this delegation becomes valid.
          }

insertRewards ::
  EpochNo ->
  EpochNo ->
  [(StakeCred, Set Generic.Reward)] ->
  App ()
insertRewards earnedEpoch spendableEpoch rewardsChunk = do
  syncEnv <- ask
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = splittRecordsEvery 100000 dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbRewards $ \rws -> dbQueryToApp $ DB.insertManyRewards dbConstraintRewards constraintNameReward rws
  where
    mkRewards ::
      (StakeCred, Set Generic.Reward) ->
      App [DB.Reward]
    mkRewards (saddr, rset) = do
      network <- askNetwork
      cache <- asks envCache
      saId <- queryOrInsertStakeAddress cache UpdateCache network saddr
      mapM (prepareReward saId) (Set.toList rset)

    prepareReward ::
      DB.StakeAddressId ->
      Generic.Reward ->
      App DB.Reward
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
      PoolKeyHash ->
      App DB.PoolHashId
    queryPool poolHash = do
      iopts <- askInsertOptions
      cache <- asks envCache
      queryPoolKeyOrInsert "insertRewards" cache UpdateCache (ioShelley iopts) poolHash

insertRewardRests ::
  EpochNo ->
  EpochNo ->
  [(StakeCred, Set Generic.RewardRest)] ->
  App ()
insertRewardRests earnedEpoch spendableEpoch rewardsChunk = do
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = splittRecordsEvery 100000 dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbRewards $ \rws -> dbQueryToApp $ DB.insertManyRewardRests rws
  where
    mkRewards ::
      (StakeCred, Set Generic.RewardRest) ->
      App [DB.RewardRest]
    mkRewards (saddr, rset) = do
      network <- askNetwork
      cache <- asks envCache
      saId <- queryOrInsertStakeAddress cache UpdateCache network saddr
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
  EpochNo ->
  EpochNo ->
  [GovActionRefunded] ->
  App ()
insertProposalRefunds earnedEpoch spendableEpoch refunds = do
  dbRewards <- mapM mkReward refunds
  dbQueryToApp $ DB.insertManyRewardRests dbRewards
  where
    mkReward ::
      GovActionRefunded ->
      App DB.RewardRest
    mkReward refund = do
      network <- askNetwork
      cache <- asks envCache
      saId <- queryOrInsertStakeAddress cache UpdateCache network (raCredential $ garReturnAddr refund)
      pure $
        DB.RewardRest
          { DB.rewardRestAddrId = saId
          , DB.rewardRestType = DB.RwdProposalRefund
          , DB.rewardRestAmount = Generic.coinToDbLovelace (garDeposit refund)
          , DB.rewardRestEarnedEpoch = unEpochNo earnedEpoch
          , DB.rewardRestSpendableEpoch = unEpochNo spendableEpoch
          }

splittRecordsEvery :: Int -> [a] -> [[a]]
splittRecordsEvery val = go
  where
    go [] = []
    go ys =
      let (as, bs) = splitAt val ys
       in as : go bs

insertPoolDepositRefunds ::
  EpochNo ->
  Generic.Rewards ->
  App ()
insertPoolDepositRefunds epochNo refunds = do
  tracer <- askTrace
  insertRewards epochNo epochNo (Map.toList rwds)
  liftIO . logInfo tracer $ "Inserted " <> show (Generic.rewardsCount refunds) <> " deposit refund rewards"
  where
    rwds = Generic.unRewards refunds

sumRewardTotal :: Map StakeCred (Set Generic.Reward) -> Shelley.Coin
sumRewardTotal =
  Shelley.Coin . Map.foldl' sumCoin 0
  where
    sumCoin :: Integer -> Set Generic.Reward -> Integer
    sumCoin !acc sr =
      acc + sum (map (Shelley.unCoin . Generic.rewardAmount) $ Set.toList sr)
