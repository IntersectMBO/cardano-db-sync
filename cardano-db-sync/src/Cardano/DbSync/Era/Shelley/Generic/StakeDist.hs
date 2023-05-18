{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.StakeDist (
  StakeSliceRes (..),
  StakeSlice (..),
  getSecurityParameter,
  getStakeSlice,
) where

import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.EpochBoundary as Ledger
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Data.VMap (VB, VMap (..), VP)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Prelude (id)

data StakeSliceRes
  = Slice !StakeSlice !Bool -- True if this is the final slice for this epoch. Can be used for logging.
  | NoSlices

data StakeSlice = StakeSlice
  { sliceEpochNo :: !EpochNo
  , sliceDistr :: !(Map StakeCred (Coin, PoolKeyHash))
  }
  deriving (Eq)

emptySlice :: EpochNo -> StakeSlice
emptySlice epoch = StakeSlice epoch Map.empty

getSecurityParameter ::
  ConsensusProtocol (BlockProtocol blk) =>
  ProtocolInfo IO blk ->
  Word64
getSecurityParameter = maxRollbacks . configSecurityParam . pInfoConfig

-- 'sliceIndex' can match the epochBlockNo for every block.
--
-- 'minSliceSize' has to be constant or it could cause missing data.
-- If this value is too small it will be adjusted to a 'defaultEpochSliceSize'
-- which is big enough to cover all delegations.
-- On mainnet, for a value minSliceSize = 2000, it will be used as the actual size of slices
-- until the size of delegations grows up to 8.6M, in which case, the size of slices
-- will be adjusted.
getStakeSlice ::
  ConsensusProtocol (BlockProtocol blk) =>
  ProtocolInfo IO blk ->
  EpochNo ->
  Word64 ->
  Word64 ->
  ExtLedgerState CardanoBlock ->
  StakeSliceRes
getStakeSlice pInfo epoch !sliceIndex !minSliceSize els =
  case ledgerState els of
    LedgerStateByron _ -> NoSlices
    LedgerStateShelley sls -> genericStakeSlice pInfo epoch sliceIndex minSliceSize sls
    LedgerStateAllegra als -> genericStakeSlice pInfo epoch sliceIndex minSliceSize als
    LedgerStateMary mls -> genericStakeSlice pInfo epoch sliceIndex minSliceSize mls
    LedgerStateAlonzo als -> genericStakeSlice pInfo epoch sliceIndex minSliceSize als
    LedgerStateBabbage bls -> genericStakeSlice pInfo epoch sliceIndex minSliceSize bls
    LedgerStateConway _cls -> panic "TODO: Conway not supported yet"

genericStakeSlice ::
  forall era c blk p.
  (c ~ StandardCrypto, EraCrypto era ~ c, ConsensusProtocol (BlockProtocol blk)) =>
  ProtocolInfo IO blk ->
  EpochNo ->
  Word64 ->
  Word64 ->
  LedgerState (ShelleyBlock p era) ->
  StakeSliceRes
genericStakeSlice pInfo epoch sliceIndex minSliceSize lstate
  | index > delegationsLen = NoSlices
  | index == delegationsLen = Slice (emptySlice epoch) True
  | index + epochSliceSize > delegationsLen = Slice (mkSlice (delegationsLen - index)) True
  | otherwise = Slice (mkSlice epochSliceSize) False
  where
    -- We use 'ssStakeSet' here instead of 'ssStateMark' because the stake addresses for the
    -- later may not have been added to the database yet. That means that when these values
    -- are added to the database, the epoch number where they become active is the current
    -- epoch plus one.

    stakeSnapshot :: Ledger.SnapShot c
    stakeSnapshot =
      Ledger.ssStakeSet . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    delegations :: VMap.KVVector VB VB (Credential 'Staking c, KeyHash 'StakePool c)
    delegations = VMap.unVMap $ Ledger.ssDelegations stakeSnapshot

    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VG.length delegations

    stakes :: VMap VB VP (Credential 'Staking c) (Ledger.CompactForm Coin)
    stakes = Ledger.unStake $ Ledger.ssStake stakeSnapshot

    lookupStake :: Credential 'Staking c -> Maybe Coin
    lookupStake cred = Ledger.fromCompact <$> VMap.lookup cred stakes

    -- This is deterministic for the whole epoch and is the constant size of slices
    -- until the data are over. This means the last slice could be of smaller size and slices
    -- after that will be empty.
    epochSliceSize :: Word64
    epochSliceSize =
      max minSliceSize defaultEpochSliceSize
      where
        -- On mainnet this is 2160
        k :: Word64
        k = getSecurityParameter pInfo

        -- On mainnet this is 21600
        expectedBlocks :: Word64
        expectedBlocks = 10 * k

        -- This size of slices is enough to cover the whole list, even if only
        -- the 20% of the expected blocks appear in an epoch.
        defaultEpochSliceSize :: Word64
        defaultEpochSliceSize = 1 + div (delegationsLen * 5) expectedBlocks

    -- The starting index of the data in the delegation vector.
    index :: Word64
    index = sliceIndex * epochSliceSize

    mkSlice :: Word64 -> StakeSlice
    mkSlice size =
      StakeSlice
        { sliceEpochNo = epoch
        , sliceDistr = distribution
        }
      where
        delegationsSliced :: VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c)
        delegationsSliced = VMap $ VG.slice (fromIntegral index) (fromIntegral size) delegations

        distribution :: Map StakeCred (Coin, PoolKeyHash)
        distribution =
          VMap.toMap $
            VMap.mapMaybe id $
              VMap.mapWithKey (\k p -> (,p) <$> lookupStake k) delegationsSliced
