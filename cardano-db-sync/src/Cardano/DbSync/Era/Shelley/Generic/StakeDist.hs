{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.DbSync.Era.Shelley.Generic.StakeDist
  ( StakeSliceRes (..)
  , StakeSlice (..)
  , stakeDistPoolHashKeys
  , stakeDistStakeCreds
  , getSecurityParameter
  , getStakeSlice
  ) where

import           Cardano.Prelude
import           Prelude (id)

import           Cardano.Crypto.Hash (hashToBytes)

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import           Cardano.Ledger.Credential (Credential)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import qualified Cardano.Ledger.Shelley.EpochBoundary as Shelley
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley hiding (_delegations)

import           Cardano.DbSync.Era.Shelley.Generic.StakeCred
import           Cardano.DbSync.Era.Shelley.Generic.StakePoolKeyHash
import           Cardano.DbSync.Types

import           Data.Compact.VMap (VB, VMap (..), VP)
import qualified Data.Compact.VMap as VMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Generic as VG

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

data StakeSliceRes =
    Slice StakeSlice Bool -- True if this is the final slice for this epoch. Can be used for logging.
  | NoSlices

data StakeSlice = StakeSlice
  { sliceEpochNo :: !EpochNo
  , sliceDistr :: !(Map StakeCred (Coin, StakePoolKeyHash))
  } deriving Eq

emptySlice :: EpochNo -> StakeSlice
emptySlice epoch = StakeSlice epoch Map.empty

getSecurityParameter :: ConsensusProtocol (BlockProtocol blk)
                     => ProtocolInfo IO blk -> Word64
getSecurityParameter =  maxRollbacks . configSecurityParam . pInfoConfig

-- 'sliceIndex' can match the epochBlockNo for every block.
--
-- 'minSliceSize' has to be constant or it could cause missing data.
-- If this value is too small it will be adjusted to a 'defaultEpochSliceSize'
-- which is big enough to cover all delegations.
-- On mainnet, for a value minSliceSize = 2000, it will be used as the actual size of slices
-- until the size of delegations grows up to 8.6M, in which case, the size of slices
-- will be adjusted.
getStakeSlice :: ConsensusProtocol (BlockProtocol blk)
              => ProtocolInfo IO blk -> Ledger.Network
              -> EpochNo -> Word64 -> Word64 -> ExtLedgerState CardanoBlock -> StakeSliceRes
getStakeSlice pInfo network epoch sliceIndex minSliceSize els =
  case ledgerState els of
    LedgerStateByron _ -> NoSlices
    LedgerStateShelley sls -> genericStakeSlice pInfo network epoch sliceIndex minSliceSize sls
    LedgerStateAllegra als -> genericStakeSlice pInfo network epoch sliceIndex minSliceSize als
    LedgerStateMary mls -> genericStakeSlice pInfo network epoch sliceIndex minSliceSize mls
    LedgerStateAlonzo als -> genericStakeSlice pInfo network epoch sliceIndex minSliceSize als

genericStakeSlice :: forall era c blk. (c ~ Crypto era, ConsensusProtocol (BlockProtocol blk))
                  => ProtocolInfo IO blk -> Ledger.Network -> EpochNo -> Word64 -> Word64
                  -> LedgerState (ShelleyBlock era) -> StakeSliceRes
genericStakeSlice pInfo network epoch sliceIndex minSliceSize lstate
    | index > delegationsLen = NoSlices
    | index == delegationsLen = Slice (emptySlice epoch) True
    | index + epochSliceSize > delegationsLen = Slice (mkSlice (delegationsLen - index)) True
    | otherwise = Slice (mkSlice epochSliceSize) False
  where
    -- We use '_pstakeSet' here instead of '_pstateMark' because the stake addresses for the
    -- later may not have been added to the database yet. That means that when these values
    -- are added to the database, the epoch number where they become active is the current
    -- epoch plus one.

    stakeSnapshot :: Shelley.SnapShot c
    stakeSnapshot = Shelley._pstakeSet . Shelley.esSnapshots . Shelley.nesEs
                $ Consensus.shelleyLedgerState lstate

    delegations :: VMap.KVVector VB VB (Credential 'Staking c, KeyHash 'StakePool c)
    delegations = VMap.unVMap $ Shelley._delegations stakeSnapshot

    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VG.length delegations

    stakes :: VMap VB VP (Credential 'Staking c) (Ledger.CompactForm Coin)
    stakes = Shelley.unStake $ Shelley._stake stakeSnapshot

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

        distribution :: Map StakeCred (Coin, StakePoolKeyHash)
        distribution = Map.mapKeys (toStakeCred network) $ VMap.toMap $
          VMap.mapMaybe id $ VMap.mapWithKey (\k p -> (, convertStakePoolkeyHash p) <$> lookupStake k) delegationsSliced

    convertStakePoolkeyHash :: KeyHash 'StakePool c -> StakePoolKeyHash
    convertStakePoolkeyHash (KeyHash h) = StakePoolKeyHash $ hashToBytes h

-- Use Set because they guarantee unique elements.
stakeDistPoolHashKeys :: StakeSlice -> Set StakePoolKeyHash
stakeDistPoolHashKeys = Set.fromList . map snd . Map.elems . sliceDistr

stakeDistStakeCreds :: StakeSlice -> Set StakeCred
stakeDistStakeCreds = Map.keysSet . sliceDistr
