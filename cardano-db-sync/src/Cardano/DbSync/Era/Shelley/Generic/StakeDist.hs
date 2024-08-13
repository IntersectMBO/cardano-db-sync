{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.StakeDist (
  StakeSliceRes (..),
  StakeSlice (..),
  getSecurityParameter,
  getStakeSlice,
  getPoolDistr,
) where

import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.EpochBoundary as Ledger
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Val ((<+>))
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Data.VMap (VB, VMap (..), VP)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Lens.Micro
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
  deriving (Show)

data StakeSlice = StakeSlice
  { sliceEpochNo :: !EpochNo
  , sliceDistr :: !(Map StakeCred (Coin, PoolKeyHash))
  }
  deriving (Show, Eq)

getSecurityParameter ::
  ConsensusProtocol (BlockProtocol blk) =>
  ProtocolInfo blk ->
  Word64
getSecurityParameter = maxRollbacks . configSecurityParam . pInfoConfig

-- | Get the stake distribution for the given epoch.
getStakeSlice ::
  Word64 ->
  ExtLedgerState CardanoBlock ->
  Bool ->
  StakeSliceRes
getStakeSlice !epochBlockNo els isMigration =
  case ledgerState els of
    LedgerStateByron _ -> NoSlices
    LedgerStateShelley sls -> genericStakeSlice epochBlockNo sls isMigration
    LedgerStateAllegra als -> genericStakeSlice epochBlockNo als isMigration
    LedgerStateMary mls -> genericStakeSlice epochBlockNo mls isMigration
    LedgerStateAlonzo als -> genericStakeSlice epochBlockNo als isMigration
    LedgerStateBabbage bls -> genericStakeSlice epochBlockNo bls isMigration
    LedgerStateConway cls -> genericStakeSlice epochBlockNo cls isMigration

genericStakeSlice ::
  forall era c p.
  (c ~ StandardCrypto, EraCrypto era ~ c) =>
  Word64 -> -- epochBlockNo
  LedgerState (ShelleyBlock p era) ->
  Bool -> -- isMigration
  StakeSliceRes
genericStakeSlice epochBlockNo lstate isMigration =
  case compare index delegationsLen of
    GT -> NoSlices
    EQ -> Slice (emptySlice epoch) True
    LT -> case compare (index + sliceSize) delegationsLen of
      GT -> Slice (mkSlice (delegationsLen - index)) True
      _otherwise -> Slice (mkSlice sliceSize) False
  where
    epoch :: EpochNo
    epoch = EpochNo $ 1 + unEpochNo (Shelley.nesEL (Consensus.shelleyLedgerState lstate))

    minSliceSize :: Word64
    minSliceSize = 2000

    maxSliceSize :: Word64
    maxSliceSize = 10000

    stakeSnapshot :: Ledger.SnapShot c
    stakeSnapshot =
      Ledger.ssStakeMark . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    delegations :: VMap.KVVector VB VB (Credential 'Staking c, KeyHash 'StakePool c)
    delegations = VMap.unVMap $ Ledger.ssDelegations stakeSnapshot

    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VG.length delegations

    stakes :: VMap VB VP (Credential 'Staking c) (Ledger.CompactForm Coin)
    stakes = Ledger.unStake $ Ledger.ssStake stakeSnapshot

    lookupStake :: Credential 'Staking c -> Maybe Coin
    lookupStake cred = Ledger.fromCompact <$> VMap.lookup cred stakes

    sliceSize :: Word64
    sliceSize = max minSliceSize (min maxSliceSize (delegationsLen `div` 10))

    index :: Word64
    index = if isMigration then 0 else epochBlockNo * sliceSize

    mkSlice :: Word64 -> StakeSlice
    mkSlice actualSize =
      StakeSlice
        { sliceEpochNo = epoch
        , sliceDistr = distribution
        }
      where
        delegationsSliced = VMap $ VG.slice (fromIntegral index) (fromIntegral actualSize) delegations
        distribution =
          VMap.toMap $
            VMap.mapMaybe id $
              VMap.mapWithKey (\a p -> (,p) <$> lookupStake a) delegationsSliced

    emptySlice :: EpochNo -> StakeSlice
    emptySlice e = StakeSlice {sliceEpochNo = e, sliceDistr = Map.empty}

getPoolDistr ::
  ExtLedgerState CardanoBlock ->
  Maybe (Map PoolKeyHash (Coin, Word64), Map PoolKeyHash Natural)
getPoolDistr els =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ genericPoolDistr sls
    LedgerStateAllegra als -> Just $ genericPoolDistr als
    LedgerStateMary mls -> Just $ genericPoolDistr mls
    LedgerStateAlonzo als -> Just $ genericPoolDistr als
    LedgerStateBabbage bls -> Just $ genericPoolDistr bls
    LedgerStateConway cls -> Just $ genericPoolDistr cls

genericPoolDistr ::
  forall era p.
  (EraCrypto era ~ StandardCrypto) =>
  LedgerState (ShelleyBlock p era) ->
  (Map PoolKeyHash (Coin, Word64), Map PoolKeyHash Natural)
genericPoolDistr lstate =
  (stakePerPool, blocksPerPool)
  where
    nes :: Shelley.NewEpochState era
    nes = Consensus.shelleyLedgerState lstate

    stakeMark :: Ledger.SnapShot StandardCrypto
    stakeMark = Ledger.ssStakeMark $ Shelley.esSnapshots $ Shelley.nesEs nes

    stakePerPool = countStakePerPool (Ledger.ssDelegations stakeMark) (Ledger.ssStake stakeMark)
    blocksPerPool = nes ^. Shelley.nesBprevL

countStakePerPool ::
  VMap VB VB StakeCred PoolKeyHash ->
  Ledger.Stake StandardCrypto ->
  Map PoolKeyHash (Coin, Word64)
countStakePerPool delegs (Ledger.Stake stake) = VMap.foldlWithKey accum Map.empty stake
  where
    accum !acc cred compactCoin =
      case VMap.lookup cred delegs of
        Nothing -> acc
        Just kh -> Map.insertWith addDel kh (Ledger.fromCompact compactCoin, 1) acc

    addDel (c, n) (c', _) = (c <+> c', n + 1)
