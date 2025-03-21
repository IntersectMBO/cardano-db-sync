{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.StakeDist (
  StakeSliceRes (..),
  StakeSlice (..),
  getSecurityParameter,
  snapShotToList,
  getSnapShot,
  getPoolDistr,
) where

import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import qualified Cardano.Ledger.EpochBoundary as Ledger
import Cardano.Ledger.Era (EraCrypto)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Val ((<+>))
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Data.VMap (VB, VMap (..), VP)
import qualified Data.VMap as VMap
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

data StakeSlice = StakeSlice
  { sliceEpochNo :: !EpochNo
  , sliceDistr :: !(Map StakeCred (Coin, PoolKeyHash))
  }
  deriving (Eq)

getSecurityParameter ::
  ConsensusProtocol (BlockProtocol blk) =>
  ProtocolInfo blk ->
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
getSnapShot ::
  ExtLedgerState CardanoBlock ->
  Maybe (Ledger.SnapShot StandardCrypto, EpochNo)
getSnapShot els =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ genericSnapShot sls
    LedgerStateAllegra als -> Just $ genericSnapShot als
    LedgerStateMary mls -> Just $ genericSnapShot mls
    LedgerStateAlonzo als -> Just $ genericSnapShot als
    LedgerStateBabbage bls -> Just $ genericSnapShot bls
    LedgerStateConway cls -> Just $ genericSnapShot cls

genericSnapShot ::
  forall era p.
  (EraCrypto era ~ StandardCrypto) =>
  LedgerState (ShelleyBlock p era) ->
  (Ledger.SnapShot StandardCrypto, EpochNo)
genericSnapShot lstate = (stakeSnapshot, epoch)
  where
    -- We use 'ssStakeMark' here. That means that when these values
    -- are added to the database, the epoch number where they become active is the current
    -- epoch plus one.
    stakeSnapshot :: Ledger.SnapShot StandardCrypto
    stakeSnapshot =
      Ledger.ssStakeMark . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    epoch = EpochNo $ 1 + unEpochNo (Shelley.nesEL (Consensus.shelleyLedgerState lstate))

snapShotToList ::
  Ledger.SnapShot StandardCrypto ->
  [(StakeCred, (Coin, PoolKeyHash))]
snapShotToList snapShot =
  VMap.toList $
    VMap.mapMaybe id $ -- This line removes entries without stake. Should we assume 0 and insert it?
      VMap.mapWithKey (\a p -> (,p) <$> lookupStake a) (Ledger.ssDelegations snapShot)
  where
    stakes :: VMap VB VP StakeCred (Ledger.CompactForm Coin)
    stakes = Ledger.unStake $ Ledger.ssStake snapShot

    lookupStake :: StakeCred -> Maybe Coin
    lookupStake cred = Ledger.fromCompact <$> VMap.lookup cred stakes

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

    addDel (c, n) (c', n') = (c <+> c', n + n')
