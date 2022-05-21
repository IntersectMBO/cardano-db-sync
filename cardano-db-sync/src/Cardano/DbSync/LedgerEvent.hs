{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.DbSync.LedgerEvent
  ( LedgerEvent (..)
  , convertAuxLedgerEvent
  , convertPoolRewards
  ) where

import           Cardano.Db hiding (EpochNo, SyncState, epochNo)

import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..), Network)
import qualified Cardano.Ledger.Shelley.Rewards as Ledger
import           Cardano.Ledger.Shelley.Rules.Epoch (EpochEvent (PoolReapEvent))
import           Cardano.Ledger.Shelley.Rules.Mir (MirEvent (..))
import           Cardano.Ledger.Shelley.Rules.NewEpoch (NewEpochEvent (..))
import           Cardano.Ledger.Shelley.Rules.PoolReap (PoolreapEvent (..))
import           Cardano.Ledger.Shelley.Rules.Rupd (RupdEvent (RupdEvent))
import           Cardano.Ledger.Shelley.Rules.Tick (TickEvent (NewEpochEvent))
import qualified Cardano.Ledger.Shelley.Rules.Tick as Tick

import           Cardano.Prelude hiding (All)

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Types

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.State.Transition (Event)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.SOP.Strict (All, K (..), hcmap, hcollapse)
import qualified Data.Strict.Maybe as Strict

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraLedgerEvent,
                   getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent, LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers


data LedgerEvent
  = LedgerMirDist !(Map Generic.StakeCred (Set Generic.Reward))
  | LedgerPoolReap !EpochNo !(Map (Ledger.StakeCredential StandardCrypto) (Map (KeyHash 'StakePool StandardCrypto) Coin))
  | LedgerIncrementalRewards !Generic.Rewards
  | LedgerDeltaRewards !Generic.Rewards
  | LedgerRestrainedRewards !EpochNo !Generic.Rewards !(Set Generic.StakeCred)
  | LedgerTotalRewards !EpochNo !(Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto)))
  | LedgerStartAtEpoch !EpochNo
  | LedgerNewEpoch !EpochNo !SyncState
  deriving Eq

instance Ord LedgerEvent where
  a <= b = toOrdering a <= toOrdering b

toOrdering :: LedgerEvent -> Int
toOrdering ev = case ev of
  LedgerMirDist {} -> 0
  LedgerPoolReap {} -> 1
  LedgerIncrementalRewards {} -> 2
  LedgerDeltaRewards {} -> 3
  LedgerRestrainedRewards {} -> 4
  LedgerTotalRewards {} -> 5
  LedgerStartAtEpoch {} -> 6
  LedgerNewEpoch {} -> 7

convertAuxLedgerEvent :: Network -> OneEraLedgerEvent (CardanoEras StandardCrypto) -> Maybe LedgerEvent
convertAuxLedgerEvent nw = toLedgerEvent nw . wrappedAuxLedgerEvent

wrappedAuxLedgerEvent
    :: OneEraLedgerEvent (CardanoEras StandardCrypto)
    -> WrapLedgerEvent (HardForkBlock (CardanoEras StandardCrypto))
wrappedAuxLedgerEvent =
  WrapLedgerEvent @(HardForkBlock (CardanoEras StandardCrypto))

class ConvertLedgerEvent blk where
  toLedgerEvent :: Network -> WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ _ = Nothing

instance
    ( Crypto ledgerera ~ StandardCrypto
    , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
    , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
    , Event (Ledger.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
    , Event (Ledger.EraRule "EPOCH" ledgerera) ~ EpochEvent ledgerera
    , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ PoolreapEvent ledgerera
    , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (Crypto ledgerera)
    ) =>
  ConvertLedgerEvent (ShelleyBlock p ledgerera)
  where
    toLedgerEvent nw evt =
      case unwrapLedgerEvent evt of
        LETotalRewards e m -> Just $ LedgerTotalRewards e m
        LERestraintRewards e m creds ->
          Just $ LedgerRestrainedRewards e (convertPoolRewards nw e m) (Set.map (Generic.toStakeCred nw) creds)
        LEDeltaReward e m ->
          Just $ LedgerDeltaRewards (convertPoolRewards nw e m)
        LEIncrementalReward e m ->
          Just $ LedgerIncrementalRewards (convertPoolRewards nw e m)
        LEMirTransfer rp tp _rtt _ttr -> Just $ LedgerMirDist (convertMirRewards nw rp tp)
        LERetiredPools r _u en -> Just $ LedgerPoolReap en r
        ShelleyLedgerEventBBODY {} -> Nothing
        ShelleyLedgerEventTICK {} -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent nw =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent nw)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

--------------------------------------------------------------------------------

convertMirRewards
    :: Network -> Map (Ledger.StakeCredential StandardCrypto) Coin -> Map (Ledger.StakeCredential StandardCrypto) Coin
    -> Map Generic.StakeCred (Set Generic.Reward)
convertMirRewards nw resPay trePay =
    Map.unionWith Set.union (convertResPay resPay) (convertTrePay trePay)
  where
    convertResPay :: Map (Ledger.StakeCredential StandardCrypto) Coin -> Map Generic.StakeCred (Set Generic.Reward)
    convertResPay = mapBimap (Generic.toStakeCred nw) (mkPayment RwdReserves)

    convertTrePay :: Map (Ledger.StakeCredential StandardCrypto) Coin -> Map Generic.StakeCred (Set Generic.Reward)
    convertTrePay = mapBimap (Generic.toStakeCred nw) (mkPayment RwdTreasury)

    mkPayment :: RewardSource -> Coin -> Set Generic.Reward
    mkPayment src coin =
      Set.singleton $
        Generic.Reward
          { Generic.rewardSource = src
          , Generic.rewardPool = Strict.Nothing
          , Generic.rewardAmount = coin
          }

convertPoolRewards
    :: Network -> EpochNo -> Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto))
    -> Generic.Rewards
convertPoolRewards network epochNum rmap =
  Generic.Rewards
    { Generic.rwdEpoch = epochNum
    , Generic.rwdRewards = mapBimap (Generic.toStakeCred network) (Set.map convertReward) rmap
    }
  where
    convertReward :: Ledger.Reward StandardCrypto -> Generic.Reward
    convertReward sr =
      Generic.Reward
        { Generic.rewardSource = rewardTypeToSource $ Ledger.rewardType sr
        , Generic.rewardAmount = Ledger.rewardAmount sr
        , Generic.rewardPool = Strict.Just $ Generic.toStakePoolKeyHash (Ledger.rewardPool sr)
        }

mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList

--------------------------------------------------------------------------------
-- Patterns for event access. Why aren't these in ledger-specs?

pattern LERestraintRewards
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto))
    -> Set (Ledger.StakeCredential StandardCrypto)
    -> AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LERestraintRewards e m creds <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (RestrainedRewards e m creds))

pattern LETotalRewards
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto))
    -> AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LETotalRewards e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (TotalRewardEvent e m))

pattern LEDeltaReward
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (Crypto ledgerera)
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto))
    -> AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEDeltaReward e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (DeltaRewardEvent (RupdEvent e m)))

pattern LEIncrementalReward
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (Crypto ledgerera)
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto))
    -> AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEIncrementalReward e m <-
  ShelleyLedgerEventTICK
    (Tick.RupdEvent (RupdEvent e m))

pattern LEMirTransfer
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       , Event (Ledger.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
       )
    => Map (Ledger.StakeCredential StandardCrypto) Coin -> Map (Ledger.StakeCredential StandardCrypto) Coin
    -> DeltaCoin -> DeltaCoin
    -> AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LEMirTransfer rp tp rtt ttr <-
  ShelleyLedgerEventTICK
    ( NewEpochEvent
        ( MirEvent
            ( MirTransfer
                ( InstantaneousRewards rp tp rtt ttr
                  )
              )
          )
      )

pattern LERetiredPools
  :: ( Crypto ledgerera ~ StandardCrypto
     , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
     , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
     , Event (Ledger.EraRule "EPOCH" ledgerera) ~ EpochEvent ledgerera
     , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ PoolreapEvent ledgerera
     )
  => Map (Ledger.StakeCredential StandardCrypto) (Map (KeyHash 'StakePool StandardCrypto) Coin)
  -> Map (Ledger.StakeCredential StandardCrypto) (Map (KeyHash 'StakePool StandardCrypto) Coin)
  -> EpochNo
  -> AuxLedgerEvent (LedgerState (ShelleyBlock p ledgerera))
pattern LERetiredPools r u e <-
  ShelleyLedgerEventTICK
    ( NewEpochEvent
        ( EpochEvent
            ( PoolReapEvent
                ( RetiredPools r u e
                  )
              )
          )
      )
