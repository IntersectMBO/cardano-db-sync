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
  ) where

import           Cardano.Db hiding (EpochNo, epochNo)

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
import           Cardano.Ledger.Shelley.Rules.Tick (TickEvent (..))

import           Cardano.Prelude hiding (All)

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Types
import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.State.Transition (Event)

import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, K (..), hcmap, hcollapse)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraLedgerEvent,
                   getOneEraLedgerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent, LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyLedgerEvent (..))
import           Ouroboros.Consensus.TypeFamilyWrappers


data LedgerEvent
  = LedgerNewEpoch !EpochNo !SyncState
  | LedgerStartAtEpoch !EpochNo
  | LedgerRewards !SlotDetails !Generic.Rewards

  | LedgerRewardDist !Generic.Rewards
  | LedgerMirDist !Generic.Rewards
  | LedgerPoolReap !EpochNo !(Map (Ledger.StakeCredential StandardCrypto) (Map (KeyHash 'StakePool StandardCrypto) Coin))
  deriving Eq

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
    ) =>
  ConvertLedgerEvent (ShelleyBlock ledgerera)
  where
    toLedgerEvent nw evt =
      case unwrapLedgerEvent evt of
        LERewards e m -> Just $ LedgerRewardDist (convertPoolRewards nw e m)
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
    -> Generic.Rewards
convertMirRewards nw resPay trePay =
  Generic.Rewards
    { Generic.rwdEpoch = EpochNo 0 -- Will be fixed later
    , Generic.rwdRewards = Map.unionWith Set.union (convertResPay resPay) (convertTrePay trePay)
    }
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
          , Generic.rewardPool = Nothing
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
        , Generic.rewardPool = Just $ Generic.toStakePoolKeyHash (Ledger.rewardPool sr)
        }

mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList

--------------------------------------------------------------------------------
-- Patterns for event access. Why aren't these in ledger-specs?

pattern LERewards
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) (Set (Ledger.Reward StandardCrypto))
    -> AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
pattern LERewards e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (RewardEvent e m))

pattern LEMirTransfer
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       , Event (Ledger.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
       )
    => Map (Ledger.StakeCredential StandardCrypto) Coin -> Map (Ledger.StakeCredential StandardCrypto) Coin
    -> DeltaCoin -> DeltaCoin
    -> AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
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
  -> AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
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
