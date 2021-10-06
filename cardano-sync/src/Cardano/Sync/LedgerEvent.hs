{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Sync.LedgerEvent
  ( LedgerEvent (..)
  , convertAuxLedgerEvent
  ) where

import           Cardano.Db

import           Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.Shelley.API (InstantaneousRewards (..))
import           Cardano.Ledger.Shelley.Rules.Epoch (EpochEvent (PoolReapEvent))
import           Cardano.Ledger.Shelley.Rules.Mir (MirEvent (..))
import           Cardano.Ledger.Shelley.Rules.NewEpoch (NewEpochEvent (..))
import           Cardano.Ledger.Shelley.Rules.PoolReap (PoolreapEvent (..))
import           Cardano.Ledger.Shelley.Rules.Tick (TickEvent (..))

import           Cardano.Slotting.Slot (EpochNo (..))
import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Control.State.Transition (Event)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, K (..), Proxy (..), hcmap, hcollapse)

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
  | LedgerStakeDist !Generic.StakeDist

  | LedgerRewardDist !EpochNo !(Map (Ledger.StakeCredential StandardCrypto) Coin)
  | LedgerMirDist !(Map (Ledger.StakeCredential StandardCrypto) Coin)
  | LedgerPoolReap !EpochNo !(Map (Ledger.StakeCredential StandardCrypto) (Map (KeyHash 'StakePool StandardCrypto) Coin))
  deriving Eq

convertAuxLedgerEvent :: OneEraLedgerEvent (CardanoEras StandardCrypto) -> Maybe LedgerEvent
convertAuxLedgerEvent = toLedgerEvent . wrappedAuxLedgerEvent

wrappedAuxLedgerEvent
    :: OneEraLedgerEvent (CardanoEras StandardCrypto)
    -> WrapLedgerEvent (HardForkBlock (CardanoEras StandardCrypto))
wrappedAuxLedgerEvent =
  WrapLedgerEvent @(HardForkBlock (CardanoEras StandardCrypto))

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

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
    toLedgerEvent evt =
      case unwrapLedgerEvent evt of
        LESumRewards e m -> Just $ LedgerRewardDist e m
        LEMirTransfer rp tp _rtt _ttr -> Just $ LedgerMirDist (Map.unionWith plusCoin rp tp)
        LERetiredPools r _u en -> Just $ LedgerPoolReap en r
        ShelleyLedgerEventBBODY {} -> Nothing
        ShelleyLedgerEventTICK {} -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

--------------------------------------------------------------------------------
-- Patterns for event access. Why aren't these in ledger-specs?

pattern LESumRewards
    :: ( Crypto ledgerera ~ StandardCrypto
       , Event (Ledger.EraRule "TICK" ledgerera) ~ TickEvent ledgerera
       , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
       )
    => EpochNo -> Map (Ledger.StakeCredential StandardCrypto) Coin
    -> AuxLedgerEvent (LedgerState (ShelleyBlock ledgerera))
pattern LESumRewards e m <-
  ShelleyLedgerEventTICK
    (NewEpochEvent (SumRewards e m))

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
