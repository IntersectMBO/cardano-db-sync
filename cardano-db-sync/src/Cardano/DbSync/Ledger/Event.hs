{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Ledger.Event (
  LedgerEvent (..),
  convertAuxLedgerEvent,
  convertPoolRewards,
  ledgerEventName,
  splitDeposits,
) where

import Cardano.Db hiding (AdaPots, EpochNo, SyncState, epochNo)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyEvent (..), AlonzoUtxoEvent (..), AlonzoUtxowEvent (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Shelley.API (AdaPots, InstantaneousRewards (..))
import Cardano.Ledger.Shelley.Rules (
  RupdEvent (RupdEvent),
  ShelleyBbodyEvent (..),
  ShelleyEpochEvent,
  ShelleyLedgersEvent (..),
  ShelleyMirEvent (..),
  ShelleyNewEpochEvent (..),
  ShelleyPoolreapEvent (..),
  ShelleyTickEvent (..),
  ShelleyUtxowEvent (UtxoEvent),
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Prelude hiding (All)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.State.Transition (Event)
import qualified Data.Map.Strict as Map
import Data.SOP.Strict (All, K (..), hcmap, hcollapse)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.SafeHash (SafeHash)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (
  OneEraLedgerEvent,
  getOneEraLedgerEvent,
 )
import Ouroboros.Consensus.Ledger.Abstract (AuxLedgerEvent)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyLedgerEvent (..))
import Ouroboros.Consensus.TypeFamilyWrappers

data LedgerEvent
  = LedgerMirDist !(Map StakeCred (Set Generic.Reward))
  | LedgerPoolReap !EpochNo !Generic.Rewards
  | LedgerIncrementalRewards !EpochNo !Generic.Rewards
  | LedgerDeltaRewards !EpochNo !Generic.Rewards
  | LedgerRestrainedRewards !EpochNo !Generic.Rewards !(Set StakeCred)
  | LedgerTotalRewards !EpochNo !(Map StakeCred (Set (Ledger.Reward StandardCrypto)))
  | LedgerAdaPots !AdaPots
  | LedgerDeposits (SafeHash StandardCrypto Ledger.EraIndependentTxBody) Coin
  | LedgerStartAtEpoch !EpochNo
  | LedgerNewEpoch !EpochNo !SyncState
  deriving (Eq)

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
  LedgerAdaPots {} -> 6
  LedgerDeposits {} -> 7
  LedgerStartAtEpoch {} -> 8
  LedgerNewEpoch {} -> 9

convertAuxLedgerEvent :: OneEraLedgerEvent (CardanoEras StandardCrypto) -> Maybe LedgerEvent
convertAuxLedgerEvent = toLedgerEvent . wrappedAuxLedgerEvent

ledgerEventName :: LedgerEvent -> Text
ledgerEventName le =
  case le of
    LedgerMirDist {} -> "LedgerMirDist"
    LedgerPoolReap {} -> "LedgerPoolReap"
    LedgerIncrementalRewards {} -> "LedgerIncrementalRewards"
    LedgerDeltaRewards {} -> "LedgerDeltaRewards"
    LedgerRestrainedRewards {} -> "LedgerRestrainedRewards"
    LedgerTotalRewards {} -> "LedgerTotalRewards"
    LedgerAdaPots {} -> "LedgerAdaPots"
    LedgerDeposits {} -> "LedgerDeposits"
    LedgerStartAtEpoch {} -> "LedgerStartAtEpoch"
    LedgerNewEpoch {} -> "LedgerNewEpoch"

wrappedAuxLedgerEvent ::
  OneEraLedgerEvent (CardanoEras StandardCrypto) ->
  WrapLedgerEvent (HardForkBlock (CardanoEras StandardCrypto))
wrappedAuxLedgerEvent =
  WrapLedgerEvent @(HardForkBlock (CardanoEras StandardCrypto))

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ = Nothing

instance ConvertLedgerEvent (ShelleyBlock protocol (ShelleyEra StandardCrypto)) where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LEDepositShelley hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt

instance ConvertLedgerEvent (ShelleyBlock protocol (AllegraEra StandardCrypto)) where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LEDepositAllegra hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt

instance ConvertLedgerEvent (ShelleyBlock protocol (MaryEra StandardCrypto)) where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LEDepositAllegra hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt

instance ConvertLedgerEvent (ShelleyBlock protocol (AlonzoEra StandardCrypto)) where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LEDepositsAlonzo hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt

instance ConvertLedgerEvent (ShelleyBlock protocol (BabbageEra StandardCrypto)) where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LEDepositsAlonzo hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt

instance ConvertLedgerEvent (ShelleyBlock protocol (ConwayEra StandardCrypto)) where
  toLedgerEvent evt =
    case unwrapLedgerEvent evt of
      LEDepositsConway hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventConway evt

toLedgerEventShelley ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "MIR" ledgerera) ~ ShelleyMirEvent ledgerera
  , Event (Ledger.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera
  , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (EraCrypto ledgerera)
  ) =>
  WrapLedgerEvent (ShelleyBlock protocol ledgerera) ->
  Maybe LedgerEvent
toLedgerEventShelley evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.TotalRewardEvent e m)) ->
      Just $ LedgerTotalRewards e m
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.RestrainedRewards e m creds)) ->
      Just $ LedgerRestrainedRewards e (convertPoolRewards m) creds
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.DeltaRewardEvent (RupdEvent e m))) ->
      Just $ LedgerDeltaRewards e (convertPoolRewards m)
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent e m)) ->
      Just $ LedgerIncrementalRewards e (convertPoolRewards m)
    ShelleyLedgerEventTICK
      ( TickNewEpochEvent
          ( MirEvent
              ( MirTransfer
                  (InstantaneousRewards rp tp _ _)
                )
            )
        ) ->
        Just $ LedgerMirDist (convertMirRewards rp tp)
    ShelleyLedgerEventTICK
      ( TickNewEpochEvent
          ( Shelley.EpochEvent
              ( Shelley.PoolReapEvent
                  (RetiredPools r _u en)
                )
            )
        ) -> Just $ LedgerPoolReap en (convertPoolDepositRefunds r)
    ShelleyLedgerEventTICK
      ( TickNewEpochEvent
          (Shelley.TotalAdaPotsEvent p)
        ) -> Just $ LedgerAdaPots p
    _ -> Nothing

toLedgerEventConway ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ConwayNewEpochEvent ledgerera
  , Event (Ledger.EraRule "EPOCH" ledgerera) ~ ConwayEpochEvent ledgerera
  , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent (EraCrypto ledgerera)
  ) =>
  WrapLedgerEvent (ShelleyBlock protocol ledgerera) ->
  Maybe LedgerEvent
toLedgerEventConway evt =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.TotalRewardEvent e m)) ->
      Just $ LedgerTotalRewards e m
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.RestrainedRewards e m creds)) ->
      Just $ LedgerRestrainedRewards e (convertPoolRewards m) creds
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.DeltaRewardEvent (RupdEvent e m))) ->
      Just $ LedgerDeltaRewards e (convertPoolRewards m)
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent e m)) ->
      Just $ LedgerIncrementalRewards e (convertPoolRewards m)
    ShelleyLedgerEventTICK
      ( TickNewEpochEvent
          ( Conway.EpochEvent
              ( Conway.PoolReapEvent
                  (RetiredPools r _u en)
                )
            )
        ) -> Just $ LedgerPoolReap en (convertPoolDepositRefunds r)
    ShelleyLedgerEventTICK
      ( TickNewEpochEvent
          (Conway.TotalAdaPotsEvent p)
        ) -> Just $ LedgerAdaPots p
    _ -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

--------------------------------------------------------------------------------

convertPoolDepositRefunds ::
  Map StakeCred (Map PoolKeyHash Coin) ->
  Generic.Rewards
convertPoolDepositRefunds rwds =
  Generic.Rewards $
    Map.map (Set.fromList . map convert . Map.toList) rwds
  where
    convert :: (PoolKeyHash, Coin) -> Generic.Reward
    convert (kh, coin) =
      Generic.Reward
        { Generic.rewardSource = RwdDepositRefund
        , Generic.rewardPool = Strict.Just kh
        , Generic.rewardAmount = coin
        }

convertMirRewards ::
  Map StakeCred Coin ->
  Map StakeCred Coin ->
  Map StakeCred (Set Generic.Reward)
convertMirRewards resPay trePay =
  Map.unionWith Set.union (convertResPay resPay) (convertTrePay trePay)
  where
    convertResPay :: Map StakeCred Coin -> Map StakeCred (Set Generic.Reward)
    convertResPay = Map.map (mkPayment RwdReserves)

    convertTrePay :: Map StakeCred Coin -> Map StakeCred (Set Generic.Reward)
    convertTrePay = Map.map (mkPayment RwdTreasury)

    mkPayment :: RewardSource -> Coin -> Set Generic.Reward
    mkPayment src coin =
      Set.singleton $
        Generic.Reward
          { Generic.rewardSource = src
          , Generic.rewardPool = Strict.Nothing
          , Generic.rewardAmount = coin
          }

convertPoolRewards ::
  Map StakeCred (Set (Ledger.Reward StandardCrypto)) ->
  Generic.Rewards
convertPoolRewards rmap =
  Generic.Rewards $
    map (Set.map convertReward) rmap
  where
    convertReward :: Ledger.Reward StandardCrypto -> Generic.Reward
    convertReward sr =
      Generic.Reward
        { Generic.rewardSource = rewardTypeToSource $ Ledger.rewardType sr
        , Generic.rewardAmount = Ledger.rewardAmount sr
        , Generic.rewardPool = Strict.Just $ Ledger.rewardPool sr
        }

--------------------------------------------------------------------------------
-- Patterns for event access.

pattern LEDepositShelley ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "BBODY" ledgerera) ~ ShelleyBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ Shelley.ShelleyUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ Shelley.UtxoEvent ledgerera
  ) =>
  SafeHash StandardCrypto Ledger.EraIndependentTxBody ->
  Coin ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LEDepositShelley hsh coin <-
  ShelleyLedgerEventBBODY
    ( LedgersEvent
        ( Shelley.LedgerEvent
            ( Shelley.UtxowEvent
                ( UtxoEvent
                    (Shelley.TotalDeposits hsh coin)
                  )
              )
          )
      )

pattern LEDepositAllegra ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "BBODY" ledgerera) ~ ShelleyBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ Shelley.ShelleyUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ Allegra.AllegraUtxoEvent ledgerera
  ) =>
  SafeHash StandardCrypto Ledger.EraIndependentTxBody ->
  Coin ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LEDepositAllegra hsh coin <-
  ShelleyLedgerEventBBODY
    ( LedgersEvent
        ( Shelley.LedgerEvent
            ( Shelley.UtxowEvent
                ( UtxoEvent
                    (Allegra.TotalDeposits hsh coin)
                  )
              )
          )
      )

pattern LEDepositsAlonzo ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "BBODY" ledgerera) ~ Alonzo.AlonzoBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  , Event (Ledger.EraRule "UTXOS" ledgerera) ~ Alonzo.AlonzoUtxosEvent ledgerera
  ) =>
  SafeHash StandardCrypto Ledger.EraIndependentTxBody ->
  Coin ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LEDepositsAlonzo hsh coin <-
  ShelleyLedgerEventBBODY
    ( ShelleyInAlonzoEvent
        ( LedgersEvent
            ( Shelley.LedgerEvent
                ( Shelley.UtxowEvent
                    ( WrappedShelleyEraEvent
                        ( UtxoEvent
                            ( UtxosEvent
                                (Alonzo.TotalDeposits hsh coin)
                              )
                          )
                      )
                  )
              )
          )
      )

pattern LEDepositsConway ::
  ( EraCrypto ledgerera ~ StandardCrypto
  , Event (Ledger.EraRule "BBODY" ledgerera) ~ Alonzo.AlonzoBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ ConwayLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  , Event (Ledger.EraRule "UTXOS" ledgerera) ~ Alonzo.AlonzoUtxosEvent ledgerera
  ) =>
  SafeHash StandardCrypto Ledger.EraIndependentTxBody ->
  Coin ->
  AuxLedgerEvent (LedgerState (ShelleyBlock protocol ledgerera))
pattern LEDepositsConway hsh coin <-
  ShelleyLedgerEventBBODY
    ( ShelleyInAlonzoEvent
        ( LedgersEvent
            ( Shelley.LedgerEvent
                ( Conway.UtxowEvent
                    ( WrappedShelleyEraEvent
                        ( UtxoEvent
                            ( UtxosEvent
                                (Alonzo.TotalDeposits hsh coin)
                              )
                          )
                      )
                  )
              )
          )
      )

splitDeposits :: [LedgerEvent] -> ([LedgerEvent], Map ByteString Coin)
splitDeposits les =
  (les', Map.fromList deposits)
  where
    (deposits, les') = partitionEithers $ eitherDeposit <$> les

    eitherDeposit :: LedgerEvent -> Either (ByteString, Coin) LedgerEvent
    eitherDeposit le =
      case le of
        LedgerDeposits hsh coin -> Left (txHashFromSafe hsh, coin)
        _ -> Right le
