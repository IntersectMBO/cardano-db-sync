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
  GovActionRefunded (..),
  convertAuxLedgerEvent,
  mkTreasuryReward,
  convertPoolRewards,
  ledgerEventName,
  splitDeposits,
) where

import Cardano.Db hiding (AdaPots, EpochNo, SyncState, TreasuryWithdrawals, epochNo)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Ledger.Address (RewardAccount)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyEvent (..), AlonzoUtxoEvent (..), AlonzoUtxowEvent (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules as Conway
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Hashes (SafeHash)
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
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Strict (hcmap, hcollapse)
import qualified Data.Set as Set
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
  = LedgerMirDist !(Map StakeCred (Set Generic.RewardRest))
  | LedgerPoolReap !EpochNo !Generic.Rewards
  | LedgerIncrementalRewards !EpochNo !Generic.Rewards
  | LedgerDeltaRewards !EpochNo !Generic.Rewards
  | LedgerRestrainedRewards !EpochNo !Generic.Rewards !(Set StakeCred)
  | LedgerTotalRewards !EpochNo !(Map StakeCred (Set Ledger.Reward))
  | LedgerAdaPots !AdaPots
  | LedgerGovInfo [GovActionRefunded] [GovActionRefunded] [GovActionRefunded] (Set GovActionId)
  | LedgerDeposits (SafeHash Ledger.EraIndependentTxBody) Coin
  | LedgerStartAtEpoch !EpochNo
  | LedgerNewEpoch !EpochNo !SyncState
  deriving (Eq)

data GovActionRefunded = GovActionRefunded
  { garGovActionId :: GovActionId
  , garDeposit :: Coin
  , garReturnAddr :: RewardAccount
  , garMTreasury :: Maybe (Map RewardAccount Coin)
  }
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
  LedgerGovInfo {} -> 7
  LedgerDeposits {} -> 8
  LedgerStartAtEpoch {} -> 9
  LedgerNewEpoch {} -> 10

convertAuxLedgerEvent :: Bool -> OneEraLedgerEvent (CardanoEras StandardCrypto) -> Maybe LedgerEvent
convertAuxLedgerEvent hasRewards = toLedgerEvent hasRewards . wrappedAuxLedgerEvent

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
    LedgerGovInfo {} -> "LedgerGovInfo"
    LedgerDeposits {} -> "LedgerDeposits"
    LedgerStartAtEpoch {} -> "LedgerStartAtEpoch"
    LedgerNewEpoch {} -> "LedgerNewEpoch"

wrappedAuxLedgerEvent ::
  OneEraLedgerEvent (CardanoEras StandardCrypto) ->
  WrapLedgerEvent (HardForkBlock (CardanoEras StandardCrypto))
wrappedAuxLedgerEvent =
  WrapLedgerEvent @(HardForkBlock (CardanoEras StandardCrypto))

class ConvertLedgerEvent blk where
  toLedgerEvent :: Bool -> WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ByronBlock where
  toLedgerEvent _ _ = Nothing

instance ConvertLedgerEvent (ShelleyBlock protocol ShelleyEra) where
  toLedgerEvent hasRewards evt =
    case unwrapLedgerEvent evt of
      LEDepositShelley hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt hasRewards

instance ConvertLedgerEvent (ShelleyBlock protocol AllegraEra) where
  toLedgerEvent hasRewards evt =
    case unwrapLedgerEvent evt of
      LEDepositAllegra hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt hasRewards

instance ConvertLedgerEvent (ShelleyBlock protocol MaryEra) where
  toLedgerEvent hasRewards evt =
    case unwrapLedgerEvent evt of
      LEDepositAllegra hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt hasRewards

instance ConvertLedgerEvent (ShelleyBlock protocol AlonzoEra) where
  toLedgerEvent hasRewards evt =
    case unwrapLedgerEvent evt of
      LEDepositsAlonzo hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt hasRewards

instance ConvertLedgerEvent (ShelleyBlock protocol BabbageEra) where
  toLedgerEvent hasRewards evt =
    case unwrapLedgerEvent evt of
      LEDepositsAlonzo hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventShelley evt hasRewards

instance ConvertLedgerEvent (ShelleyBlock protocol ConwayEra) where
  toLedgerEvent hasRewards evt =
    case unwrapLedgerEvent evt of
      LEDepositsConway hsh coin -> Just $ LedgerDeposits hsh coin
      _ -> toLedgerEventConway evt hasRewards

toLedgerEventShelley ::
  ( Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ShelleyNewEpochEvent ledgerera
  , Event (Ledger.EraRule "MIR" ledgerera) ~ ShelleyMirEvent ledgerera
  , Event (Ledger.EraRule "EPOCH" ledgerera) ~ ShelleyEpochEvent ledgerera
  , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent
  ) =>
  WrapLedgerEvent (ShelleyBlock protocol ledgerera) ->
  Bool ->
  Maybe LedgerEvent
toLedgerEventShelley evt hasRewards =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.TotalRewardEvent e m)) ->
      whenHasRew hasRewards $ LedgerTotalRewards e m
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.RestrainedRewards e m creds)) ->
      whenHasRew hasRewards $ LedgerRestrainedRewards e (convertPoolRewards m) creds
    ShelleyLedgerEventTICK (TickNewEpochEvent (Shelley.DeltaRewardEvent (RupdEvent e m))) ->
      whenHasRew hasRewards $ LedgerDeltaRewards e (convertPoolRewards m)
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent e m)) ->
      whenHasRew hasRewards $ LedgerIncrementalRewards e (convertPoolRewards m)
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
  ( Event (Ledger.EraRule "TICK" ledgerera) ~ ShelleyTickEvent ledgerera
  , Event (Ledger.EraRule "NEWEPOCH" ledgerera) ~ ConwayNewEpochEvent ledgerera
  , Event (Ledger.EraRule "EPOCH" ledgerera) ~ ConwayEpochEvent ledgerera
  , Event (Ledger.EraRule "POOLREAP" ledgerera) ~ ShelleyPoolreapEvent ledgerera
  , Event (Ledger.EraRule "RUPD" ledgerera) ~ RupdEvent
  ) =>
  WrapLedgerEvent (ShelleyBlock protocol ledgerera) ->
  Bool ->
  Maybe LedgerEvent
toLedgerEventConway evt hasRewards =
  case unwrapLedgerEvent evt of
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.TotalRewardEvent e m)) ->
      whenHasRew hasRewards $ LedgerTotalRewards e m
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.RestrainedRewards e m creds)) ->
      whenHasRew hasRewards $ LedgerRestrainedRewards e (convertPoolRewards m) creds
    ShelleyLedgerEventTICK (TickNewEpochEvent (Conway.DeltaRewardEvent (RupdEvent e m))) ->
      whenHasRew hasRewards $ LedgerDeltaRewards e (convertPoolRewards m)
    ShelleyLedgerEventTICK (TickRupdEvent (RupdEvent e m)) ->
      whenHasRew hasRewards $ LedgerIncrementalRewards e (convertPoolRewards m)
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
    ShelleyLedgerEventTICK
      ( TickNewEpochEvent
          ( Conway.EpochEvent
              (Conway.GovInfoEvent en droppedEnacted expired uncl)
            )
        ) ->
        Just $
          LedgerGovInfo
            (toGovActionRefunded <$> toList en)
            (toGovActionRefunded <$> toList droppedEnacted)
            (toGovActionRefunded <$> toList expired)
            (Map.keysSet uncl)
    _ -> Nothing
  where
    toGovActionRefunded :: GovActionState era -> GovActionRefunded
    toGovActionRefunded gas =
      GovActionRefunded
        { garGovActionId = gasId gas
        , garDeposit = pProcDeposit $ gasProposalProcedure gas
        , garReturnAddr = pProcReturnAddr $ gasProposalProcedure gas
        , garMTreasury = mWithrawal
        }
      where
        mWithrawal = case pProcGovAction (gasProposalProcedure gas) of
          TreasuryWithdrawals mp _ -> Just mp
          _ -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent hasRewards =
    hcollapse
      . hcmap (Proxy @ConvertLedgerEvent) (K . toLedgerEvent hasRewards)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

whenHasRew :: Bool -> a -> Maybe a
whenHasRew has a = if has then Just a else Nothing

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
        , Generic.rewardPool = kh
        , Generic.rewardAmount = coin
        }

convertMirRewards ::
  Map StakeCred Coin ->
  Map StakeCred Coin ->
  Map StakeCred (Set Generic.RewardRest)
convertMirRewards resPay trePay =
  Map.unionWith Set.union (convertResPay resPay) (convertTrePay trePay)
  where
    convertResPay :: Map StakeCred Coin -> Map StakeCred (Set Generic.RewardRest)
    convertResPay = Map.map (mkPayment RwdReserves)

    convertTrePay :: Map StakeCred Coin -> Map StakeCred (Set Generic.RewardRest)
    convertTrePay = Map.map (mkPayment RwdTreasury)

    mkPayment :: RewardSource -> Coin -> Set Generic.RewardRest
    mkPayment src coin =
      Set.singleton $
        Generic.RewardRest
          { Generic.irSource = src
          , Generic.irAmount = coin
          }

mkTreasuryReward :: Coin -> Generic.RewardRest
mkTreasuryReward c =
  Generic.RewardRest
    { Generic.irSource = RwdTreasury
    , Generic.irAmount = c
    }

convertPoolRewards ::
  Map StakeCred (Set Ledger.Reward) ->
  Generic.Rewards
convertPoolRewards rmap =
  Generic.Rewards $
    map (Set.map convertReward) rmap
  where
    convertReward :: Ledger.Reward -> Generic.Reward
    convertReward sr =
      Generic.Reward
        { Generic.rewardSource = rewardTypeToSource $ Ledger.rewardType sr
        , Generic.rewardAmount = Ledger.rewardAmount sr
        , Generic.rewardPool = Ledger.rewardPool sr
        }

--------------------------------------------------------------------------------
-- Patterns for event access.

pattern LEDepositShelley ::
  ( Event (Ledger.EraRule "BBODY" ledgerera) ~ ShelleyBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ Shelley.ShelleyUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ Shelley.UtxoEvent ledgerera
  ) =>
  SafeHash Ledger.EraIndependentTxBody ->
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
  ( Event (Ledger.EraRule "BBODY" ledgerera) ~ ShelleyBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ Shelley.ShelleyUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ Allegra.AllegraUtxoEvent ledgerera
  ) =>
  SafeHash Ledger.EraIndependentTxBody ->
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
  ( Event (Ledger.EraRule "BBODY" ledgerera) ~ Alonzo.AlonzoBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ Shelley.ShelleyLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  , Event (Ledger.EraRule "UTXOS" ledgerera) ~ Alonzo.AlonzoUtxosEvent ledgerera
  ) =>
  SafeHash Ledger.EraIndependentTxBody ->
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
  ( Event (Ledger.EraRule "BBODY" ledgerera) ~ Alonzo.AlonzoBbodyEvent ledgerera
  , Event (Ledger.EraRule "LEDGERS" ledgerera) ~ ShelleyLedgersEvent ledgerera
  , Event (Ledger.EraRule "LEDGER" ledgerera) ~ ConwayLedgerEvent ledgerera
  , Event (Ledger.EraRule "UTXOW" ledgerera) ~ AlonzoUtxowEvent ledgerera
  , Event (Ledger.EraRule "UTXO" ledgerera) ~ AlonzoUtxoEvent ledgerera
  , Event (Ledger.EraRule "UTXOS" ledgerera) ~ Conway.ConwayUtxosEvent ledgerera
  ) =>
  SafeHash Ledger.EraIndependentTxBody ->
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
                                (Conway.TotalDeposits hsh coin)
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
