{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Tx.Shelley (
  mkPaymentTx,
  mkDCertTxPools,
  mkSimpleTx,
  mkDCertTx,
  mkSimpleDCertTx,
  consPaymentTxBody,
  consCertTxBody,
  consTxBody,
  mkDummyTxWithSlot,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Shelley.TxOut
import Cardano.Ledger.TxIn
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude
import qualified Data.Maybe.Strict as Strict
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (LedgerState, ShelleyEra)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

type ShelleyUTxOIndex = UTxOIndex ShelleyEra

type ShelleyLedgerState = LedgerState (ShelleyBlock TPraosStandard ShelleyEra)

type ShelleyTx = Core.Tx Core.TopTx ShelleyEra

mkPaymentTx ::
  ShelleyUTxOIndex ->
  ShelleyUTxOIndex ->
  Integer ->
  Integer ->
  ShelleyLedgerState mk ->
  Either ForgingError ShelleyTx
mkPaymentTx inputIndex outputIndex amount fees st = do
  (inputPair, _) <- resolveUTxOIndex inputIndex st
  (outputPair, _) <- resolveUTxOIndex outputIndex st
  let input = Set.singleton $ fst inputPair
      addr = snd outputPair ^. Core.addrTxOutL
      output = ShelleyTxOut addr (Coin amount)
      addr' = snd inputPair ^. Core.addrTxOutL
      Coin inputValue = snd inputPair ^. Core.valueTxOutL
      change = ShelleyTxOut addr' $ Coin (inputValue - amount - fees)

  Right $ mkSimpleTx $ consPaymentTxBody input (StrictSeq.fromList [output, change]) (Coin fees)

mkDCertTxPools :: ShelleyLedgerState mk -> Either ForgingError ShelleyTx
mkDCertTxPools sta = Right $ mkSimpleTx $ consCertTxBody (allPoolStakeCert sta) (Core.Withdrawals mempty)

mkSimpleTx :: TxBody Core.TopTx ShelleyEra -> ShelleyTx
mkSimpleTx txBody =
  ShelleyTx.MkShelleyTx $
    ShelleyTx.ShelleyTx
      txBody
      mempty
      (maybeToStrictMaybe Nothing)

mkDCertTx :: [ShelleyTxCert ShelleyEra] -> Core.Withdrawals -> Either ForgingError ShelleyTx
mkDCertTx certs wdrl = Right $ mkSimpleTx $ consCertTxBody certs wdrl

mkSimpleDCertTx ::
  [(StakeIndex, Credential Core.Staking -> ShelleyTxCert ShelleyEra)] ->
  ShelleyLedgerState mk ->
  Either ForgingError ShelleyTx
mkSimpleDCertTx consDert st = do
  dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  mkDCertTx dcerts (Core.Withdrawals mempty)

consPaymentTxBody ::
  Set TxIn ->
  StrictSeq (ShelleyTxOut ShelleyEra) ->
  Coin ->
  TxBody Core.TopTx ShelleyEra
consPaymentTxBody ins outs fees = consTxBody ins outs fees mempty (Core.Withdrawals mempty)

consCertTxBody :: [ShelleyTxCert ShelleyEra] -> Core.Withdrawals -> TxBody Core.TopTx ShelleyEra
consCertTxBody = consTxBody mempty mempty (Coin 0)

consTxBody ::
  Set TxIn ->
  StrictSeq (ShelleyTxOut ShelleyEra) ->
  Coin ->
  [ShelleyTxCert ShelleyEra] ->
  Core.Withdrawals ->
  TxBody Core.TopTx ShelleyEra
consTxBody ins outs fees certs wdrl =
  ShelleyTxBody
    ins
    outs
    (StrictSeq.fromList certs)
    wdrl
    fees
    (SlotNo 1000000000) -- TODO ttl
    Strict.SNothing
    Strict.SNothing

-- | Create a dummy tx with a unique TTL based on the slot.
-- This ensures a unique tx body hash for each slot.
mkDummyTxWithSlot :: SlotNo -> Core.Tx ShelleyEra
mkDummyTxWithSlot slot =
  ShelleyTx.MkShelleyTx $
    ShelleyTx.ShelleyTx
      ( ShelleyTxBody
          mempty
          mempty
          mempty
          (Withdrawals mempty)
          (Coin 0)
          slot
          Strict.SNothing
          Strict.SNothing
      )
      mempty
      Strict.SNothing
