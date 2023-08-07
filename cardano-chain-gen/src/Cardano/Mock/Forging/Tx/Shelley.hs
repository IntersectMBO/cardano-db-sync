{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.Tx hiding (ShelleyTx)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude
import qualified Data.Maybe.Strict as Strict
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (LedgerState, StandardShelley)
import Ouroboros.Consensus.Shelley.Eras (ShelleyEra, StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

type ShelleyUTxOIndex = UTxOIndex (ShelleyEra StandardCrypto)

type ShelleyLedgerState = LedgerState (ShelleyBlock TPraosStandard (ShelleyEra StandardCrypto))

type ShelleyTx = ShelleyTx.ShelleyTx (ShelleyEra StandardCrypto)

-- instance HasField "address" (TxOut (ShelleyEra StandardCrypto)) (Addr StandardCrypto) where
--     getField (TxOut addr _) = addr

mkPaymentTx ::
  ShelleyUTxOIndex ->
  ShelleyUTxOIndex ->
  Integer ->
  Integer ->
  ShelleyLedgerState ->
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

mkDCertTxPools :: ShelleyLedgerState -> Either ForgingError ShelleyTx
mkDCertTxPools sta = Right $ mkSimpleTx $ consCertTxBody (allPoolStakeCert sta) (Withdrawals mempty)

mkSimpleTx :: ShelleyTxBody (ShelleyEra StandardCrypto) -> ShelleyTx
mkSimpleTx txBody =
  ShelleyTx.ShelleyTx
    txBody
    mempty
    (maybeToStrictMaybe Nothing)

mkDCertTx :: [ShelleyTxCert StandardShelley] -> Withdrawals StandardCrypto -> Either ForgingError ShelleyTx
mkDCertTx certs wdrl = Right $ mkSimpleTx $ consCertTxBody certs wdrl

mkSimpleDCertTx ::
  [(StakeIndex, StakeCredential StandardCrypto -> ShelleyTxCert StandardShelley)] ->
  ShelleyLedgerState ->
  Either ForgingError ShelleyTx
mkSimpleDCertTx consDert st = do
  dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  mkDCertTx dcerts (Withdrawals mempty)

consPaymentTxBody ::
  Set (TxIn StandardCrypto) ->
  StrictSeq (ShelleyTxOut (ShelleyEra StandardCrypto)) ->
  Coin ->
  ShelleyTxBody (ShelleyEra StandardCrypto)
consPaymentTxBody ins outs fees = consTxBody ins outs fees mempty (Withdrawals mempty)

consCertTxBody :: [ShelleyTxCert StandardShelley] -> Withdrawals StandardCrypto -> ShelleyTxBody (ShelleyEra StandardCrypto)
consCertTxBody = consTxBody mempty mempty (Coin 0)

consTxBody ::
  Set (TxIn StandardCrypto) ->
  StrictSeq (ShelleyTxOut (ShelleyEra StandardCrypto)) ->
  Coin ->
  [ShelleyTxCert StandardShelley] ->
  Withdrawals StandardCrypto ->
  ShelleyTxBody (ShelleyEra StandardCrypto)
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
