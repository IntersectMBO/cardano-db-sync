{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Mock.Forging.Tx.Alonzo where

import           Cardano.Prelude

import qualified Data.Maybe.Strict as Strict
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Cardano.Ledger.Alonzo.Tx
import           Cardano.Ledger.Alonzo.TxBody
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Shelley.TxBody (DCert (..), Wdrl (..))
import           Cardano.Ledger.ShelleyMA.Timelocks
import           Cardano.Ledger.TxIn (TxIn (..))

import           Ouroboros.Consensus.Cardano.Block (LedgerState)
import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

consTxBody :: Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
           -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
           -> Coin
           -> [DCert StandardCrypto]
           -> Wdrl StandardCrypto
           -> TxBody (AlonzoEra StandardCrypto)
consTxBody ins outs fees certs wdrl =
    TxBody
      ins
      mempty
      outs
      (StrictSeq.fromList certs)
      wdrl
      fees
      (ValidityInterval Strict.SNothing Strict.SNothing)
      Strict.SNothing
      mempty
      mempty
      Strict.SNothing
      Strict.SNothing
      (Strict.SJust Testnet)

consPaymentTxBody :: Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
                  -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
                  -> Coin -> TxBody (AlonzoEra StandardCrypto)
consPaymentTxBody ins outs fees = consTxBody ins outs fees mempty (Wdrl mempty)

consCertTxBody :: [DCert StandardCrypto] -> Wdrl StandardCrypto -> TxBody (AlonzoEra StandardCrypto)
consCertTxBody = consTxBody mempty mempty (Coin 0)

mkPaymentTx :: UTxOIndex -> UTxOIndex -> Integer -> Integer
            -> LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
            -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkPaymentTx inputIndex outputIndex amount fees sta = do
    (inputPair, _) <- resolveUTxOIndex inputIndex sta
    (outputPair, _ ) <- resolveUTxOIndex outputIndex sta

    let input = Set.singleton $ fst inputPair
        TxOut addr _ _ = snd outputPair
        output = TxOut addr (valueFromList (fromIntegral amount) []) Strict.SNothing
        TxOut addr' (Value inputValue _) _ = snd inputPair
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) Strict.SNothing
    Right $ mkSimpleTx $ consPaymentTxBody input (StrictSeq.fromList [output, change]) (Coin fees)

mkDCertTx :: [DCert StandardCrypto] -> Wdrl StandardCrypto
          -> LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
          -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertTx certs wdrl sta = Right $ mkSimpleTx $ consCertTxBody certs wdrl

mkDCertTxPools :: LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
               -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertTxPools sta = Right $ mkSimpleTx $ consCertTxBody (allPoolStakeCert sta) (Wdrl mempty)

mkSimpleTx :: TxBody (AlonzoEra StandardCrypto) -> ValidatedTx (AlonzoEra StandardCrypto)
mkSimpleTx txBody = ValidatedTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

emptyTxBody :: TxBody (AlonzoEra StandardCrypto)
emptyTxBody = TxBody
  mempty
  mempty
  mempty
  mempty
  (Wdrl mempty)
  (Coin 0)
  (ValidityInterval Strict.SNothing Strict.SNothing)
  Strict.SNothing
  mempty
  mempty
  Strict.SNothing
  Strict.SNothing
  (Strict.SJust Testnet)

emptyTx :: ValidatedTx (AlonzoEra StandardCrypto)
emptyTx = ValidatedTx
    { body = emptyTxBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
