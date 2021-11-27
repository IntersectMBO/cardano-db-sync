{-# LANGUAGE OverloadedStrings #-}

module Cardano.Mock.Forging.Tx where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
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
import           Cardano.Ledger.Shelley.LedgerState hiding (LedgerState)
import           Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import           Cardano.Ledger.Shelley.UTxO
import           Cardano.Ledger.ShelleyMA.Timelocks
import           Cardano.Ledger.TxIn (TxIn (..))

import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock, LedgerState)
import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

type CardanoBlock = HardForkBlock (CardanoEras StandardCrypto)

consPaymentTx :: Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
              -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
              -> Coin -> TxBody (AlonzoEra StandardCrypto)
consPaymentTx ins outs fees =
    TxBody
      ins
      mempty
      outs
      mempty
      (Wdrl mempty)
      fees
      (ValidityInterval Strict.SNothing Strict.SNothing)
      Strict.SNothing
      mempty
      mempty
      Strict.SNothing
      Strict.SNothing
      (Strict.SJust $ Testnet)

mkPaymentTx :: LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
            -> Int -> Int -> Word64 -> Word64 -> ValidatedTx (AlonzoEra StandardCrypto)
mkPaymentTx sta inputIndex outputIntex amount fee =
    mkSimpleTx $ consPaymentTx input (StrictSeq.fromList [output, change]) (Coin 0)
  where
    utxoPairs = Map.toList $ unUTxO $ _utxo $ _utxoState $ esLState $
        nesEs $ Consensus.shelleyLedgerState sta
    inputPair = utxoPairs !! inputIndex
    input = Set.singleton $ fst inputPair
    outputPair = utxoPairs !! outputIntex
    TxOut addr _ _ = snd outputPair
    output = TxOut addr (valueFromList (fromIntegral amount) []) Strict.SNothing
    TxOut addr' _ _ = snd inputPair
    change = TxOut addr' (valueFromList (fromIntegral fee) []) Strict.SNothing

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
  (Strict.SJust $ Testnet)

emptyTx :: ValidatedTx (AlonzoEra StandardCrypto)
emptyTx = ValidatedTx
    { body = emptyTxBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }


