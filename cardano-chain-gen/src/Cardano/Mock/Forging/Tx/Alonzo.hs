{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Tx.Alonzo where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as Strict
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.Alonzo.Tx
import           Cardano.Ledger.Alonzo.TxBody
import           Cardano.Ledger.Alonzo.TxWitness
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Hashes
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Shelley.TxBody (DCert (..), Wdrl (..))
import           Cardano.Ledger.ShelleyMA.Timelocks
import           Cardano.Ledger.TxIn (TxIn (..))

import           Ouroboros.Consensus.Cardano.Block (LedgerState)
import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

consTxBody :: Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
           -> Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
           -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
           -> Coin
           -> [DCert StandardCrypto]
           -> Wdrl StandardCrypto
           -> TxBody (AlonzoEra StandardCrypto)
consTxBody ins cols outs fees certs wdrl =
    TxBody
      ins
      cols
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
                  -> Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
                  -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
                  -> Coin -> TxBody (AlonzoEra StandardCrypto)
consPaymentTxBody ins cols outs fees = consTxBody ins cols outs fees mempty (Wdrl mempty)

consCertTxBody :: [DCert StandardCrypto] -> Wdrl StandardCrypto -> TxBody (AlonzoEra StandardCrypto)
consCertTxBody = consTxBody mempty mempty mempty (Coin 0)

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
    Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList [output, change]) (Coin fees)

mkLockByScriptTx :: UTxOIndex -> Bool -> Integer -> Integer
                 -> LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
                 -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkLockByScriptTx inputIndex spendable amount fees sta = do
    (inputPair, _) <- resolveUTxOIndex inputIndex sta

    let input = Set.singleton $ fst inputPair
        outAddress = if spendable then alwaysSuccedsScriptAddr else alwaysFailsScriptAddr
        output = TxOut outAddress (valueFromList (fromIntegral amount) []) (Strict.SJust datahash)
        TxOut addr' (Value inputValue _) _ = snd inputPair
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) Strict.SNothing
    -- No wittnesses are necessary when the outputs is a script address. Only when it's consumed.
    Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList [output, change]) (Coin fees)
  where
    datahash = hashData plutusDataList

mkUnlockScriptTx :: TxIn StandardCrypto -> UTxOIndex -> UTxOIndex -> Bool -> Integer -> Integer
                 -> LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
                 -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkUnlockScriptTx inp colInputIndex outputIndex succeeds amount fees sta = do
    (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
    (outputPair, _ ) <- resolveUTxOIndex outputIndex sta

    let input = Set.singleton $ inp
        colInput = Set.singleton $ fst colInputPair
        TxOut addr' _ _ = snd outputPair
        output = TxOut addr' (valueFromList (fromIntegral amount) []) Strict.SNothing
    Right $ mkScriptTx succeeds [(alwaysSucceedsScriptHash, alwaysSuccedsScript)]
      $ consPaymentTxBody input colInput (StrictSeq.fromList [output]) (Coin fees)

mkDCertTx :: [DCert StandardCrypto] -> Wdrl StandardCrypto
          -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertTx certs wdrl = Right $ mkSimpleTx True $ consCertTxBody certs wdrl

mkSimpleDCertTx :: StakeIndex -> (StakeCredential StandardCrypto -> DCert StandardCrypto)
                -> LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
                -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkSimpleDCertTx index mkCert st = do
    cred <- resolveStakeCreds index st
    mkDCertTx [mkCert cred] (Wdrl mempty)

mkDCertTxPools :: LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
               -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertTxPools sta = Right $ mkSimpleTx True $ consCertTxBody (allPoolStakeCert sta) (Wdrl mempty)

mkSimpleTx :: Bool -> TxBody (AlonzoEra StandardCrypto) -> ValidatedTx (AlonzoEra StandardCrypto)
mkSimpleTx valid txBody = ValidatedTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

mkScriptTx :: Bool -> [(ScriptHash StandardCrypto, Core.Script (AlonzoEra StandardCrypto))]
           -> TxBody (AlonzoEra StandardCrypto)
           -> ValidatedTx (AlonzoEra StandardCrypto)
mkScriptTx valid scripts txBody = ValidatedTx
    { body = txBody
    , wits = witnesses
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
  where
    witnesses = mkWitnesses scripts [(hashData @(AlonzoEra StandardCrypto) plutusDataList, plutusDataList)]

mkWitnesses :: -- (Crypto era ~ StandardCrypto)
               [(ScriptHash StandardCrypto, Core.Script (AlonzoEra StandardCrypto))]
            -> [(DataHash StandardCrypto, (Data (AlonzoEra StandardCrypto)))]
            -> TxWitness (AlonzoEra StandardCrypto)
mkWitnesses scripts datas =
    TxWitness
      mempty
      mempty
      (Map.fromList scripts)
      (TxDats $ Map.fromList datas)
      (Redeemers $ Map.singleton (RdmrPtr Spend 0) (plutusDataList, ExUnits 100 100))

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
