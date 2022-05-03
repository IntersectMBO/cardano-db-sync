{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Tx.Alonzo where

import           Cardano.Prelude hiding (sum, (.))

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Maybe.Strict as Strict
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Cardano.Slotting.Slot

import           Cardano.Crypto.VRF

import           Cardano.Ledger.Address
import           Cardano.Ledger.Alonzo.Data
import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.Alonzo.Tx
import           Cardano.Ledger.Alonzo.TxBody
import           Cardano.Ledger.Alonzo.TxWitness
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Hashes
import           Cardano.Ledger.Keys
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Shelley.Metadata
import           Cardano.Ledger.Shelley.TxBody (DCert (..), PoolCert (..), PoolMetadata (..),
                   PoolParams (..), StakePoolRelay (..), Wdrl (..))
import           Cardano.Ledger.ShelleyMA.Timelocks
import           Cardano.Ledger.TxIn (TxIn (..), txid)

import           Ouroboros.Consensus.Cardano.Block (LedgerState)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Mock.Forging.Crypto
import           Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

type AlonzoUTxOIndex = UTxOIndex (AlonzoEra StandardCrypto)
type AlonzoLedgerState = LedgerState (ShelleyBlock (TPraos StandardCrypto) (AlonzoEra StandardCrypto))

instance HasField "address" (TxOut (AlonzoEra StandardCrypto)) (Addr StandardCrypto) where
    getField (TxOut addr _ _) = addr

consTxBody :: Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
           -> Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
           -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
           -> Coin
           -> Value StandardCrypto
           -> [DCert StandardCrypto]
           -> Wdrl StandardCrypto
           -> TxBody (AlonzoEra StandardCrypto)
consTxBody ins cols outs fees minted certs wdrl =
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
      minted
      Strict.SNothing
      Strict.SNothing
      (Strict.SJust Testnet)

addValidityInterval :: SlotNo
                    -> ValidatedTx (AlonzoEra StandardCrypto)
                    -> ValidatedTx (AlonzoEra StandardCrypto)
addValidityInterval slotNo tx =
    tx {body = txBody'}
  where
    interval = ValidityInterval Strict.SNothing (Strict.SJust slotNo)
    -- TxBody has a restricted export via pattern synonyms, there is no better way to do this.
    TxBody a b c d e f _ h i j k l m = body tx
    txBody' = TxBody a b c d e f interval h i j k l m

consPaymentTxBody :: Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
                  -> Set (TxIn (Crypto (AlonzoEra StandardCrypto)))
                  -> StrictSeq (TxOut (AlonzoEra StandardCrypto))
                  -> Coin -> Value StandardCrypto
                  -> TxBody (AlonzoEra StandardCrypto)
consPaymentTxBody ins cols outs fees minted = consTxBody ins cols outs fees minted mempty (Wdrl mempty)

consCertTxBody :: [DCert StandardCrypto] -> Wdrl StandardCrypto -> TxBody (AlonzoEra StandardCrypto)
consCertTxBody = consTxBody mempty mempty mempty (Coin 0) mempty

mkPaymentTx :: AlonzoUTxOIndex -> AlonzoUTxOIndex -> Integer -> Integer
            -> AlonzoLedgerState
            -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkPaymentTx inputIndex outputIndex amount fees sta = do
    (inputPair, _) <- resolveUTxOIndex inputIndex sta
    addr <- resolveAddress outputIndex sta

    let input = Set.singleton $ fst inputPair
        output = TxOut addr (valueFromList (fromIntegral amount) []) Strict.SNothing
        TxOut addr' (Value inputValue _) _ = snd inputPair
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) Strict.SNothing
    Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList [output, change]) (Coin fees) mempty

mkPaymentTx' :: AlonzoUTxOIndex
             -> [(AlonzoUTxOIndex, Value StandardCrypto)]
             -> AlonzoLedgerState
             -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkPaymentTx' inputIndex outputIndex sta = do
    inputPair <- fst <$> resolveUTxOIndex inputIndex sta
    outps <- mapM mkOuts outputIndex

    let inps = Set.singleton $ fst inputPair
        TxOut addr' (Value inputValue _) _ = snd inputPair
        outValue = sum ((\ (Value vl _) -> vl) . snd <$> outputIndex)
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - outValue) []) Strict.SNothing
    Right $ mkSimpleTx True $ consPaymentTxBody inps mempty (StrictSeq.fromList $ outps ++ [change]) (Coin 0) mempty
  where
    mkOuts (outIx, vl) = do
        addr <- resolveAddress outIx sta
        Right $ TxOut addr vl Strict.SNothing

mkLockByScriptTx :: AlonzoUTxOIndex -> [Bool] -> Integer -> Integer
                 -> AlonzoLedgerState
                 -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkLockByScriptTx inputIndex spendable amount fees sta = do
    (inputPair, _) <- resolveUTxOIndex inputIndex sta

    let input = Set.singleton $ fst inputPair
        outs = mkOut <$> spendable
        TxOut addr' (Value inputValue _) _ = snd inputPair
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) Strict.SNothing
    -- No witnesses are necessary when the outputs is a script address. Only when it's consumed.
    Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList $ outs <> [change]) (Coin fees) mempty
  where
    datahash = hashData plutusDataList
    mkOut sp =
        let outAddress = if sp then alwaysSucceedsScriptAddr else alwaysFailsScriptAddr
        in TxOut outAddress (valueFromList (fromIntegral amount) []) (Strict.SJust datahash)

mkUnlockScriptTx :: [AlonzoUTxOIndex] -> AlonzoUTxOIndex -> AlonzoUTxOIndex
                 -> Bool -> Integer -> Integer -> AlonzoLedgerState
                 -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkUnlockScriptTx inputIndex colInputIndex outputIndex succeeds amount fees sta = do
    inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
    (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
    addr <- resolveAddress outputIndex sta

    let inpts = Set.fromList $ fst <$> inputPairs
        colInput = Set.singleton $ fst colInputPair
        output = TxOut addr (valueFromList (fromIntegral amount) []) Strict.SNothing
    Right $ mkScriptTx succeeds
      (mapMaybe mkScriptInp $ zip [0..] inputPairs)
      $ consPaymentTxBody inpts colInput (StrictSeq.fromList [output]) (Coin fees) mempty

mkScriptInp :: (Word64, (TxIn (Crypto (AlonzoEra StandardCrypto)), Core.TxOut (AlonzoEra StandardCrypto)))
            -> Maybe (RdmrPtr, (ScriptHash StandardCrypto, Core.Script (AlonzoEra StandardCrypto)))
mkScriptInp (n, (_txIn, txOut))
  | addr == alwaysFailsScriptAddr
  = Just
      (RdmrPtr Spend n, (alwaysFailsScriptHash, alwaysFailsScript))
  | addr == alwaysSucceedsScriptAddr
  = Just
      (RdmrPtr Spend n, (alwaysSucceedsScriptHash, alwaysSucceedsScript))
  | addr == alwaysMintScriptAddr
  = Just (RdmrPtr Spend n, (alwaysMintScriptHash, alwaysMintScript))
  | otherwise = Nothing
  where
      addr = getField @"address" txOut

mkScriptMint :: Value StandardCrypto
             -> [(RdmrPtr, (ScriptHash StandardCrypto, Core.Script (AlonzoEra StandardCrypto)))]
mkScriptMint (Value _ mp) = mapMaybe f $ zip [0..] (Map.keys mp)
  where
    f (n, policyId)
      | policyID policyId == alwaysFailsScriptHash
      = Just (RdmrPtr Mint n, (alwaysFailsScriptHash, alwaysFailsScript))
      | policyID policyId == alwaysSucceedsScriptHash
      = Just
          (RdmrPtr Mint n, (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      | policyID policyId == alwaysMintScriptHash
      = Just (RdmrPtr Mint n, (alwaysMintScriptHash, alwaysMintScript))
      | otherwise = Nothing

mkMAssetsScriptTx :: [AlonzoUTxOIndex] -> AlonzoUTxOIndex
                  -> [(AlonzoUTxOIndex, Value StandardCrypto)]
                  -> Value StandardCrypto -> Bool -> Integer -> AlonzoLedgerState
                  -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkMAssetsScriptTx inputIndex colInputIndex outputIndex minted succeeds fees sta = do
    inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
    colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInputIndex sta
    outps <- mapM mkOuts outputIndex
    let inpts = Set.fromList $ fst <$> inputPairs

    Right $ mkScriptTx succeeds
      (mapMaybe mkScriptInp (zip [0 .. ] inputPairs)
        ++ mkScriptMint minted)
      $ consPaymentTxBody inpts colInput (StrictSeq.fromList outps) (Coin fees) minted

  where
    mkOuts (outIx, vl) = do
        addr <- resolveAddress outIx sta
        Right $ TxOut addr vl (Strict.SJust (hashData plutusDataList))

mkDCertTx :: [DCert StandardCrypto] -> Wdrl StandardCrypto
          -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertTx certs wdrl = Right $ mkSimpleTx True $ consCertTxBody certs wdrl

mkSimpleDCertTx :: [(StakeIndex, StakeCredential StandardCrypto -> DCert StandardCrypto)]
                -> AlonzoLedgerState
                -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkSimpleDCertTx consDert st = do
    dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
      cred <- resolveStakeCreds stakeIndex st
      pure $ mkDCert cred
    mkDCertTx dcerts (Wdrl mempty)

mkDCertPoolTx :: [([StakeIndex], PoolIndex,
                  [StakeCredential StandardCrypto] -> KeyHash 'StakePool StandardCrypto -> DCert StandardCrypto)]
              -> AlonzoLedgerState
              -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertPoolTx consDert st = do
    dcerts <- forM consDert $ \(stakeIxs, poolIx, mkDCert) -> do
      stakeCreds <- forM stakeIxs $ \ix -> resolveStakeCreds ix st
      let poolId = resolvePool poolIx st
      pure $ mkDCert stakeCreds poolId
    mkDCertTx dcerts (Wdrl mempty)

mkScriptDCertTx :: [(StakeIndex, Bool, StakeCredential StandardCrypto -> DCert StandardCrypto)]
                -> Bool -> AlonzoLedgerState
                -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkScriptDCertTx consDert valid st = do
    dcerts <- forM consDert $ \(stakeIndex, _, mkDCert) -> do
      cred <- resolveStakeCreds stakeIndex st
      pure $ mkDCert cred
    Right $ mkScriptTx valid (mapMaybe prepareRedeemer $ zip [0..] consDert)
              $ consCertTxBody dcerts (Wdrl mempty)
  where
    prepareRedeemer (n, (StakeIndexScript bl, addRedeemer, _)) =
        if not addRedeemer then Nothing else Just $
        if bl then (RdmrPtr Cert n, (alwaysFailsScriptHash, alwaysFailsScript))
              else (RdmrPtr Cert n, (alwaysSucceedsScriptHash, alwaysSucceedsScript))
    prepareRedeemer _ = Nothing

mkDepositTxPools :: AlonzoUTxOIndex -> Integer -> AlonzoLedgerState
                 -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDepositTxPools inputIndex deposit sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      TxOut addr' (Value inputValue _) _ = snd inputPair
      change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - deposit) []) Strict.SNothing
  Right $ mkSimpleTx True $ consTxBody input mempty (StrictSeq.fromList [change]) (Coin 0) mempty (allPoolStakeCert sta) (Wdrl mempty)

mkDCertTxPools :: AlonzoLedgerState
               -> Either ForgingError (ValidatedTx (AlonzoEra StandardCrypto))
mkDCertTxPools sta = Right $ mkSimpleTx True $ consCertTxBody (allPoolStakeCert sta) (Wdrl mempty)

mkSimpleTx :: Bool -> TxBody (AlonzoEra StandardCrypto) -> ValidatedTx (AlonzoEra StandardCrypto)
mkSimpleTx valid txBody = ValidatedTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

consPoolParams :: KeyHash 'StakePool StandardCrypto -> StakeCredential StandardCrypto
               -> [KeyHash 'Staking StandardCrypto]
               -> PoolParams StandardCrypto
consPoolParams poolId rwCred owners =
  PoolParams
    { _poolId = poolId
    , _poolVrf = hashVerKeyVRF . snd . mkVRFKeyPair $ RawSeed 0 0 0 0 0 -- undefined
    , _poolPledge = Coin 1000
    , _poolCost = Coin 10000
    , _poolMargin = minBound
    , _poolRAcnt = RewardAcnt Testnet rwCred
    , _poolOwners = Set.fromList owners
    , _poolRelays = StrictSeq.singleton $ SingleHostAddr Strict.SNothing Strict.SNothing Strict.SNothing
    , _poolMD = Strict.SJust $ PoolMetadata (fromJust $ textToUrl "best.pool") "89237365492387654983275634298756"
    }

consPoolParamsTwoOwners :: [StakeCredential StandardCrypto]
                        -> KeyHash 'StakePool StandardCrypto
                        -> DCert StandardCrypto
consPoolParamsTwoOwners [rwCred, KeyHashObj owner0, KeyHashObj owner1] poolId =
    DCertPool $ RegPool $ consPoolParams poolId rwCred [owner0, owner1]
consPoolParamsTwoOwners _ _ = panic "expected 2 pool owners"

mkScriptTx :: Bool -> [(RdmrPtr, (ScriptHash StandardCrypto, Core.Script (AlonzoEra StandardCrypto)))]
           -> TxBody (AlonzoEra StandardCrypto)
           -> ValidatedTx (AlonzoEra StandardCrypto)
mkScriptTx valid rdmrs txBody = ValidatedTx
    { body = txBody
    , wits = witnesses
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
  where
    witnesses = mkWitnesses rdmrs [(hashData @(AlonzoEra StandardCrypto) plutusDataList, plutusDataList)]

mkWitnesses :: [(RdmrPtr, (ScriptHash StandardCrypto, Core.Script (AlonzoEra StandardCrypto)))]
            -> [(DataHash StandardCrypto, Data (AlonzoEra StandardCrypto))]
            -> TxWitness (AlonzoEra StandardCrypto)
mkWitnesses rdmrs datas =
    TxWitness
      mempty
      mempty
      (Map.fromList $ snd <$> rdmrs)
      (TxDats $ Map.fromList datas)
      (Redeemers $ Map.fromList redeemers)
  where
    redeemers = fmap (, (plutusDataList, ExUnits 100 100))
                    (fst <$> rdmrs)

addMetadata :: ValidatedTx (AlonzoEra StandardCrypto) -> Word64
            -> ValidatedTx (AlonzoEra StandardCrypto)
addMetadata tx n = tx { auxiliaryData = Strict.SJust $ AuxiliaryData mp mempty}
  where
    mp = Map.singleton n $ List []

mkUTxOAlonzo :: ValidatedTx (AlonzoEra StandardCrypto) -> [(TxIn StandardCrypto, TxOut (AlonzoEra StandardCrypto))]
mkUTxOAlonzo tx =
    [ (TxIn transId idx, out)
    | (out, idx) <- zip (toList $ getField @"outputs" (getField @"body" tx)) (TxIx <$> [0 ..])
    ]
  where
    transId = txid $ getField @"body" tx

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
