{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Tx.Alonzo (
  consTxBody,
  addValidityInterval,
  consPaymentTxBody,
  consCertTxBody,
  mkPaymentTx,
  mkPaymentTx',
  mkLockByScriptTx,
  mkUnlockScriptTx,
  mkScriptInp,
  mkScriptMint,
  mkMAssetsScriptTx,
  mkDCertTx,
  mkSimpleDCertTx,
  mkDCertPoolTx,
  mkScriptDCertTx,
  mkDepositTxPools,
  mkDCertTxPools,
  mkSimpleTx,
  consPoolParams,
  consPoolParamsTwoOwners,
  mkScriptTx,
  mkWitnesses,
  mkUTxOAlonzo,
  emptyTxBody,
  emptyTx,
) where

import Cardano.Crypto.VRF
import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.TxBody (
  PoolMetadata (..),
  PoolParams (..),
  StakePoolRelay (..),
 )
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Mock.Forging.Crypto
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (sum, (.))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Maybe.Strict as Strict
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (LedgerState, StandardAlonzo)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

type AlonzoUTxOIndex = UTxOIndex StandardAlonzo

type AlonzoLedgerState = LedgerState (ShelleyBlock TPraosStandard StandardAlonzo)

consTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (AlonzoTxOut StandardAlonzo) ->
  Coin ->
  MultiAsset StandardCrypto ->
  [ShelleyTxCert StandardAlonzo] ->
  Withdrawals StandardCrypto ->
  AlonzoTxBody StandardAlonzo
consTxBody ins cols outs fees minted certs wdrl =
  AlonzoTxBody
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

addValidityInterval ::
  SlotNo ->
  AlonzoTx StandardAlonzo ->
  AlonzoTx StandardAlonzo
addValidityInterval slotNo tx =
  tx {body = txBody'}
  where
    interval = ValidityInterval Strict.SNothing (Strict.SJust slotNo)
    -- TxBody has a restricted export via pattern synonyms, there is no better way to do this.
    AlonzoTxBody a b c d e f _ h i j k l m = body tx
    txBody' = AlonzoTxBody a b c d e f interval h i j k l m

consPaymentTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (AlonzoTxOut StandardAlonzo) ->
  Coin ->
  MultiAsset StandardCrypto ->
  AlonzoTxBody StandardAlonzo
consPaymentTxBody ins cols outs fees minted = consTxBody ins cols outs fees minted mempty (Withdrawals mempty)

consCertTxBody :: [ShelleyTxCert StandardAlonzo] -> Withdrawals StandardCrypto -> AlonzoTxBody StandardAlonzo
consCertTxBody = consTxBody mempty mempty mempty (Coin 0) mempty

mkPaymentTx ::
  AlonzoUTxOIndex ->
  AlonzoUTxOIndex ->
  Integer ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkPaymentTx inputIndex outputIndex amount fees sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta
  addr <- resolveAddress outputIndex sta

  let input = Set.singleton $ fst inputPair
      output = AlonzoTxOut addr (valueFromList (fromIntegral amount) []) Strict.SNothing
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      change = AlonzoTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) Strict.SNothing
  Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList [output, change]) (Coin fees) mempty

mkPaymentTx' ::
  AlonzoUTxOIndex ->
  [(AlonzoUTxOIndex, MaryValue StandardCrypto)] ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkPaymentTx' inputIndex outputIndex sta = do
  inputPair <- fst <$> resolveUTxOIndex inputIndex sta
  outps <- mapM mkOuts outputIndex

  let inps = Set.singleton $ fst inputPair
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      outValue = sum ((\(MaryValue vl _) -> vl) . snd <$> outputIndex)
      change = AlonzoTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - outValue) []) Strict.SNothing
  Right $ mkSimpleTx True $ consPaymentTxBody inps mempty (StrictSeq.fromList $ outps ++ [change]) (Coin 0) mempty
  where
    mkOuts (outIx, vl) = do
      addr <- resolveAddress outIx sta
      Right $ AlonzoTxOut addr vl Strict.SNothing

mkLockByScriptTx ::
  AlonzoUTxOIndex ->
  [Bool] ->
  Integer ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkLockByScriptTx inputIndex spendable amount fees sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      outs = mkOut <$> spendable
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      change = AlonzoTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) Strict.SNothing
  -- No witnesses are necessary when the outputs is a script address. Only when it's consumed.
  Right $ mkSimpleTx True $ consPaymentTxBody input mempty (StrictSeq.fromList $ outs <> [change]) (Coin fees) mempty
  where
    datahash = hashData @StandardAlonzo plutusDataList
    mkOut sp =
      let outAddress = if sp then alwaysSucceedsScriptAddr else alwaysFailsScriptAddr
       in AlonzoTxOut outAddress (valueFromList (fromIntegral amount) []) (Strict.SJust datahash)

mkUnlockScriptTx ::
  [AlonzoUTxOIndex] ->
  AlonzoUTxOIndex ->
  AlonzoUTxOIndex ->
  Bool ->
  Integer ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkUnlockScriptTx inputIndex colInputIndex outputIndex succeeds amount fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
  addr <- resolveAddress outputIndex sta

  let inpts = Set.fromList $ fst <$> inputPairs
      colInput = Set.singleton $ fst colInputPair
      output = AlonzoTxOut addr (valueFromList (fromIntegral amount) []) Strict.SNothing
  Right
    $ mkScriptTx
      succeeds
      (mapMaybe mkScriptInp $ zip [0 ..] inputPairs)
    $ consPaymentTxBody inpts colInput (StrictSeq.fromList [output]) (Coin fees) mempty

mkScriptInp ::
  (Word64, (TxIn StandardCrypto, Core.TxOut StandardAlonzo)) ->
  Maybe (RdmrPtr, (ScriptHash StandardCrypto, Core.Script StandardAlonzo))
mkScriptInp (n, (_txIn, txOut))
  | addr == alwaysFailsScriptAddr =
      Just
        (RdmrPtr Spend n, (alwaysFailsScriptHash, alwaysFailsScript))
  | addr == alwaysSucceedsScriptAddr =
      Just
        (RdmrPtr Spend n, (alwaysSucceedsScriptHash, alwaysSucceedsScript))
  | addr == alwaysMintScriptAddr =
      Just (RdmrPtr Spend n, (alwaysMintScriptHash, alwaysMintScript))
  | otherwise = Nothing
  where
    addr = txOut ^. Core.addrTxOutL

mkScriptMint ::
  MultiAsset StandardCrypto ->
  [(RdmrPtr, (ScriptHash StandardCrypto, Core.Script StandardAlonzo))]
mkScriptMint (MultiAsset mp) = mapMaybe f $ zip [0 ..] (Map.keys mp)
  where
    f (n, policyId)
      | policyID policyId == alwaysFailsScriptHash =
          Just (RdmrPtr Mint n, (alwaysFailsScriptHash, alwaysFailsScript))
      | policyID policyId == alwaysSucceedsScriptHash =
          Just
            (RdmrPtr Mint n, (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      | policyID policyId == alwaysMintScriptHash =
          Just (RdmrPtr Mint n, (alwaysMintScriptHash, alwaysMintScript))
      | otherwise = Nothing

mkMAssetsScriptTx ::
  [AlonzoUTxOIndex] ->
  AlonzoUTxOIndex ->
  [(AlonzoUTxOIndex, MaryValue StandardCrypto)] ->
  MultiAsset StandardCrypto ->
  Bool ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkMAssetsScriptTx inputIndex colInputIndex outputIndex minted succeeds fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInputIndex sta
  outps <- mapM mkOuts outputIndex
  let inpts = Set.fromList $ fst <$> inputPairs

  Right
    $ mkScriptTx
      succeeds
      ( mapMaybe mkScriptInp (zip [0 ..] inputPairs)
          ++ mkScriptMint minted
      )
    $ consPaymentTxBody inpts colInput (StrictSeq.fromList outps) (Coin fees) minted
  where
    mkOuts (outIx, vl) = do
      addr <- resolveAddress outIx sta
      Right $ AlonzoTxOut addr vl (Strict.SJust (hashData @StandardAlonzo plutusDataList))

mkDCertTx ::
  [ShelleyTxCert StandardAlonzo] ->
  Withdrawals StandardCrypto ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkDCertTx certs wdrl = Right $ mkSimpleTx True $ consCertTxBody certs wdrl

mkSimpleDCertTx ::
  [(StakeIndex, StakeCredential StandardCrypto -> ShelleyTxCert StandardAlonzo)] ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkSimpleDCertTx consDert st = do
  dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  mkDCertTx dcerts (Withdrawals mempty)

mkDCertPoolTx ::
  [ ( [StakeIndex]
    , PoolIndex
    , [StakeCredential StandardCrypto] -> KeyHash 'StakePool StandardCrypto -> ShelleyTxCert StandardAlonzo
    )
  ] ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkDCertPoolTx consDert st = do
  dcerts <- forM consDert $ \(stakeIxs, poolIx, mkDCert) -> do
    stakeCreds <- forM stakeIxs $ \stix -> resolveStakeCreds stix st
    let poolId = resolvePool poolIx st
    pure $ mkDCert stakeCreds poolId
  mkDCertTx dcerts (Withdrawals mempty)

mkScriptDCertTx ::
  [(StakeIndex, Bool, StakeCredential StandardCrypto -> ShelleyTxCert StandardAlonzo)] ->
  Bool ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkScriptDCertTx consDert valid st = do
  dcerts <- forM consDert $ \(stakeIndex, _, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  Right $
    mkScriptTx valid (mapMaybe prepareRedeemer $ zip [0 ..] consDert) $
      consCertTxBody dcerts (Withdrawals mempty)
  where
    prepareRedeemer (n, (StakeIndexScript bl, addRedeemer, _)) =
      if not addRedeemer
        then Nothing
        else
          Just $
            if bl
              then (RdmrPtr Cert n, (alwaysFailsScriptHash, alwaysFailsScript))
              else (RdmrPtr Cert n, (alwaysSucceedsScriptHash, alwaysSucceedsScript))
    prepareRedeemer _ = Nothing

mkDepositTxPools ::
  AlonzoUTxOIndex ->
  Integer ->
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkDepositTxPools inputIndex deposit sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      AlonzoTxOut addr' (MaryValue inputValue _) _ = snd inputPair
      change = AlonzoTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - deposit) []) Strict.SNothing
  Right $ mkSimpleTx True $ consTxBody input mempty (StrictSeq.fromList [change]) (Coin 0) mempty (allPoolStakeCert sta) (Withdrawals mempty)

mkDCertTxPools ::
  AlonzoLedgerState ->
  Either ForgingError (AlonzoTx StandardAlonzo)
mkDCertTxPools sta = Right $ mkSimpleTx True $ consCertTxBody (allPoolStakeCert sta) (Withdrawals mempty)

mkSimpleTx :: Bool -> AlonzoTxBody StandardAlonzo -> AlonzoTx StandardAlonzo
mkSimpleTx valid txBody =
  AlonzoTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

consPoolParams ::
  KeyHash 'StakePool StandardCrypto ->
  StakeCredential StandardCrypto ->
  [KeyHash 'Staking StandardCrypto] ->
  PoolParams StandardCrypto
consPoolParams poolId rwCred owners =
  PoolParams
    { ppId = poolId
    , ppVrf = hashVerKeyVRF . snd . mkVRFKeyPair $ RawSeed 0 0 0 0 0 -- undefined
    , ppPledge = Coin 1000
    , ppCost = Coin 10000
    , ppMargin = minBound
    , ppRewardAcnt = RewardAcnt Testnet rwCred
    , ppOwners = Set.fromList owners
    , ppRelays = StrictSeq.singleton $ SingleHostAddr Strict.SNothing Strict.SNothing Strict.SNothing
    , ppMetadata = Strict.SJust $ PoolMetadata (fromJust $ textToUrl "best.pool") "89237365492387654983275634298756"
    }

consPoolParamsTwoOwners ::
  [StakeCredential StandardCrypto] ->
  KeyHash 'StakePool StandardCrypto ->
  ShelleyTxCert StandardAlonzo
consPoolParamsTwoOwners [rwCred, KeyHashObj owner0, KeyHashObj owner1] poolId =
  ShelleyTxCertPool $ RegPool $ consPoolParams poolId rwCred [owner0, owner1]
consPoolParamsTwoOwners _ _ = panic "expected 2 pool owners"

mkScriptTx ::
  Bool ->
  [(RdmrPtr, (ScriptHash StandardCrypto, Core.Script StandardAlonzo))] ->
  AlonzoTxBody StandardAlonzo ->
  AlonzoTx StandardAlonzo
mkScriptTx valid rdmrs txBody =
  AlonzoTx
    { body = txBody
    , wits = witnesses
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
  where
    witnesses = mkWitnesses rdmrs [(hashData @StandardAlonzo plutusDataList, plutusDataList)]

mkWitnesses ::
  [(RdmrPtr, (ScriptHash StandardCrypto, Core.Script StandardAlonzo))] ->
  [(DataHash StandardCrypto, Data StandardAlonzo)] ->
  AlonzoTxWits StandardAlonzo
mkWitnesses rdmrs datas =
  AlonzoTxWits
    mempty
    mempty
    (Map.fromList $ snd <$> rdmrs)
    (TxDats $ Map.fromList datas)
    (Redeemers $ Map.fromList redeemers)
  where
    redeemers =
      fmap
        (,(plutusDataList, ExUnits 100 100))
        (fst <$> rdmrs)

mkUTxOAlonzo :: AlonzoTx StandardAlonzo -> [(TxIn StandardCrypto, AlonzoTxOut StandardAlonzo)]
mkUTxOAlonzo tx =
  [ (TxIn transId idx, out)
  | (out, idx) <- zip (toList (outputs' $ tx ^. Core.bodyTxL)) (TxIx <$> [0 ..])
  ]
  where
    transId = txid $ getField @"body" tx

emptyTxBody :: AlonzoTxBody StandardAlonzo
emptyTxBody =
  AlonzoTxBody
    mempty
    mempty
    mempty
    mempty
    (Withdrawals mempty)
    (Coin 0)
    (ValidityInterval Strict.SNothing Strict.SNothing)
    Strict.SNothing
    mempty
    mempty
    Strict.SNothing
    Strict.SNothing
    (Strict.SJust Testnet)

emptyTx :: AlonzoTx StandardAlonzo
emptyTx =
  AlonzoTx
    { body = emptyTxBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
