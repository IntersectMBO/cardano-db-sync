{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Tx.Babbage
  ( BabbageUTxOIndex
  , BabbageLedgerState
  , TxOutScriptType (..)
  , DatumType (..)
  , ReferenceScript (..)
  , consTxBody
  , addValidityInterval
  , consPaymentTxBody
  , consCertTxBody
  , mkPaymentTx
  , mkPaymentTx'
  , scriptSucceeds
  , getInlineScript
  , mkLockByScriptTx
  , mkOutFromType
  , mkUnlockScriptTx
  , mkUnlockScriptTxBabbage
  , mkScriptInp
  , mkScriptMint
  , mkMAssetsScriptTx
  , mkDCertTx
  , mkSimpleDCertTx
  , mkDummyRegisterTx
  , mkDCertPoolTx
  , mkScriptDCertTx
  , mkDepositTxPools
  , mkDCertTxPools
  , mkSimpleTx
  , consPoolParams
  , consPoolParamsTwoOwners
  , mkScriptTx
  , mkWitnesses
  , addMetadata
  , mkUTxOBabbage
  , mkUTxOCollBabbage
  , mkTxHash
  , mkFullTx
  , emptyTxBody
  , emptyTx
  ) where

import           Cardano.Prelude hiding (sum, (.))

import           Data.ByteString.Short (ShortByteString)
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
import           Cardano.Ledger.Alonzo.Language
import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.Alonzo.TxWitness
import           Cardano.Ledger.Babbage.PParams
import           Cardano.Ledger.Babbage.Tx
import           Cardano.Ledger.Babbage.TxBody
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Crypto (ADDRHASH)
import           Cardano.Ledger.Hashes
import           Cardano.Ledger.Keys
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Serialization
import           Cardano.Ledger.Shelley.Metadata
import           Cardano.Ledger.Shelley.PParams hiding (emptyPParamsUpdate)
import           Cardano.Ledger.Shelley.TxBody (DCert (..), Delegation (..), DelegCert (..), MIRCert (..), MIRPot (..),
                   PoolCert (..), MIRTarget (..), PoolMetadata (..), PoolParams (..), StakePoolRelay (..), Wdrl (..))
import           Cardano.Ledger.ShelleyMA.Timelocks
import           Cardano.Ledger.TxIn (TxId, TxIn (..), txid)

import           Ouroboros.Consensus.Cardano.Block (LedgerState)
import           Ouroboros.Consensus.Shelley.Eras (StandardBabbage, StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Ledger.Babbage.Collateral (collOuts)
import           Cardano.Ledger.Shelley.UTxO
import           Cardano.Mock.Forging.Crypto
import           Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

import qualified Plutus.V1.Ledger.EvaluationContext as PV1

type BabbageUTxOIndex = UTxOIndex StandardBabbage
type BabbageLedgerState = LedgerState (ShelleyBlock PraosStandard StandardBabbage)

data TxOutScriptType
  = TxOutNoInline Bool -- nothing is inlined, like in Alonzo
  | TxOutInline Bool DatumType ReferenceScript -- validScript, inlineDatum, reference script

data DatumType
  = NotInlineDatum
  | InlineDatum
  | InlineDatumCBOR ShortByteString

data ReferenceScript
  = NoReferenceScript
  | ReferenceScript Bool

consTxBody :: Set (TxIn StandardCrypto)
           -> Set (TxIn StandardCrypto)
           -> Set (TxIn StandardCrypto)
           -> StrictSeq (TxOut StandardBabbage)
           -> StrictMaybe (TxOut StandardBabbage)
           -> Coin
           -> Value StandardCrypto
           -> [DCert StandardCrypto]
           -> Wdrl StandardCrypto
           -> TxBody StandardBabbage
consTxBody ins cols ref outs collOut fees minted certs wdrl =
    TxBody
      ins
      cols
      ref
      (fmap (`Sized` 0) outs)
      (fmap (`Sized` 0) collOut)
      Strict.SNothing
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
                    -> ValidatedTx StandardBabbage
                    -> ValidatedTx StandardBabbage
addValidityInterval slotNo tx =
    tx {body = txBody'}
  where
    interval = ValidityInterval Strict.SNothing (Strict.SJust slotNo)
    -- TxBody has a restricted export via pattern synonyms, there is no better way to do this.
    TxBody a b c d e f n o p _ h i j k l m = body tx
    txBody' = TxBody a b c d e f n o p interval h i j k l m

consPaymentTxBody :: Set (TxIn StandardCrypto)
                  -> Set (TxIn StandardCrypto)
                  -> Set (TxIn StandardCrypto)
                  -> StrictSeq (TxOut StandardBabbage)
                  -> StrictMaybe (TxOut StandardBabbage)
                  -> Coin -> Value StandardCrypto
                  -> TxBody StandardBabbage
consPaymentTxBody ins cols ref outs colOut fees minted = consTxBody ins cols ref outs colOut fees minted mempty (Wdrl mempty)

consCertTxBody :: Maybe (TxIn StandardCrypto) -> [DCert StandardCrypto] -> Wdrl StandardCrypto -> TxBody StandardBabbage
consCertTxBody ref = consTxBody mempty mempty (toSet ref) mempty Strict.SNothing (Coin 0) mempty
  where
    toSet Nothing = mempty
    toSet (Just a) = Set.singleton a

mkPaymentTx :: BabbageUTxOIndex -> BabbageUTxOIndex -> Integer -> Integer
            -> BabbageLedgerState
            -> Either ForgingError (ValidatedTx StandardBabbage)
mkPaymentTx inputIndex outputIndex amount fees sta = do
    (inputPair, _) <- resolveUTxOIndex inputIndex sta
    addr <- resolveAddress outputIndex sta

    let input = Set.singleton $ fst inputPair
        output = TxOut addr (valueFromList (fromIntegral amount) []) NoDatum Strict.SNothing
        TxOut addr' (Value inputValue _) _ _ = snd inputPair
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) NoDatum Strict.SNothing
    Right $ mkSimpleTx True $ consPaymentTxBody input mempty mempty (StrictSeq.fromList [output, change]) Strict.SNothing (Coin fees) mempty

mkPaymentTx' :: BabbageUTxOIndex
             -> [(BabbageUTxOIndex, Value StandardCrypto)]
             -> BabbageLedgerState
             -> Either ForgingError (ValidatedTx StandardBabbage)
mkPaymentTx' inputIndex outputIndex sta = do
    inputPair <- fst <$> resolveUTxOIndex inputIndex sta
    outps <- mapM mkOuts outputIndex

    let inps = Set.singleton $ fst inputPair
        TxOut addr' (Value inputValue _) _ _ = snd inputPair
        outValue = sum ((\ (Value vl _) -> vl) . snd <$> outputIndex)
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - outValue) []) NoDatum Strict.SNothing
    Right $ mkSimpleTx True $ consPaymentTxBody inps mempty mempty (StrictSeq.fromList $ outps ++ [change]) Strict.SNothing (Coin 0) mempty
  where
    mkOuts (outIx, vl) = do
        addr <- resolveAddress outIx sta
        Right $ TxOut addr vl NoDatum Strict.SNothing

scriptSucceeds :: TxOutScriptType -> Bool
scriptSucceeds st =
  case st of
    TxOutNoInline sc -> sc
    TxOutInline sc _ _ -> sc

getDatum :: TxOutScriptType -> DatumType
getDatum st =
  case st of
    TxOutNoInline {} -> NotInlineDatum
    TxOutInline _ inl _ -> inl

getInlineScript :: TxOutScriptType -> StrictMaybe Bool
getInlineScript st =
  case st of
    TxOutNoInline {} -> Strict.SNothing
    TxOutInline _ _ rs ->
      case rs of
        NoReferenceScript -> Strict.SNothing
        ReferenceScript rs' -> Strict.SJust rs'

mkLockByScriptTx :: BabbageUTxOIndex -> [TxOutScriptType] -> Integer -> Integer
                 -> BabbageLedgerState
                 -> Either ForgingError (ValidatedTx StandardBabbage)
mkLockByScriptTx inputIndex txOutTypes amount fees sta = do
    (inputPair, _) <- resolveUTxOIndex inputIndex sta

    let input = Set.singleton $ fst inputPair
        outs = mkOutFromType amount <$> txOutTypes
        TxOut addr' (Value inputValue _) _ _ = snd inputPair
        change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) NoDatum Strict.SNothing
    -- No witnesses are necessary when the outputs is a script address. Only when it's consumed.
    Right $ mkSimpleTx True $ consPaymentTxBody input mempty mempty (StrictSeq.fromList $ outs <> [change]) Strict.SNothing (Coin fees) mempty

mkOutFromType :: Integer -> TxOutScriptType -> TxOut StandardBabbage
mkOutFromType amount txOutType =
    let outAddress = if scriptSucceeds txOutType then alwaysSucceedsScriptAddr else alwaysFailsScriptAddr
        datahash = hashData @StandardBabbage plutusDataList
        dt = case getDatum txOutType of
          NotInlineDatum -> DatumHash datahash
          InlineDatum -> Datum (dataToBinaryData plutusDataList)
          InlineDatumCBOR sbs -> Datum $ either error id $ makeBinaryData sbs
        scpt = case getInlineScript txOutType of
          Strict.SNothing -> Strict.SNothing
          Strict.SJust True -> Strict.SJust alwaysSucceedsScript
          Strict.SJust False -> Strict.SJust alwaysFailsScript
    in TxOut outAddress (valueFromList (fromIntegral amount) []) dt scpt

mkUnlockScriptTx :: [BabbageUTxOIndex] -> BabbageUTxOIndex -> BabbageUTxOIndex
                 -> Bool -> Integer -> Integer -> BabbageLedgerState
                 -> Either ForgingError (ValidatedTx StandardBabbage)
mkUnlockScriptTx inputIndex colInputIndex outputIndex succeeds amount fees sta = do
    inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
    (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
    addr <- resolveAddress outputIndex sta

    let inpts = Set.fromList $ fst <$> inputPairs
        colInput = Set.singleton $ fst colInputPair
        output = TxOut addr (valueFromList (fromIntegral amount) []) NoDatum Strict.SNothing
    Right $ mkScriptTx succeeds
      (mapMaybe mkScriptInp $ zip [0..] inputPairs)
      $ consPaymentTxBody inpts colInput mempty (StrictSeq.fromList [output]) Strict.SNothing (Coin fees) mempty

mkUnlockScriptTxBabbage :: [BabbageUTxOIndex] -> BabbageUTxOIndex -> BabbageUTxOIndex
                        -> [BabbageUTxOIndex] -> Bool
                        -> Bool -> Integer -> Integer -> BabbageLedgerState
                        -> Either ForgingError (ValidatedTx StandardBabbage)
mkUnlockScriptTxBabbage inputIndex colInputIndex outputIndex refInput compl succeeds amount fees sta = do
    inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
    refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) refInput
    (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
    addr <- resolveAddress outputIndex sta

    let inpts = Set.fromList $ fst <$> inputPairs
        colOut =  maybeToStrictMaybe $ mkOutFromType amount <$> collTxOutType
        refInpts = Set.fromList $ fst <$> refInputPairs
        colInput = Set.singleton $ fst colInputPair
        output = TxOut addr (valueFromList (fromIntegral amount) []) NoDatum Strict.SNothing
    Right $ mkScriptTx succeeds
      (mapMaybe mkScriptInp $ zip [0..] inputPairs)
      $ consPaymentTxBody inpts colInput refInpts (StrictSeq.fromList [output]) colOut (Coin fees) mempty
  where
    collTxOutType = if compl then Just $ TxOutInline True InlineDatum (ReferenceScript True)
                    else Just $ TxOutNoInline True

mkScriptInp :: (Word64, (TxIn StandardCrypto, Core.TxOut StandardBabbage))
            -> Maybe (RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))
mkScriptInp (n, (_txIn, TxOut addr _ _ mscr)) =
    case mscr of
      Strict.SNothing | addr == alwaysFailsScriptAddr ->
        Just (RdmrPtr Spend n, Just (alwaysFailsScriptHash, alwaysFailsScript))
      Strict.SNothing | addr == alwaysSucceedsScriptAddr ->
        Just (RdmrPtr Spend n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      Strict.SNothing | addr == alwaysMintScriptAddr ->
        Just (RdmrPtr Spend n, Just (alwaysMintScriptHash, alwaysMintScript))
      Strict.SJust _ ->
        Just (RdmrPtr Spend n, Nothing)
      _ -> Nothing

mkScriptMint :: Value StandardCrypto
             -> [(RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))]
mkScriptMint (Value _ mp) = mapMaybe f $ zip [0..] (Map.keys mp)
  where
    f (n, policyId)
      | policyID policyId == alwaysFailsScriptHash
      = Just (RdmrPtr Mint n, Just (alwaysFailsScriptHash, alwaysFailsScript))
      | policyID policyId == alwaysSucceedsScriptHash
      = Just
          (RdmrPtr Mint n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      | policyID policyId == alwaysMintScriptHash
      = Just (RdmrPtr Mint n, Just (alwaysMintScriptHash, alwaysMintScript))
      | otherwise = Nothing

mkMAssetsScriptTx :: [BabbageUTxOIndex] -> BabbageUTxOIndex
                  -> [(BabbageUTxOIndex, Value StandardCrypto)] -> [BabbageUTxOIndex]
                  -> Value StandardCrypto -> Bool -> Integer -> BabbageLedgerState
                  -> Either ForgingError (ValidatedTx StandardBabbage)
mkMAssetsScriptTx inputIndex colInputIndex outputIndex refInput minted succeeds fees sta = do
    inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
    refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) refInput
    colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInputIndex sta
    outps <- mapM mkOuts outputIndex
    let inpts = Set.fromList $ fst <$> inputPairs
        refInpts = Set.fromList $ fst <$> refInputPairs
    Right $ mkScriptTx succeeds
      (mapMaybe mkScriptInp (zip [0 .. ] inputPairs)
        ++ mkScriptMint minted)
      $ consPaymentTxBody inpts colInput refInpts (StrictSeq.fromList outps) Strict.SNothing (Coin fees) minted

  where
    mkOuts (outIx, vl) = do
        addr <- resolveAddress outIx sta
        Right $ TxOut addr vl (DatumHash (hashData @StandardBabbage plutusDataList)) Strict.SNothing

mkDCertTx :: [DCert StandardCrypto] -> Wdrl StandardCrypto -> Maybe (TxIn StandardCrypto)
          -> Either ForgingError (ValidatedTx StandardBabbage)
mkDCertTx certs wdrl ref = Right $ mkSimpleTx True $ consCertTxBody ref certs wdrl

mkSimpleDCertTx :: [(StakeIndex, StakeCredential StandardCrypto -> DCert StandardCrypto)]
                -> BabbageLedgerState
                -> Either ForgingError (ValidatedTx StandardBabbage)
mkSimpleDCertTx consDert st = do
    dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
      cred <- resolveStakeCreds stakeIndex st
      pure $ mkDCert cred
    mkDCertTx dcerts (Wdrl mempty) Nothing

mkDummyRegisterTx :: Int -> Int -> Either ForgingError (ValidatedTx StandardBabbage)
mkDummyRegisterTx n m =
  mkDCertTx (DCertDeleg . RegKey . KeyHashObj . KeyHash . mkDummyHash (Proxy @(ADDRHASH StandardCrypto)) . fromIntegral <$> [n, m])
            (Wdrl mempty) Nothing

mkDCertPoolTx :: [([StakeIndex], PoolIndex,
                  [StakeCredential StandardCrypto] -> KeyHash 'StakePool StandardCrypto -> DCert StandardCrypto)]
              -> BabbageLedgerState
              -> Either ForgingError (ValidatedTx StandardBabbage)
mkDCertPoolTx consDert st = do
    dcerts <- forM consDert $ \(stakeIxs, poolIx, mkDCert) -> do
      stakeCreds <- forM stakeIxs $ \ix -> resolveStakeCreds ix st
      let poolId = resolvePool poolIx st
      pure $ mkDCert stakeCreds poolId
    mkDCertTx dcerts (Wdrl mempty) Nothing

mkScriptDCertTx :: [(StakeIndex, Bool, StakeCredential StandardCrypto -> DCert StandardCrypto)]
                -> Bool -> BabbageLedgerState
                -> Either ForgingError (ValidatedTx StandardBabbage)
mkScriptDCertTx consDert valid st = do
    dcerts <- forM consDert $ \(stakeIndex, _, mkDCert) -> do
      cred <- resolveStakeCreds stakeIndex st
      pure $ mkDCert cred
    Right $ mkScriptTx valid (mapMaybe prepareRedeemer $ zip [0..] consDert)
              $ consCertTxBody Nothing dcerts (Wdrl mempty)
  where
    prepareRedeemer (n, (StakeIndexScript bl, addRedeemer, _)) =
        if not addRedeemer then Nothing else Just $
        if bl then (RdmrPtr Cert n, Just (alwaysFailsScriptHash, alwaysFailsScript))
              else (RdmrPtr Cert n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
    prepareRedeemer _ = Nothing

mkDepositTxPools :: BabbageUTxOIndex -> Integer -> BabbageLedgerState
                 -> Either ForgingError (ValidatedTx StandardBabbage)
mkDepositTxPools inputIndex deposit sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      TxOut addr' (Value inputValue _) _ _ = snd inputPair
      change = TxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - deposit) []) NoDatum Strict.SNothing
  Right $ mkSimpleTx True $ consTxBody input mempty mempty (StrictSeq.fromList [change]) Strict.SNothing (Coin 0) mempty (allPoolStakeCert sta) (Wdrl mempty)

mkDCertTxPools :: BabbageLedgerState
               -> Either ForgingError (ValidatedTx StandardBabbage)
mkDCertTxPools sta = Right $ mkSimpleTx True $ consCertTxBody Nothing (allPoolStakeCert sta) (Wdrl mempty)

mkSimpleTx :: Bool -> TxBody StandardBabbage -> ValidatedTx StandardBabbage
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

mkScriptTx :: Bool -> [(RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))]
           -> TxBody StandardBabbage
           -> ValidatedTx StandardBabbage
mkScriptTx valid rdmrs txBody = ValidatedTx
    { body = txBody
    , wits = witnesses
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
  where
    witnesses = mkWitnesses rdmrs [(hashData @StandardBabbage plutusDataList, plutusDataList)]

mkWitnesses :: [(RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))]
            -> [(DataHash StandardCrypto, Data StandardBabbage)]
            -> TxWitness StandardBabbage
mkWitnesses rdmrs datas =
    TxWitness
      mempty
      mempty
      (Map.fromList $ mapMaybe snd rdmrs)
      (TxDats $ Map.fromList datas)
      (Redeemers $ Map.fromList redeemers)
  where
    redeemers = fmap (, (plutusDataList, ExUnits 100 100))
                    (fst <$> rdmrs)

addMetadata :: Word64 -> Word64 -> ValidatedTx StandardBabbage
            -> ValidatedTx StandardBabbage
addMetadata n m tx = tx { auxiliaryData = Strict.SJust $ AuxiliaryData mp mempty}
  where
    mp = Map.fromList [(n, List []), (m , List [])]

mkUTxOBabbage :: ValidatedTx StandardBabbage -> [(TxIn StandardCrypto, TxOut StandardBabbage)]
mkUTxOBabbage tx =
    [ (TxIn (mkTxHash tx) idx, out)
    | (out, idx) <- zip (toList $ getField @"outputs" (getField @"body" tx)) (TxIx <$> [0 ..])
    ]

mkUTxOCollBabbage :: ValidatedTx StandardBabbage -> [(TxIn StandardCrypto, TxOut StandardBabbage)]
mkUTxOCollBabbage tx = Map.toList $ unUTxO $ collOuts $ getField @"body" tx

mkTxHash :: ValidatedTx StandardBabbage -> TxId StandardCrypto
mkTxHash = txid . getField @"body"

emptyTxBody :: TxBody StandardBabbage
emptyTxBody = TxBody
  mempty
  mempty
  mempty
  mempty
  Strict.SNothing
  Strict.SNothing
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

emptyTx :: ValidatedTx StandardBabbage
emptyTx = ValidatedTx
    { body = emptyTxBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

mkFullTx
  :: Int -> Integer -> BabbageLedgerState
  -> Either ForgingError (ValidatedTx StandardBabbage)
mkFullTx n m sta = do
    inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inps
    let rdmrs = mapMaybe mkScriptInp $ zip [0..] inputPairs
    let witnesses = mkWitnesses rdmrs [(hashData @StandardBabbage plutusDataList, plutusDataList)]
    refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) refInps
    colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInps sta
    Right $ ValidatedTx
      { body = txBody (mkInps inputPairs) (mkInps refInputPairs) colInput
      , wits = witnesses
      , isValid = IsValid True
      , auxiliaryData = Strict.SJust auxiliaryData'
      }
  where
    mkInps ins = Set.fromList $ fst <$> ins
    txBody ins cols ref =
      TxBody
        ins cols ref (fmap (`Sized` 0) outs) (fmap (`Sized` 0) collOut)
        Strict.SNothing (StrictSeq.fromList certs) wthdr (Coin m)
        (ValidityInterval Strict.SNothing Strict.SNothing)
        (Strict.SJust updates) witKeys minted
        Strict.SNothing Strict.SNothing (Strict.SJust Testnet)
    inps = [UTxOIndex $ n * 3 + 0]
    refInps = [UTxOIndex $ n * 3 + 1]
    colInps = UTxOIndex $ n * 3 + 2
    policy0 = PolicyID alwaysMintScriptHash
    policy1 = PolicyID alwaysSucceedsScriptHash
    assets0 = Map.fromList [(Prelude.head assetNames, 5), (assetNames !! 1, 2)]
    outValue0 = Value 20 $ Map.fromList [(policy0, assets0), (policy1, assets0)]
    addr0 = Addr Testnet (Prelude.head unregisteredAddresses) (StakeRefBase $ Prelude.head unregisteredStakeCredentials)
    addr2 = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) (StakeRefBase $ unregisteredStakeCredentials !! 2)
    out0, out1, out2 :: TxOut StandardBabbage
    out0 = TxOut addr0 outValue0 (DatumHash (hashData @StandardBabbage plutusDataList)) (Strict.SJust alwaysFailsScript)
    out1 = TxOut alwaysSucceedsScriptAddr outValue0 (DatumHash (hashData @StandardBabbage plutusDataList)) Strict.SNothing
    out2 = TxOut addr2 outValue0 (DatumHash (hashData @StandardBabbage plutusDataList)) (Strict.SJust alwaysFailsScript)
    outs = StrictSeq.fromList [out0, out1]
    collOut = Strict.SJust out2
    assetsMinted0 = Map.fromList [(Prelude.head assetNames, 10), (assetNames !! 1, 4)]
    minted = Value 100 $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
    poolParams0 = consPoolParams
                   (Prelude.head unregisteredPools)
                   (unregisteredStakeCredentials !! 2)
                   [unregisteredKeyHash !! 1, unregisteredKeyHash !! 2]
    poolParams1 = consPoolParams
                   (unregisteredPools !! 2)
                   (unregisteredStakeCredentials !! 2)
                   [unregisteredKeyHash !! 1, unregisteredKeyHash !! 2]

    certs = [ DCertDeleg $ RegKey $ Prelude.head unregisteredStakeCredentials
            , DCertPool $ RegPool poolParams0
            , DCertPool $ RegPool poolParams1
            , DCertPool $ RetirePool (Prelude.head unregisteredPools) (EpochNo 0)
            , DCertDeleg $ DeRegKey $ unregisteredStakeCredentials !! 2
            , DCertDeleg $ Delegate $ Delegation (unregisteredStakeCredentials !! 1) (unregisteredPools !! 2)
            , DCertMir $ MIRCert ReservesMIR (StakeAddressesMIR $ Map.fromList
                [ (Prelude.head unregisteredStakeCredentials, DeltaCoin 100)
                , (unregisteredStakeCredentials !! 2, DeltaCoin 200)
                ])
            , DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR $ Map.fromList
                [ (Prelude.head unregisteredStakeCredentials, DeltaCoin 100)
                , (unregisteredStakeCredentials !! 2, DeltaCoin 200)
                ])
            , DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR $ Coin 300)
            ]

    wthdr = Wdrl $ Map.fromList
              [ (RewardAcnt Testnet (unregisteredStakeCredentials !! 1), Coin 100)
              , (RewardAcnt Testnet (unregisteredStakeCredentials !! 1), Coin 100)
              ]

    witKeys = Set.fromList
                [ unregisteredWitnessKey !! 1
                , unregisteredWitnessKey !! 2
                ]

    auxiliaryData' = AuxiliaryData auxiliaryDataMap auxiliaryDataScripts
    auxiliaryDataMap = Map.fromList [(1, List []), (2 , List [])]
    auxiliaryDataScripts = StrictSeq.fromList [alwaysFailsScript]

    costModels = CostModels $ Map.fromList [(PlutusV2, testingCostModelV2)]
    paramsUpdate = emptyPParamsUpdate {_costmdls = Strict.SJust costModels}
    proposed :: ProposedPPUpdates StandardBabbage
    proposed = ProposedPPUpdates $ Map.fromList
        [ (unregisteredGenesisKeys !! 1, paramsUpdate)
        , (unregisteredGenesisKeys !! 2, paramsUpdate)
        ]
    updates :: Update StandardBabbage
    updates = Update proposed (EpochNo 0)

testingCostModelV2 :: CostModel
testingCostModelV2 =
  fromRight (error "testingCostModelV2 is not well-formed") $
    mkCostModel PlutusV2 PV1.costModelParamsForTesting -- TODO use PV2 when it exists
