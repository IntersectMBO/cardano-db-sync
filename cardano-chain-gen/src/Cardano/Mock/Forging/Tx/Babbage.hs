{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Tx.Babbage (
  BabbageUTxOIndex,
  BabbageLedgerState,
  TxOutScriptType (..),
  DatumType (..),
  ReferenceScript (..),
  consTxBody,
  addValidityInterval,
  consPaymentTxBody,
  consCertTxBody,
  mkPaymentTx,
  mkPaymentTx',
  scriptSucceeds,
  getInlineScript,
  mkLockByScriptTx,
  mkOutFromType,
  mkUnlockScriptTx,
  mkUnlockScriptTxBabbage,
  mkScriptInp,
  mkScriptMint,
  mkMAssetsScriptTx,
  mkDCertTx,
  mkSimpleDCertTx,
  mkDummyRegisterTx,
  mkDCertPoolTx,
  mkScriptDCertTx,
  mkDepositTxPools,
  mkDCertTxPools,
  mkSimpleTx,
  consPoolParams,
  consPoolParamsTwoOwners,
  mkScriptTx,
  mkWitnesses,
  mkUTxOBabbage,
  mkUTxOCollBabbage,
  mkTxHash,
  mkFullTx,
  emptyTxBody,
  emptyTx,
) where

import Cardano.Crypto.VRF
import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.TxAuxData as Alonzo
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Babbage.Collateral (collOuts)
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (ADDRHASH)
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.TxAuxData
import Cardano.Ledger.Shelley.TxBody (
  PoolMetadata (..),
  PoolParams (..),
  StakePoolRelay (..),
 )
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Shelley.UTxO
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import Cardano.Ledger.Val
import Cardano.Mock.Forging.Crypto
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (sum, (.))
import Data.ByteString.Short (ShortByteString)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Maybe.Strict as Strict
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (LedgerState)
import Ouroboros.Consensus.Shelley.Eras (StandardBabbage, StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

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

consTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (BabbageTxOut StandardBabbage) ->
  StrictMaybe (BabbageTxOut StandardBabbage) ->
  Coin ->
  MultiAsset StandardCrypto ->
  [ShelleyTxCert StandardBabbage] ->
  Withdrawals StandardCrypto ->
  BabbageTxBody StandardBabbage
consTxBody ins cols ref outs collOut fees minted certs wdrl =
  BabbageTxBody
    ins
    cols
    ref
    (fmap (`Sized` 0) outs)
    (fmap (`Sized` 0) collOut)
    Strict.SNothing
    (StrictSeq.fromList certs)
    wdrl
    fees
    (ValidityInterval SNothing SNothing)
    Strict.SNothing
    mempty
    minted
    Strict.SNothing
    Strict.SNothing
    (Strict.SJust Testnet)

addValidityInterval ::
  SlotNo ->
  AlonzoTx StandardBabbage ->
  AlonzoTx StandardBabbage
addValidityInterval slotNo tx =
  tx {body = txBody'}
  where
    interval = ValidityInterval SNothing (SJust slotNo)
    txBody' = set vldtTxBodyL interval $ body tx

consPaymentTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (BabbageTxOut StandardBabbage) ->
  StrictMaybe (BabbageTxOut StandardBabbage) ->
  Coin ->
  MultiAsset StandardCrypto ->
  BabbageTxBody StandardBabbage
consPaymentTxBody ins cols ref outs colOut fees minted = consTxBody ins cols ref outs colOut fees minted mempty (Withdrawals mempty)

consCertTxBody :: Maybe (TxIn StandardCrypto) -> [ShelleyTxCert StandardBabbage] -> Withdrawals StandardCrypto -> BabbageTxBody StandardBabbage
consCertTxBody ref = consTxBody mempty mempty (toSet ref) mempty SNothing (Coin 0) mempty
  where
    toSet Nothing = mempty
    toSet (Just a) = Set.singleton a

mkPaymentTx ::
  BabbageUTxOIndex ->
  BabbageUTxOIndex ->
  Integer ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkPaymentTx inputIndex outputIndex amount fees sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta
  addr <- resolveAddress outputIndex sta

  let input = Set.singleton $ fst inputPair
      output = BabbageTxOut addr (valueFromList (fromIntegral amount) []) NoDatum SNothing
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change = BabbageTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) NoDatum SNothing
  Right $ mkSimpleTx True $ consPaymentTxBody input mempty mempty (StrictSeq.fromList [output, change]) SNothing (Coin fees) mempty

mkPaymentTx' ::
  BabbageUTxOIndex ->
  [(BabbageUTxOIndex, MaryValue StandardCrypto)] ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkPaymentTx' inputIndex outputIndex sta = do
  inputPair <- fst <$> resolveUTxOIndex inputIndex sta
  outps <- mapM mkOuts outputIndex

  let inps = Set.singleton $ fst inputPair
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      outValue = sum (unCoin . coin . snd <$> outputIndex)
      change = BabbageTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - outValue) []) NoDatum SNothing
  Right $ mkSimpleTx True $ consPaymentTxBody inps mempty mempty (StrictSeq.fromList $ outps ++ [change]) SNothing (Coin 0) mempty
  where
    mkOuts (outIx, vl) = do
      addr <- resolveAddress outIx sta
      Right $ BabbageTxOut addr vl NoDatum SNothing

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
    TxOutNoInline {} -> SNothing
    TxOutInline _ _ rs ->
      case rs of
        NoReferenceScript -> SNothing
        ReferenceScript rs' -> SJust rs'

mkLockByScriptTx ::
  BabbageUTxOIndex ->
  [TxOutScriptType] ->
  Integer ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkLockByScriptTx inputIndex txOutTypes amount fees sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      outs = mkOutFromType amount <$> txOutTypes
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change = BabbageTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) []) NoDatum SNothing
  -- No witnesses are necessary when the outputs is a script address. Only when it's consumed.
  Right $ mkSimpleTx True $ consPaymentTxBody input mempty mempty (StrictSeq.fromList $ outs <> [change]) SNothing (Coin fees) mempty

mkOutFromType :: Integer -> TxOutScriptType -> BabbageTxOut StandardBabbage
mkOutFromType amount txOutType =
  let outAddress = if scriptSucceeds txOutType then alwaysSucceedsScriptAddr else alwaysFailsScriptAddr
      datahash = hashData @StandardBabbage plutusDataList
      dt = case getDatum txOutType of
        NotInlineDatum -> DatumHash datahash
        InlineDatum -> Datum (Alonzo.dataToBinaryData plutusDataList)
        InlineDatumCBOR sbs -> Datum $ either error id $ Alonzo.makeBinaryData sbs
      scpt :: StrictMaybe (AlonzoScript StandardBabbage) = case getInlineScript txOutType of
        SNothing -> SNothing
        SJust True -> SJust alwaysSucceedsScript
        SJust False -> SJust alwaysFailsScript
   in BabbageTxOut outAddress (valueFromList (fromIntegral amount) []) dt scpt

mkUnlockScriptTx ::
  [BabbageUTxOIndex] ->
  BabbageUTxOIndex ->
  BabbageUTxOIndex ->
  Bool ->
  Integer ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkUnlockScriptTx inputIndex colInputIndex outputIndex succeeds amount fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
  addr <- resolveAddress outputIndex sta

  let inpts = Set.fromList $ fst <$> inputPairs
      colInput = Set.singleton $ fst colInputPair
      output = BabbageTxOut addr (valueFromList (fromIntegral amount) []) NoDatum SNothing
  Right
    $ mkScriptTx
      succeeds
      (mapMaybe mkScriptInp $ zip [0 ..] inputPairs)
    $ consPaymentTxBody inpts colInput mempty (StrictSeq.fromList [output]) SNothing (Coin fees) mempty

mkUnlockScriptTxBabbage ::
  [BabbageUTxOIndex] ->
  BabbageUTxOIndex ->
  BabbageUTxOIndex ->
  [BabbageUTxOIndex] ->
  Bool ->
  Bool ->
  Integer ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkUnlockScriptTxBabbage inputIndex colInputIndex outputIndex refInput compl succeeds amount fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) refInput
  (colInputPair, _) <- resolveUTxOIndex colInputIndex sta
  addr <- resolveAddress outputIndex sta

  let inpts = Set.fromList $ fst <$> inputPairs
      colOut = maybeToStrictMaybe $ mkOutFromType amount <$> collTxOutType
      refInpts = Set.fromList $ fst <$> refInputPairs
      colInput = Set.singleton $ fst colInputPair
      output = BabbageTxOut addr (valueFromList (fromIntegral amount) []) NoDatum SNothing
  Right
    $ mkScriptTx
      succeeds
      (mapMaybe mkScriptInp $ zip [0 ..] inputPairs)
    $ consPaymentTxBody inpts colInput refInpts (StrictSeq.fromList [output]) colOut (Coin fees) mempty
  where
    collTxOutType =
      if compl
        then Just $ TxOutInline True InlineDatum (ReferenceScript True)
        else Just $ TxOutNoInline True

mkScriptInp ::
  (Word64, (TxIn StandardCrypto, Core.TxOut StandardBabbage)) ->
  Maybe (RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))
mkScriptInp (n, (_txIn, txOut)) =
  case mscr of
    SNothing
      | addr == alwaysFailsScriptAddr ->
          Just (RdmrPtr Spend n, Just (alwaysFailsScriptHash, alwaysFailsScript))
    SNothing
      | addr == alwaysSucceedsScriptAddr ->
          Just (RdmrPtr Spend n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
    SNothing
      | addr == alwaysMintScriptAddr ->
          Just (RdmrPtr Spend n, Just (alwaysMintScriptHash, alwaysMintScript))
    SJust _ ->
      Just (RdmrPtr Spend n, Nothing)
    _ -> Nothing
  where
    addr = txOut ^. Core.addrTxOutL
    mscr = txOut ^. referenceScriptTxOutL

mkScriptMint ::
  MultiAsset StandardCrypto ->
  [(RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))]
mkScriptMint (MultiAsset mp) = mapMaybe f $ zip [0 ..] (Map.keys mp)
  where
    f (n, policyId)
      | policyID policyId == alwaysFailsScriptHash =
          Just (RdmrPtr Mint n, Just (alwaysFailsScriptHash, alwaysFailsScript))
      | policyID policyId == alwaysSucceedsScriptHash =
          Just
            (RdmrPtr Mint n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      | policyID policyId == alwaysMintScriptHash =
          Just (RdmrPtr Mint n, Just (alwaysMintScriptHash, alwaysMintScript))
      | otherwise = Nothing

mkMAssetsScriptTx ::
  [BabbageUTxOIndex] ->
  BabbageUTxOIndex ->
  [(BabbageUTxOIndex, MaryValue StandardCrypto)] ->
  [BabbageUTxOIndex] ->
  MultiAsset StandardCrypto ->
  Bool ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkMAssetsScriptTx inputIndex colInputIndex outputIndex refInput minted succeeds fees sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inputIndex
  refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) refInput
  colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInputIndex sta
  outps <- mapM mkOuts outputIndex
  let inpts = Set.fromList $ fst <$> inputPairs
      refInpts = Set.fromList $ fst <$> refInputPairs
  Right
    $ mkScriptTx
      succeeds
      ( mapMaybe mkScriptInp (zip [0 ..] inputPairs)
          ++ mkScriptMint minted
      )
    $ consPaymentTxBody inpts colInput refInpts (StrictSeq.fromList outps) SNothing (Coin fees) minted
  where
    mkOuts (outIx, vl) = do
      addr <- resolveAddress outIx sta
      Right $ BabbageTxOut addr vl (DatumHash (hashData @StandardBabbage plutusDataList)) SNothing

mkDCertTx ::
  [ShelleyTxCert StandardBabbage] ->
  Withdrawals StandardCrypto ->
  Maybe (TxIn StandardCrypto) ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkDCertTx certs wdrl ref = Right $ mkSimpleTx True $ consCertTxBody ref certs wdrl

mkSimpleDCertTx ::
  [(StakeIndex, StakeCredential StandardCrypto -> ShelleyTxCert StandardBabbage)] ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkSimpleDCertTx consDert st = do
  dcerts <- forM consDert $ \(stakeIndex, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  mkDCertTx dcerts (Withdrawals mempty) Nothing

mkDummyRegisterTx :: Int -> Int -> Either ForgingError (AlonzoTx StandardBabbage)
mkDummyRegisterTx n m =
  mkDCertTx
    (ShelleyTxCertDelegCert . ShelleyRegCert . KeyHashObj . KeyHash . mkDummyHash (Proxy @(ADDRHASH StandardCrypto)) . fromIntegral <$> [n, m])
    (Withdrawals mempty)
    Nothing

mkDCertPoolTx ::
  [ ( [StakeIndex]
    , PoolIndex
    , [StakeCredential StandardCrypto] -> KeyHash 'StakePool StandardCrypto -> ShelleyTxCert StandardBabbage
    )
  ] ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkDCertPoolTx consDert st = do
  dcerts <- forM consDert $ \(stakeIxs, poolIx, mkDCert) -> do
    stakeCreds <- forM stakeIxs $ \stix -> resolveStakeCreds stix st
    let poolId = resolvePool poolIx st
    pure $ mkDCert stakeCreds poolId
  mkDCertTx dcerts (Withdrawals mempty) Nothing

mkScriptDCertTx ::
  [(StakeIndex, Bool, StakeCredential StandardCrypto -> ShelleyTxCert StandardBabbage)] ->
  Bool ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkScriptDCertTx consDert valid st = do
  dcerts <- forM consDert $ \(stakeIndex, _, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure $ mkDCert cred
  Right $
    mkScriptTx valid (mapMaybe prepareRedeemer $ zip [0 ..] consDert) $
      consCertTxBody Nothing dcerts (Withdrawals mempty)
  where
    prepareRedeemer (n, (StakeIndexScript bl, addRedeemer, _)) =
      if not addRedeemer
        then Nothing
        else
          Just $
            if bl
              then (RdmrPtr Cert n, Just (alwaysFailsScriptHash, alwaysFailsScript))
              else (RdmrPtr Cert n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
    prepareRedeemer _ = Nothing

mkDepositTxPools ::
  BabbageUTxOIndex ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkDepositTxPools inputIndex deposit sta = do
  (inputPair, _) <- resolveUTxOIndex inputIndex sta

  let input = Set.singleton $ fst inputPair
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change = BabbageTxOut addr' (valueFromList (fromIntegral $ fromIntegral inputValue - deposit) []) NoDatum SNothing
  Right $ mkSimpleTx True $ consTxBody input mempty mempty (StrictSeq.fromList [change]) SNothing (Coin 0) mempty (allPoolStakeCert sta) (Withdrawals mempty)

mkDCertTxPools ::
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkDCertTxPools sta = Right $ mkSimpleTx True $ consCertTxBody Nothing (allPoolStakeCert sta) (Withdrawals mempty)

mkSimpleTx :: Bool -> BabbageTxBody StandardBabbage -> AlonzoTx StandardBabbage
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
    , ppRelays = StrictSeq.singleton $ SingleHostAddr SNothing SNothing SNothing
    , ppMetadata = SJust $ PoolMetadata (fromJust $ textToUrl "best.pool") "89237365492387654983275634298756"
    }

consPoolParamsTwoOwners ::
  [StakeCredential StandardCrypto] ->
  KeyHash 'StakePool StandardCrypto ->
  ShelleyTxCert StandardBabbage
consPoolParamsTwoOwners [rwCred, KeyHashObj owner0, KeyHashObj owner1] poolId =
  ShelleyTxCertPool $ RegPool $ consPoolParams poolId rwCred [owner0, owner1]
consPoolParamsTwoOwners _ _ = panic "expected 2 pool owners"

mkScriptTx ::
  Bool ->
  [(RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))] ->
  BabbageTxBody StandardBabbage ->
  AlonzoTx StandardBabbage
mkScriptTx valid rdmrs txBody =
  AlonzoTx
    { body = txBody
    , wits = witnesses
    , isValid = IsValid valid
    , auxiliaryData = maybeToStrictMaybe Nothing
    }
  where
    witnesses = mkWitnesses rdmrs [(hashData @StandardBabbage plutusDataList, plutusDataList)]

mkWitnesses ::
  [(RdmrPtr, Maybe (ScriptHash StandardCrypto, Core.Script StandardBabbage))] ->
  [(DataHash StandardCrypto, Data StandardBabbage)] ->
  AlonzoTxWits StandardBabbage
mkWitnesses rdmrs datas =
  AlonzoTxWits
    mempty
    mempty
    (Map.fromList $ mapMaybe snd rdmrs)
    (TxDats $ Map.fromList datas)
    (Redeemers $ Map.fromList redeemers)
  where
    redeemers =
      fmap
        (,(plutusDataList, ExUnits 100 100))
        (fst <$> rdmrs)

mkUTxOBabbage :: AlonzoTx StandardBabbage -> [(TxIn StandardCrypto, BabbageTxOut StandardBabbage)]
mkUTxOBabbage tx =
  [ (TxIn (mkTxHash tx) idx, out)
  | (out, idx) <- zip (toList (outputs' $ tx ^. Core.bodyTxL)) (TxIx <$> [0 ..])
  ]

mkUTxOCollBabbage :: AlonzoTx StandardBabbage -> [(TxIn StandardCrypto, BabbageTxOut StandardBabbage)]
mkUTxOCollBabbage tx = Map.toList $ unUTxO $ collOuts $ getField @"body" tx

mkTxHash :: AlonzoTx StandardBabbage -> TxId StandardCrypto
mkTxHash = txid . getField @"body"

emptyTxBody :: BabbageTxBody StandardBabbage
emptyTxBody =
  BabbageTxBody
    mempty
    mempty
    mempty
    mempty
    Strict.SNothing
    Strict.SNothing
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

emptyTx :: AlonzoTx StandardBabbage
emptyTx =
  AlonzoTx
    { body = emptyTxBody
    , wits = mempty
    , isValid = IsValid True
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

mkFullTx ::
  Int ->
  Integer ->
  BabbageLedgerState ->
  Either ForgingError (AlonzoTx StandardBabbage)
mkFullTx n m sta = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) inps
  let rdmrs = mapMaybe mkScriptInp $ zip [0 ..] inputPairs
  let witnesses = mkWitnesses rdmrs [(hashData @StandardBabbage plutusDataList, plutusDataList)]
  refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` sta) refInps
  colInput <- Set.singleton . fst . fst <$> resolveUTxOIndex colInps sta
  Right $
    AlonzoTx
      { body = txBody (mkInps inputPairs) (mkInps refInputPairs) colInput
      , wits = witnesses
      , isValid = IsValid True
      , auxiliaryData = Strict.SJust auxiliaryData'
      }
  where
    mkInps ins = Set.fromList $ fst <$> ins
    txBody ins cols ref =
      BabbageTxBody
        ins
        cols
        ref
        (fmap (`Sized` 0) outs)
        (fmap (`Sized` 0) collOut)
        Strict.SNothing
        (StrictSeq.fromList certs)
        wthdr
        (Coin m)
        (ValidityInterval Strict.SNothing Strict.SNothing)
        (Strict.SJust updates)
        witKeys
        minted
        Strict.SNothing
        Strict.SNothing
        (Strict.SJust Testnet)
    inps = [UTxOIndex $ n * 3 + 0]
    refInps = [UTxOIndex $ n * 3 + 1]
    colInps = UTxOIndex $ n * 3 + 2
    policy0 = PolicyID alwaysMintScriptHash
    policy1 = PolicyID alwaysSucceedsScriptHash
    assets0 = Map.fromList [(Prelude.head assetNames, 5), (assetNames !! 1, 2)]
    outValue0 = MaryValue 20 $ MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]
    addr0 = Addr Testnet (Prelude.head unregisteredAddresses) (StakeRefBase $ Prelude.head unregisteredStakeCredentials)
    addr2 = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) (StakeRefBase $ unregisteredStakeCredentials !! 2)
    out0, out1, out2 :: BabbageTxOut StandardBabbage
    out0 = BabbageTxOut addr0 outValue0 (DatumHash (hashData @StandardBabbage plutusDataList)) (Strict.SJust alwaysFailsScript)
    out1 = BabbageTxOut alwaysSucceedsScriptAddr outValue0 (DatumHash (hashData @StandardBabbage plutusDataList)) Strict.SNothing
    out2 = BabbageTxOut addr2 outValue0 (DatumHash (hashData @StandardBabbage plutusDataList)) (Strict.SJust alwaysFailsScript)
    outs = StrictSeq.fromList [out0, out1]
    collOut = Strict.SJust out2
    assetsMinted0 = Map.fromList [(Prelude.head assetNames, 10), (assetNames !! 1, 4)]
    minted = MultiAsset $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
    poolParams0 =
      consPoolParams
        (Prelude.head unregisteredPools)
        (unregisteredStakeCredentials !! 2)
        [unregisteredKeyHash !! 1, unregisteredKeyHash !! 2]
    poolParams1 =
      consPoolParams
        (unregisteredPools !! 2)
        (unregisteredStakeCredentials !! 2)
        [unregisteredKeyHash !! 1, unregisteredKeyHash !! 2]

    certs =
      [ ShelleyTxCertDelegCert $ ShelleyRegCert $ Prelude.head unregisteredStakeCredentials
      , ShelleyTxCertPool $ RegPool poolParams0
      , ShelleyTxCertPool $ RegPool poolParams1
      , ShelleyTxCertPool $ RetirePool (Prelude.head unregisteredPools) (EpochNo 0)
      , ShelleyTxCertDelegCert $ ShelleyUnRegCert $ unregisteredStakeCredentials !! 2
      , ShelleyTxCertDelegCert $ ShelleyDelegCert (unregisteredStakeCredentials !! 1) (unregisteredPools !! 2)
      , ShelleyTxCertMir $
          MIRCert
            ReservesMIR
            ( StakeAddressesMIR $
                Map.fromList
                  [ (Prelude.head unregisteredStakeCredentials, DeltaCoin 100)
                  , (unregisteredStakeCredentials !! 2, DeltaCoin 200)
                  ]
            )
      , ShelleyTxCertMir $
          MIRCert
            TreasuryMIR
            ( StakeAddressesMIR $
                Map.fromList
                  [ (Prelude.head unregisteredStakeCredentials, DeltaCoin 100)
                  , (unregisteredStakeCredentials !! 2, DeltaCoin 200)
                  ]
            )
      , ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR $ Coin 300)
      ]

    wthdr =
      Withdrawals $
        Map.fromList
          [ (RewardAcnt Testnet (unregisteredStakeCredentials !! 1), Coin 100)
          , (RewardAcnt Testnet (unregisteredStakeCredentials !! 1), Coin 100)
          ]

    witKeys =
      Set.fromList
        [ unregisteredWitnessKey !! 1
        , unregisteredWitnessKey !! 2
        ]

    auxiliaryDataMap = Map.fromList [(1, List []), (2, List [])]
    auxiliaryDataScripts = NonEmpty.fromList [toBinaryPlutus alwaysFailsScript]
    auxiliaryData' = Alonzo.AlonzoTxAuxData auxiliaryDataMap mempty (Map.singleton PlutusV2 auxiliaryDataScripts)

    costModels = CostModels (Map.fromList [(PlutusV2, testingCostModelV2)]) mempty mempty
    paramsUpdate = Core.emptyPParamsUpdate & ppuCostModelsL .~ Strict.SJust costModels -- {_costmdls = Strict.SJust costModels}
    proposed :: ProposedPPUpdates StandardBabbage
    proposed =
      ProposedPPUpdates $
        Map.fromList
          [ (unregisteredGenesisKeys !! 1, paramsUpdate)
          , (unregisteredGenesisKeys !! 2, paramsUpdate)
          ]
    updates :: Update StandardBabbage
    updates = Update proposed (EpochNo 0)

testingCostModelV2 :: CostModel
testingCostModelV2 =
  fromRight (error "testingCostModelV2 is not well-formed") $
    mkCostModel PlutusV2 (replicate 175 0)
