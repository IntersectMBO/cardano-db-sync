{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Mock.Forging.Tx.Conway (
  Babbage.TxOutScriptType (..),
  consTxBody,
  consCertTxBody,
  consPoolParams,
  mkPaymentTx,
  mkPaymentTx',
  mkLockByScriptTx,
  mkUnlockScriptTx,
  mkScriptTx,
  mkSimpleTx,
  mkDCertTx,
  mkDCertTxPools,
  mkSimpleDCertTx,
  mkScriptDCertTx,
  mkMultiAssetsScriptTx,
  mkDepositTxPools,
  mkDummyRegisterTx,
  mkTxDelegCert,
  mkRegTxCert,
  mkUnRegTxCert,
  mkDelegTxCert,
  mkFullTx,
  mkScriptMint,
  Babbage.mkScriptInp,
  mkWitnesses,
  mkUTxOConway,
  addValidityInterval,
) where

import Cardano.Ledger.Address (Addr (..), RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (Tag (..))
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), Network (..))
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxCert hiding (mkDelegTxCert)
import Cardano.Ledger.Conway.TxOut (BabbageTxOut (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential, StakeReference (..))
import Cardano.Ledger.Crypto (ADDRHASH ())
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), valueFromList)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..), coin)
import Cardano.Mock.Forging.Tx.Alonzo (
  addValidityInterval,
  mkScriptTx,
  mkUTxOAlonzo,
  mkWitnesses,
 )
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Examples
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude
import Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import Data.Sequence.Strict (StrictSeq ())
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Ouroboros.Consensus.Cardano.Block (LedgerState ())
import Ouroboros.Consensus.Shelley.Eras (StandardConway (), StandardCrypto ())
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Prelude (error, (!!))
import qualified Prelude

type ConwayUTxOIndex = UTxOIndex StandardConway
type ConwayLedgerState = LedgerState (ShelleyBlock PraosStandard StandardConway)

consTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (BabbageTxOut StandardConway) ->
  StrictMaybe (BabbageTxOut StandardConway) ->
  Coin ->
  MultiAsset StandardCrypto ->
  [ConwayTxCert StandardConway] ->
  Withdrawals StandardCrypto ->
  ConwayTxBody StandardConway
consTxBody ins cols ref outs colOut fees minted certs withdrawals =
  ConwayTxBody
    { ctbSpendInputs = ins
    , ctbCollateralInputs = cols
    , ctbReferenceInputs = ref
    , ctbOutputs = (`Sized` 0) <$> outs
    , ctbCollateralReturn = (`Sized` 0) <$> colOut
    , ctbTotalCollateral = SNothing
    , ctbCerts = StrictSeq.fromList certs
    , ctbWithdrawals = withdrawals
    , ctbTxfee = fees
    , ctbVldt = ValidityInterval SNothing SNothing
    , ctbReqSignerHashes = mempty
    , ctbMint = minted
    , ctbScriptIntegrityHash = SNothing
    , ctbAdHash = SNothing
    , ctbTxNetworkId = SJust Testnet
    , ctbVotingProcedures = VotingProcedures mempty
    , ctbProposalProcedures = mempty
    , ctbCurrentTreasuryValue = SNothing
    , ctbTreasuryDonation = Coin 0
    }

consCertTxBody ::
  Maybe (TxIn StandardCrypto) ->
  [ConwayTxCert StandardConway] ->
  Withdrawals StandardCrypto ->
  ConwayTxBody StandardConway
consCertTxBody ref = consTxBody mempty mempty (toSet ref) mempty SNothing (Coin 0) mempty
  where
    toSet Nothing = mempty
    toSet (Just a) = Set.singleton a

mkPaymentTx ::
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkPaymentTx inputIndex outputIndex amount = mkPaymentTx' inputIndex outputIndices
  where
    outputIndices = [(outputIndex, valueFromList amount [])]

mkPaymentTx' ::
  ConwayUTxOIndex ->
  [(ConwayUTxOIndex, MaryValue StandardCrypto)] ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkPaymentTx' inputIndex outputIndices fees state' = do
  (inputPair, _) <- resolveUTxOIndex inputIndex state'
  outputs <- mapM mkOutputs outputIndices

  let inputs = Set.singleton (fst inputPair)
      outValue = sum $ map (unCoin . coin . snd) outputIndices
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change =
        BabbageTxOut
          addr'
          (valueFromList (fromIntegral $ fromIntegral inputValue - outValue - fees) [])
          Alonzo.NoDatum
          SNothing

  pure $
    mkSimpleTx True $
      consPaymentTxBody
        inputs
        mempty
        mempty
        (StrictSeq.fromList $ outputs <> [change])
        SNothing
        (Coin fees)
        mempty
  where
    mkOutputs (outIx, val) = do
      addr <- resolveAddress outIx state'
      pure (BabbageTxOut addr val Alonzo.NoDatum SNothing)

mkLockByScriptTx ::
  ConwayUTxOIndex ->
  [Babbage.TxOutScriptType] ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkLockByScriptTx inputIndex txOutTypes amount fees state' = do
  (inputPair, _) <- resolveUTxOIndex inputIndex state'

  let inputs = Set.singleton (fst inputPair)
      outputs = mkOutFromType amount <$> txOutTypes
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change =
        BabbageTxOut
          addr'
          (valueFromList (fromIntegral $ fromIntegral inputValue - amount - fees) [])
          Alonzo.NoDatum
          SNothing

  pure $
    mkSimpleTx True $
      consPaymentTxBody
        inputs
        mempty
        mempty
        (StrictSeq.fromList $ outputs <> [change])
        SNothing
        (Coin fees)
        mempty

mkUnlockScriptTx ::
  [ConwayUTxOIndex] ->
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  Bool ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkUnlockScriptTx inputIndex colInputIndex outputIndex succeeds amount fees state' = do
  inputPairs <- map fst <$> mapM (`resolveUTxOIndex` state') inputIndex
  (colInputPair, _) <- resolveUTxOIndex colInputIndex state'
  addr <- resolveAddress outputIndex state'

  let inputs = Set.fromList $ map fst inputPairs
      colInputs = Set.singleton $ fst colInputPair
      output =
        BabbageTxOut
          addr
          (valueFromList (fromIntegral amount) [])
          Alonzo.NoDatum
          SNothing

  pure $
    mkScriptTx succeeds (mkScriptInps inputPairs) $
      consPaymentTxBody
        inputs
        colInputs
        mempty
        (StrictSeq.singleton output)
        SNothing
        (Coin fees)
        mempty

mkDCertTx ::
  [ConwayTxCert StandardConway] ->
  Withdrawals StandardCrypto ->
  Maybe (TxIn StandardCrypto) ->
  Either ForgingError (AlonzoTx StandardConway)
mkDCertTx certs wdrl ref = Right (mkSimpleTx True $ consCertTxBody ref certs wdrl)

mkDCertTxPools :: ConwayLedgerState -> Either ForgingError (AlonzoTx StandardConway)
mkDCertTxPools state' =
  Right $
    mkSimpleTx True $
      consCertTxBody Nothing (allPoolStakeCert' state') (Withdrawals mempty)

mkSimpleTx :: Bool -> ConwayTxBody StandardConway -> AlonzoTx StandardConway
mkSimpleTx isValid' txBody =
  AlonzoTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid isValid'
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

mkSimpleDCertTx ::
  [(StakeIndex, StakeCredential StandardCrypto -> ConwayTxCert StandardConway)] ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkSimpleDCertTx consDCert st = do
  dcerts <- forM consDCert $ \(stakeIndex, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex st
    pure (mkDCert cred)
  mkDCertTx dcerts (Withdrawals mempty) Nothing

mkScriptDCertTx ::
  [(StakeIndex, Bool, StakeCredential StandardCrypto -> ConwayTxCert StandardConway)] ->
  Bool ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkScriptDCertTx consCert isValid' state' = do
  dcerts <- forM consCert $ \(stakeIndex, _, mkDCert) -> do
    cred <- resolveStakeCreds stakeIndex state'
    pure $ mkDCert cred

  pure $
    mkScriptTx isValid' (mapMaybe prepareRedeemer . zip [0 ..] $ consCert) $
      consCertTxBody Nothing dcerts (Withdrawals mempty)
  where
    prepareRedeemer (n, (StakeIndexScript bl, shouldAddRedeemer, _))
      | not shouldAddRedeemer = Nothing
      | bl =
          mkRedeemer n (Examples.alwaysFailsScriptHash, Examples.alwaysFailsScript)
      | otherwise =
          mkRedeemer n (Examples.alwaysSucceedsScriptHash, Examples.alwaysSucceedsScript)
    prepareRedeemer _ = Nothing

    mkRedeemer n (a, b) = Just (RdmrPtr Cert n, (a, b))

mkMultiAssetsScriptTx ::
  [ConwayUTxOIndex] ->
  ConwayUTxOIndex ->
  [(ConwayUTxOIndex, MaryValue StandardCrypto)] ->
  [ConwayUTxOIndex] ->
  MultiAsset StandardCrypto ->
  Bool ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkMultiAssetsScriptTx inputIx colInputIx outputIx refInput minted succeeds fees state' = do
  inputs <- mapM (`resolveUTxOIndex` state') inputIx
  refs <- mapM (`resolveUTxOIndex` state') refInput
  (colInput, _) <- resolveUTxOIndex colInputIx state'
  outputs <- mapM mkOuts outputIx

  let inputs' = Set.fromList $ map (fst . fst) inputs
      refInputs' = Set.fromList $ map (fst . fst) refs
      colInputs' = Set.singleton $ fst colInput

  pure $
    mkScriptTx succeeds (mkScriptInps (map fst inputs) ++ mkScriptMint minted) $
      consTxBody
        inputs'
        colInputs'
        refInputs'
        (StrictSeq.fromList outputs)
        SNothing
        (Coin fees)
        mempty
        mempty -- TODO[sgillespie]: minted?
        (Withdrawals mempty)
  where
    mkOuts (outIx, val) = do
      addr <- resolveAddress outIx state'
      pure $
        BabbageTxOut
          addr
          val
          (Alonzo.DatumHash $ Alonzo.hashData @StandardConway Examples.plutusDataList)
          SNothing

mkDepositTxPools ::
  ConwayUTxOIndex ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkDepositTxPools inputIndex deposit state' = do
  (inputPair, _) <- resolveUTxOIndex inputIndex state'

  let input = Set.singleton (fst inputPair)
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change =
        BabbageTxOut
          addr'
          (valueFromList (fromIntegral $ fromIntegral inputValue - deposit) [])
          Alonzo.NoDatum
          SNothing

  pure $
    mkSimpleTx True $
      consTxBody
        input
        mempty
        mempty
        (StrictSeq.fromList [change])
        SNothing
        (Coin 0)
        mempty
        (allPoolStakeCert' state')
        (Withdrawals mempty)

mkDummyRegisterTx :: Int -> Int -> Either ForgingError (AlonzoTx StandardConway)
mkDummyRegisterTx n m = mkDCertTx consDelegCert (Withdrawals mempty) Nothing
  where
    consDelegCert =
      mkRegTxCert SNothing
        . KeyHashObj
        . KeyHash
        . mkDummyHash (Proxy @(ADDRHASH StandardCrypto))
        . fromIntegral
        <$> [n, m]

mkRegTxCert ::
  StrictMaybe Coin ->
  StakeCredential StandardCrypto ->
  ConwayTxCert StandardConway
mkRegTxCert coin' = mkTxDelegCert $ \cred -> ConwayRegCert cred coin'

mkUnRegTxCert ::
  StrictMaybe Coin ->
  StakeCredential StandardCrypto ->
  ConwayTxCert StandardConway
mkUnRegTxCert coin' = mkTxDelegCert $ \cred -> ConwayUnRegCert cred coin'

mkDelegTxCert ::
  Delegatee StandardCrypto ->
  StakeCredential StandardCrypto ->
  ConwayTxCert StandardConway
mkDelegTxCert delegatee = mkTxDelegCert $ \cred -> ConwayDelegCert cred delegatee

mkTxDelegCert ::
  (StakeCredential StandardCrypto -> ConwayDelegCert StandardCrypto) ->
  StakeCredential StandardCrypto ->
  ConwayTxCert StandardConway
mkTxDelegCert f = ConwayTxCertDeleg . f

mkFullTx ::
  Int ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkFullTx n m state' = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` state') inputs
  let redeemers = mapMaybe Babbage.mkScriptInp $ zip [0 ..] inputPairs
      witnesses =
        mkWitnesses
          redeemers
          [
            ( Alonzo.hashData @StandardConway Examples.plutusDataList
            , Examples.plutusDataList @StandardConway
            )
          ]
  refInputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` state') refInputs
  collateralInput <- Set.singleton . fst . fst <$> resolveUTxOIndex collateralInputs state'

  pure $
    AlonzoTx
      { body =
          txBody
            (mkInputs inputPairs)
            (mkInputs refInputPairs)
            collateralInput
      , wits = witnesses
      , isValid = IsValid True
      , auxiliaryData = SJust auxiliaryData'
      }
  where
    inputs = [UTxOIndex $ n * 3 + 0]
    refInputs = [UTxOIndex $ n * 3 + 1]
    collateralInputs = UTxOIndex $ n * 3 + 2
    txBody inputs' collaterals ref =
      ConwayTxBody
        { ctbSpendInputs = inputs'
        , ctbCollateralInputs = collaterals
        , ctbReferenceInputs = ref
        , ctbOutputs = (`Sized` 0) <$> outputs
        , ctbCollateralReturn = (`Sized` 0) <$> SJust out2
        , ctbTotalCollateral = SNothing
        , ctbCerts = StrictSeq.fromList certs
        , ctbWithdrawals = withdrawals
        , ctbTxfee = Coin m
        , ctbVldt = ValidityInterval SNothing SNothing
        , ctbReqSignerHashes = witKeys
        , ctbMint = minted
        , ctbScriptIntegrityHash = SNothing
        , ctbAdHash = SNothing
        , ctbTxNetworkId = SJust Testnet
        , ctbVotingProcedures = VotingProcedures mempty
        , ctbProposalProcedures = mempty
        , ctbCurrentTreasuryValue = SNothing
        , ctbTreasuryDonation = Coin 0
        }
    -- Outputs
    outputs = StrictSeq.fromList [out0, out1]
    out0, out1, out2 :: BabbageTxOut StandardConway
    out0 =
      BabbageTxOut
        addr0
        outValue0
        (Alonzo.DatumHash (Alonzo.hashData @StandardConway Examples.plutusDataList))
        (SJust Examples.alwaysFailsScript)
    out1 =
      BabbageTxOut
        Examples.alwaysSucceedsScriptAddr
        outValue0
        (Alonzo.DatumHash (Alonzo.hashData @StandardConway Examples.plutusDataList))
        SNothing
    out2 =
      BabbageTxOut
        addr2
        outValue0
        (Alonzo.DatumHash (Alonzo.hashData @StandardConway Examples.plutusDataList))
        (SJust Examples.alwaysFailsScript)
    addr0 =
      Addr
        Testnet
        (Prelude.head unregisteredAddresses)
        (StakeRefBase $ Prelude.head unregisteredStakeCredentials)
    addr2 =
      Addr
        Testnet
        (ScriptHashObj Examples.alwaysFailsScriptHash)
        (StakeRefBase $ unregisteredStakeCredentials !! 2)
    outValue0 =
      MaryValue 20 $ MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]
    policy0 = PolicyID Examples.alwaysMintScriptHash
    policy1 = PolicyID Examples.alwaysSucceedsScriptHash
    assets0 =
      Map.fromList
        [(Prelude.head Examples.assetNames, 5), (Examples.assetNames !! 1, 2)]

    -- Inputs
    mkInputs inputs' = Set.fromList $ fst <$> inputs'

    -- Certificates
    certs =
      [ ConwayTxCertDeleg $ ConwayRegCert (Prelude.head unregisteredStakeCredentials) SNothing
      , ConwayTxCertPool $ Core.RegPool poolParams0
      , ConwayTxCertPool $ Core.RegPool poolParams1
      , ConwayTxCertPool $ Core.RetirePool (Prelude.head unregisteredPools) (EpochNo 0)
      , ConwayTxCertDeleg $ ConwayUnRegCert (unregisteredStakeCredentials !! 2) SNothing
      , ConwayTxCertDeleg $
          ConwayDelegCert
            (unregisteredStakeCredentials !! 1)
            (DelegStake $ unregisteredPools !! 2)
      ]
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

    -- Withdrawals
    withdrawals =
      Withdrawals $
        Map.fromList
          [ (RewardAcnt Testnet (unregisteredStakeCredentials !! 1), Coin 100)
          , (RewardAcnt Testnet (unregisteredStakeCredentials !! 1), Coin 100)
          ]

    -- Witness keys
    witKeys =
      Set.fromList
        [ unregisteredWitnessKey !! 1
        , unregisteredWitnessKey !! 2
        ]

    -- Minted
    minted = MultiAsset $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
    assetsMinted0 =
      Map.fromList [(Prelude.head Examples.assetNames, 10), (Examples.assetNames !! 1, 4)]

    -- Auxiliary data
    auxiliaryData' =
      AlonzoTxAuxData auxiliaryDataMap mempty (Map.singleton PlutusV2 auxiliaryDataScripts)
    auxiliaryDataMap = Map.fromList [(1, List []), (2, List [])]
    auxiliaryDataScripts =
      NonEmpty.fromList [Examples.toBinaryPlutus Examples.alwaysFailsScript]

mkScriptMint ::
  MultiAsset StandardCrypto ->
  [(RdmrPtr, (Core.ScriptHash StandardCrypto, Core.Script StandardConway))]
mkScriptMint (MultiAsset m) =
  mapMaybe mkMint . zip [0 ..] . map policyID $ Map.keys m
  where
    mkMint (n, policyId)
      | policyId == Examples.alwaysFailsScriptHash =
          Just (RdmrPtr Mint n, alwaysFails)
      | policyId == Examples.alwaysSucceedsScriptHash =
          Just (RdmrPtr Mint n, alwaysSucceeds)
      | policyId == Examples.alwaysMintScriptHash =
          Just (RdmrPtr Mint n, alwaysMint)
      | otherwise = Nothing

    alwaysFails = (Examples.alwaysFailsScriptHash, Examples.alwaysFailsScript)
    alwaysSucceeds = (Examples.alwaysFailsScriptHash, Examples.alwaysSucceedsScript)
    alwaysMint = (Examples.alwaysMintScriptHash, Examples.alwaysMintScript)

consPaymentTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (BabbageTxOut StandardConway) ->
  StrictMaybe (BabbageTxOut StandardConway) ->
  Coin ->
  MultiAsset StandardCrypto ->
  ConwayTxBody StandardConway
consPaymentTxBody ins cols ref outs colOut fees minted =
  consTxBody ins cols ref outs colOut fees minted mempty (Withdrawals mempty)

mkUTxOConway ::
  AlonzoTx StandardConway ->
  [(TxIn StandardCrypto, BabbageTxOut StandardConway)]
mkUTxOConway = mkUTxOAlonzo

mkOutFromType ::
  Integer ->
  Babbage.TxOutScriptType ->
  BabbageTxOut StandardConway
mkOutFromType amount txOutType =
  BabbageTxOut outAddress (valueFromList (fromIntegral amount) []) datum script
  where
    outAddress
      | Babbage.scriptSucceeds txOutType = Examples.alwaysSucceedsScriptAddr
      | otherwise = Examples.alwaysFailsScriptAddr
    datum = case Babbage.getDatum txOutType of
      Babbage.NotInlineDatum -> Alonzo.DatumHash dataHash
      Babbage.InlineDatum ->
        Alonzo.Datum (Alonzo.dataToBinaryData Examples.plutusDataList)
      Babbage.InlineDatumCBOR sbs ->
        Alonzo.Datum $ either error identity (Alonzo.makeBinaryData sbs)
    dataHash = Alonzo.hashData @StandardConway Examples.plutusDataList
    script = case Babbage.getInlineScript txOutType of
      SNothing -> SNothing
      SJust True -> SJust Examples.alwaysSucceedsScript
      SJust False -> SJust Examples.alwaysFailsScript

-- | Takes a nested Monad of the form Monad (_, Monad), and combines them into one
-- outer monad
joinSnd :: Monad m => m (a, m b) -> m (a, b)
joinSnd m = do
  (a, m') <- m
  b <- m'
  pure (a, b)

mkScriptInps ::
  [(TxIn StandardCrypto, Core.TxOut StandardConway)] ->
  [(RdmrPtr, (Core.ScriptHash StandardCrypto, Core.Script StandardConway))]
mkScriptInps = mapMaybe joinSnd . map Babbage.mkScriptInp . zip [0 ..]

allPoolStakeCert' :: ConwayLedgerState -> [ConwayTxCert StandardConway]
allPoolStakeCert' st = map (mkRegTxCert SNothing) (getCreds st)
  where
    getCreds = nub . concatMap getPoolStakeCreds . Map.elems . stakePoolParams
    stakePoolParams =
      LedgerState.psStakePoolParams
        . LedgerState.certPState
        . LedgerState.lsCertState
        . LedgerState.esLState
        . LedgerState.nesEs
        . Consensus.shelleyLedgerState
