{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Cardano.Mock.Forging.Tx.Conway (
  ConwayLedgerState,
  Babbage.TxOutScriptType (..),
  Babbage.DatumType (..),
  Babbage.ReferenceScript (..),
  consTxBody,
  consCertTxBody,
  consPoolParams,
  consTxCertPool,
  mkPaymentTx,
  mkPaymentTx',
  mkDonationTx,
  mkLockByScriptTx,
  mkUnlockScriptTx,
  mkUnlockScriptTxBabbage,
  mkScriptTx,
  mkSimpleTx,
  mkAuxDataTx,
  mkDCertTx,
  mkDCertPoolTx,
  mkDCertTxPools,
  mkSimpleDCertTx,
  mkScriptDCertTx,
  mkMultiAssetsScriptTx,
  mkDepositTxPools,
  mkRegisterDRepTx,
  mkCommitteeAuthTx,
  mkNewConstitutionTx,
  mkDummyRegisterTx,
  mkDummyTxBody,
  mkTxDelegCert,
  mkRegTxCert,
  mkUnRegTxCert,
  mkDelegTxCert,
  mkRegDelegTxCert,
  mkAddCommitteeTx,
  mkTreasuryWithdrawalTx,
  mkParamChangeTx,
  mkHardForkInitTx,
  mkInfoTx,
  mkGovActionProposalTx,
  mkGovVoteTx,
  mkGovVoteYesTx,
  Babbage.mkParamUpdateTx,
  mkFullTx,
  mkScriptMint',
  Babbage.mkScriptInp,
  mkWitnesses,
  mkUTxOConway,
  mkUTxOCollConway,
  addValidityInterval,
) where

import Cardano.Ledger.Address (Addr (..), RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), mkAlonzoTxAuxData)
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut, BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Conway.Scripts
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxCert hiding (mkDelegTxCert)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential, StakeReference (..))
import Cardano.Ledger.Crypto (ADDRHASH ())
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), valueFromList)
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.Language (Language (..))
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..), coin)
import Cardano.Mock.Forging.Tx.Alonzo (
  addValidityInterval,
  mkScriptMint,
  mkScriptTx,
  mkUTxOAlonzo,
  mkWitnesses,
 )
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude
import Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq ())
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (EraCrypto, LedgerState ())
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
  Coin ->
  ConwayTxBody StandardConway
consTxBody ins cols ref outs colOut fees minted certs withdrawals donation =
  ConwayTxBody
    { ctbSpendInputs = ins
    , ctbCollateralInputs = cols
    , ctbReferenceInputs = ref
    , ctbOutputs = (`Sized` 0) <$> outs
    , ctbCollateralReturn = (`Sized` 0) <$> colOut
    , ctbTotalCollateral = SNothing
    , ctbCerts = OSet.fromList certs
    , ctbWithdrawals = withdrawals
    , ctbTxfee = fees
    , ctbVldt = ValidityInterval SNothing SNothing
    , ctbReqSignerHashes = mempty
    , ctbMint = minted
    , ctbScriptIntegrityHash = SNothing
    , ctbAdHash = SNothing
    , ctbTxNetworkId = SJust Testnet
    , ctbVotingProcedures = Governance.VotingProcedures mempty
    , ctbProposalProcedures = mempty
    , ctbCurrentTreasuryValue = SNothing
    , ctbTreasuryDonation = donation
    }

consCertTxBody ::
  Maybe (TxIn StandardCrypto) ->
  [ConwayTxCert StandardConway] ->
  Withdrawals StandardCrypto ->
  ConwayTxBody StandardConway
consCertTxBody ref certs withdrawals =
  consTxBody
    mempty
    mempty
    (toSet ref)
    mempty
    SNothing
    (Coin 0)
    mempty
    certs
    withdrawals
    (Coin 0)
  where
    toSet Nothing = mempty
    toSet (Just a) = Set.singleton a

consTxCertPool ::
  [StakeCredential StandardCrypto] ->
  KeyHash 'StakePool StandardCrypto ->
  ConwayTxCert StandardConway
consTxCertPool [] _ = panic "Expected at least 1 pool owner"
consTxCertPool (rwCred : poolOwners) poolId =
  ConwayTxCertPool
    . Core.RegPool
    . consPoolParams poolId rwCred
    . map unKeyHashObj
    $ poolOwners
  where
    unKeyHashObj (KeyHashObj owner) = owner
    unKeyHashObj _ = panic "Expected a KeyHashObj"

mkPaymentTx ::
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  Integer ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkPaymentTx inputIndex outputIndex amount =
  mkPaymentTx' inputIndex outputIndices
  where
    outputIndices = [(outputIndex, valueFromList (Coin amount) [])]

mkPaymentTx' ::
  ConwayUTxOIndex ->
  [(ConwayUTxOIndex, MaryValue StandardCrypto)] ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkPaymentTx' inputIndex outputIndices fees donation state' = do
  (inputPair, _) <- resolveUTxOIndex inputIndex state'
  outputs <- mapM mkOutputs outputIndices

  let inputs = Set.singleton (fst inputPair)
      outValue = sum $ map (unCoin . coin . snd) outputIndices
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change =
        BabbageTxOut
          addr'
          (valueFromList (Coin $ unCoin inputValue - outValue - fees) [])
          NoDatum
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
        (Coin donation)
  where
    mkOutputs (outIx, val) = do
      addr <- resolveAddress outIx state'
      pure (BabbageTxOut addr val NoDatum SNothing)

mkDonationTx :: Coin -> AlonzoTx StandardConway
mkDonationTx amount = mkSimpleTx True txBody
  where
    txBody = mkDummyTxBody {ctbTreasuryDonation = amount}

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
          (valueFromList (Coin $ unCoin inputValue - amount - fees) [])
          NoDatum
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
        (Coin 0)

mkUnlockScriptTx ::
  [ConwayUTxOIndex] ->
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  Bool ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkUnlockScriptTx inputIndex colInputIndex outputIndex =
  mkUnlockScriptTx' inputIndex colInputIndex outputIndex mempty Nothing

mkUnlockScriptTxBabbage ::
  [ConwayUTxOIndex] ->
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  [ConwayUTxOIndex] ->
  Bool ->
  Bool ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkUnlockScriptTxBabbage inputIndex colInputIndex outputIndex refInput compl succeeds amount fees state' = do
  let colTxOutType =
        if compl
          then Just $ Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)
          else Just $ Babbage.TxOutNoInline True
      colOutput = mkOutFromType amount <$> colTxOutType

  mkUnlockScriptTx'
    inputIndex
    colInputIndex
    outputIndex
    refInput
    colOutput
    succeeds
    amount
    fees
    state'

mkDCertTx ::
  [ConwayTxCert StandardConway] ->
  Withdrawals StandardCrypto ->
  Maybe (TxIn StandardCrypto) ->
  Either ForgingError (AlonzoTx StandardConway)
mkDCertTx certs wdrl ref = Right (mkSimpleTx True $ consCertTxBody ref certs wdrl)

mkDCertPoolTx ::
  [ ( [StakeIndex]
    , PoolIndex
    , [StakeCredential StandardCrypto] ->
      KeyHash 'StakePool StandardCrypto ->
      ConwayTxCert StandardConway
    )
  ] ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkDCertPoolTx consDCert state' = do
  dcerts <- forM consDCert $ \(stakeIxs, poolIx, mkDCert) -> do
    stakeCreds <- forM stakeIxs $ \stakeIx -> resolveStakeCreds stakeIx state'
    let poolId = resolvePool poolIx state'
    pure $ mkDCert stakeCreds poolId

  mkDCertTx dcerts (Withdrawals mempty) Nothing

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

mkAuxDataTx ::
  Bool ->
  ConwayTxBody StandardConway ->
  Map Word64 Metadatum ->
  AlonzoTx StandardConway
mkAuxDataTx isValid' txBody auxData =
  AlonzoTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid isValid'
    , auxiliaryData = SJust (mkAlonzoTxAuxData auxData [])
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
          mkRedeemer n (alwaysFailsScriptHash, alwaysFailsScript)
      | otherwise =
          mkRedeemer n (alwaysSucceedsScriptHash, alwaysSucceedsScript)
    prepareRedeemer _ = Nothing

    mkRedeemer n (a, b) = Just (ConwayCertifying (AsIx n), Just (a, b))

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
    mkScriptTx succeeds (mkScriptInps (map fst inputs) ++ mkScriptMint' minted) $
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
        (Coin 0)
  where
    mkOuts (outIx, val) = do
      addr <- resolveAddress outIx state'
      pure $
        BabbageTxOut
          addr
          val
          (DatumHash $ hashData @StandardConway plutusDataList)
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
          (valueFromList (Coin $ unCoin inputValue - deposit) [])
          NoDatum
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
        (Coin 0)

mkRegisterDRepTx ::
  Credential 'DRepRole StandardCrypto ->
  Either ForgingError (AlonzoTx StandardConway)
mkRegisterDRepTx cred = mkDCertTx [cert] (Withdrawals mempty) Nothing
  where
    cert = ConwayTxCertGov (ConwayRegDRep cred deposit SNothing)
    deposit = Coin 500_000_000

mkCommitteeAuthTx ::
  Credential 'ColdCommitteeRole StandardCrypto ->
  Credential 'HotCommitteeRole StandardCrypto ->
  Either ForgingError (AlonzoTx StandardConway)
mkCommitteeAuthTx cold hot = mkDCertTx [cert] (Withdrawals mempty) Nothing
  where
    cert = ConwayTxCertGov (ConwayAuthCommitteeHotKey cold hot)

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

mkRegDelegTxCert ::
  Coin ->
  Delegatee StandardCrypto ->
  StakeCredential StandardCrypto ->
  ConwayTxCert StandardConway
mkRegDelegTxCert deposit delegatee =
  mkTxDelegCert $ \cred -> ConwayRegDelegCert cred delegatee deposit

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

mkAddCommitteeTx ::
  Credential 'ColdCommitteeRole StandardCrypto ->
  AlonzoTx StandardConway
mkAddCommitteeTx cred = mkGovActionProposalTx govAction
  where
    govAction = Governance.UpdateCommittee SNothing mempty newMembers threshold
    newMembers = Map.singleton cred (EpochNo 20)
    threshold = fromJust $ boundRational (1 % 1)

mkNewConstitutionTx ::
  Anchor StandardCrypto ->
  AlonzoTx StandardConway
mkNewConstitutionTx anchor = mkGovActionProposalTx govAction
  where
    govAction = Governance.NewConstitution SNothing constitution
    constitution = Governance.Constitution anchor SNothing

mkTreasuryWithdrawalTx ::
  RewardAccount StandardCrypto ->
  Coin ->
  AlonzoTx StandardConway
mkTreasuryWithdrawalTx rewardAccount amount = mkGovActionProposalTx govAction
  where
    govAction = Governance.TreasuryWithdrawals withdrawals hashProtection
    withdrawals = Map.singleton rewardAccount amount
    hashProtection = SNothing

mkParamChangeTx :: AlonzoTx StandardConway
mkParamChangeTx = mkGovActionProposalTx govAction
  where
    govAction = Governance.ParameterChange SNothing paramUpdate hasProtection
    paramUpdate = Core.emptyPParamsUpdate & Core.ppuMaxTxSizeL .~ SJust 32_000
    hasProtection = SNothing

mkHardForkInitTx :: AlonzoTx StandardConway
mkHardForkInitTx = mkGovActionProposalTx govAction
  where
    govAction = Governance.HardForkInitiation SNothing protoVersion
    protoVersion = ProtVer (natVersion @11) 0

mkInfoTx :: AlonzoTx StandardConway
mkInfoTx = mkGovActionProposalTx govAction
  where
    govAction = Governance.InfoAction

mkGovActionProposalTx ::
  Governance.GovAction StandardConway ->
  AlonzoTx StandardConway
mkGovActionProposalTx govAction = mkSimpleTx True txBody
  where
    txBody = mkDummyTxBody {ctbProposalProcedures = OSet.singleton proposal}

    proposal =
      Governance.ProposalProcedure
        { Governance.pProcDeposit = Coin 50_000_000_000
        , Governance.pProcReturnAddr =
            RewardAccount Testnet (Prelude.head unregisteredStakeCredentials)
        , Governance.pProcGovAction = govAction
        , Governance.pProcAnchor = anchor
        }

    anchor =
      Governance.Anchor
        { Governance.anchorUrl = fromJust (textToUrl 64 "best.cc")
        , Governance.anchorDataHash = hashAnchorData (Governance.AnchorData mempty)
        }

mkGovVoteYesTx ::
  Governance.GovActionId StandardCrypto ->
  [Governance.Voter StandardCrypto] ->
  AlonzoTx StandardConway
mkGovVoteYesTx govAction =
  mkGovVoteTx govAction . Map.fromList . map (,Governance.VoteYes)

mkGovVoteTx ::
  Governance.GovActionId StandardCrypto ->
  Map (Governance.Voter StandardCrypto) Governance.Vote ->
  AlonzoTx StandardConway
mkGovVoteTx govAction votes = mkSimpleTx True txBody
  where
    txBody = mkDummyTxBody {ctbVotingProcedures = Governance.VotingProcedures votes'}
    votes' = fmap mkGovActionVote votes
    mkGovActionVote = Map.singleton govAction . mkVote
    mkVote v =
      Governance.VotingProcedure
        { Governance.vProcVote = v
        , Governance.vProcAnchor = SNothing
        }

mkDummyTxBody :: ConwayTxBody StandardConway
mkDummyTxBody =
  consTxBody
    mempty
    mempty
    mempty
    mempty
    SNothing
    mempty
    mempty
    mempty
    (Withdrawals mempty)
    mempty

mkFullTx ::
  Int ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkFullTx n m state' = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` state') inputs
  let redeemers = mkScriptInps inputPairs
      witnesses =
        mkWitnesses
          redeemers
          [
            ( hashData @StandardConway plutusDataList
            , plutusDataList @StandardConway
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
        , ctbCerts = OSet.fromList certs
        , ctbWithdrawals = withdrawals
        , ctbTxfee = Coin m
        , ctbVldt = ValidityInterval SNothing SNothing
        , ctbReqSignerHashes = witKeys
        , ctbMint = minted
        , ctbScriptIntegrityHash = SNothing
        , ctbAdHash = SNothing
        , ctbTxNetworkId = SJust Testnet
        , ctbVotingProcedures = Governance.VotingProcedures mempty
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
        (DatumHash (hashData @StandardConway plutusDataList))
        (SJust alwaysFailsScript)
    out1 =
      BabbageTxOut
        alwaysSucceedsScriptAddr
        outValue0
        (DatumHash (hashData @StandardConway plutusDataList))
        SNothing
    out2 =
      BabbageTxOut
        addr2
        outValue0
        (DatumHash (hashData @StandardConway plutusDataList))
        (SJust alwaysFailsScript)
    addr0 =
      Addr
        Testnet
        (Prelude.head unregisteredAddresses)
        (StakeRefBase $ Prelude.head unregisteredStakeCredentials)
    addr2 =
      Addr
        Testnet
        (ScriptHashObj alwaysFailsScriptHash)
        (StakeRefBase $ unregisteredStakeCredentials !! 2)
    outValue0 =
      MaryValue (Coin 20) $ MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]
    policy0 = PolicyID alwaysMintScriptHash
    policy1 = PolicyID alwaysSucceedsScriptHash
    assets0 = Map.fromList [(Prelude.head assetNames, 5), (assetNames !! 1, 2)]

    -- Inputs
    mkInputs inputs' = Set.fromList $ fst <$> inputs'

    -- Certificates
    certs :: [ConwayTxCert StandardConway]
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
          [ (RewardAccount Testnet (unregisteredStakeCredentials !! 1), Coin 100)
          , (RewardAccount Testnet (unregisteredStakeCredentials !! 1), Coin 100)
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
      Map.fromList [(Prelude.head assetNames, 10), (assetNames !! 1, 4)]

    -- Auxiliary data
    auxiliaryData' =
      AlonzoTxAuxData auxiliaryDataMap mempty (Map.singleton PlutusV2 auxiliaryDataScripts)
    auxiliaryDataMap = Map.fromList [(1, List []), (2, List [])]
    auxiliaryDataScripts =
      NonEmpty.fromList [alwaysFailsPlutusBinary]

mkScriptMint' ::
  MultiAsset StandardCrypto ->
  [(ConwayPlutusPurpose AsIx era, Maybe (Core.ScriptHash StandardCrypto, Core.Script StandardConway))]
mkScriptMint' = fmap (first (ConwayMinting . AsIx)) . mkScriptMint

{-}
mkScriptMint ::
  MultiAsset StandardCrypto ->
  [(AlonzoPlutusPurpose AsIx era, Maybe (Core.ScriptHash StandardCrypto, Core.Script StandardConway))]
mkScriptMint (MultiAsset m) =
  mapMaybe mkMint . zip [0 ..] . map policyID $ Map.keys m
  where
    mkMint (n, policyId)
      | policyId == alwaysFailsScriptHash =
          Just (RdmrPtr Mint n, Just alwaysFails)
      | policyId == alwaysSucceedsScriptHash =
          Just (RdmrPtr Mint n, Just alwaysSucceeds)
      | policyId == alwaysMintScriptHash =
          Just (RdmrPtr Mint n, Just alwaysMint)
      | otherwise = Nothing

    alwaysFails = (alwaysFailsScriptHash, alwaysFailsScript)
    alwaysSucceeds = (alwaysFailsScriptHash, alwaysSucceedsScript)
    alwaysMint = (alwaysMintScriptHash, alwaysMintScript)
-}
consPaymentTxBody ::
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  Set (TxIn StandardCrypto) ->
  StrictSeq (BabbageTxOut StandardConway) ->
  StrictMaybe (BabbageTxOut StandardConway) ->
  Coin ->
  MultiAsset StandardCrypto ->
  Coin ->
  ConwayTxBody StandardConway
consPaymentTxBody ins cols ref outs colOut fees minted =
  consTxBody ins cols ref outs colOut fees minted mempty (Withdrawals mempty)

mkUTxOConway ::
  AlonzoTx StandardConway ->
  [(TxIn StandardCrypto, BabbageTxOut StandardConway)]
mkUTxOConway = mkUTxOAlonzo

mkUTxOCollConway ::
  AlonzoTx StandardConway ->
  [(TxIn StandardCrypto, BabbageTxOut StandardConway)]
mkUTxOCollConway = Babbage.mkUTxOCollBabbage

mkOutFromType ::
  Integer ->
  Babbage.TxOutScriptType ->
  BabbageTxOut StandardConway
mkOutFromType amount txOutType =
  BabbageTxOut outAddress (valueFromList (Coin amount) []) datum script
  where
    outAddress
      | Babbage.scriptSucceeds txOutType = alwaysSucceedsScriptAddr
      | otherwise = alwaysFailsScriptAddr
    datum = case Babbage.getDatum txOutType of
      Babbage.NotInlineDatum -> DatumHash dataHash
      Babbage.InlineDatum ->
        Datum (dataToBinaryData plutusDataList)
      Babbage.InlineDatumCBOR sbs ->
        Datum $ either error identity (makeBinaryData sbs)
    dataHash = hashData @StandardConway plutusDataList
    script = case Babbage.getInlineScript txOutType of
      SNothing -> SNothing
      SJust True -> SJust alwaysSucceedsScript
      SJust False -> SJust alwaysFailsScript

mkScriptInps ::
  [(TxIn StandardCrypto, Core.TxOut StandardConway)] ->
  [(ConwayPlutusPurpose AsIx StandardConway, Maybe (Core.ScriptHash StandardCrypto, AlonzoScript StandardConway))]
mkScriptInps = mapMaybe mkScriptInp . zip [0 ..]

mkScriptInp ::
  (BabbageEraTxOut era, EraCrypto era ~ StandardCrypto) =>
  (Word64, (TxIn StandardCrypto, Core.TxOut era)) ->
  Maybe (ConwayPlutusPurpose AsIx era, Maybe (Core.ScriptHash StandardCrypto, AlonzoScript era))
mkScriptInp = fmap (first $ ConwaySpending . AsIx) . Babbage.mkScriptInp

mkUnlockScriptTx' ::
  [ConwayUTxOIndex] ->
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  [ConwayUTxOIndex] ->
  Maybe (BabbageTxOut StandardConway) ->
  Bool ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkUnlockScriptTx' inputIndex colInputIndex outputIndex refInput colOut succeeds amount fees state' = do
  inputPairs <- map fst <$> mapM (`resolveUTxOIndex` state') inputIndex
  refInputPairs <- map fst <$> mapM (`resolveUTxOIndex` state') refInput
  (colInputPair, _) <- resolveUTxOIndex colInputIndex state'
  addr <- resolveAddress outputIndex state'

  let inputs = Set.fromList $ map fst inputPairs
      colInputs = Set.singleton $ fst colInputPair
      refInputs = Set.fromList $ map fst refInputPairs
      output =
        BabbageTxOut
          addr
          (valueFromList (Coin amount) [])
          NoDatum
          SNothing

  pure $
    mkScriptTx succeeds (mkScriptInps inputPairs) $
      consPaymentTxBody
        inputs
        colInputs
        refInputs
        (StrictSeq.singleton output)
        (maybeToStrictMaybe colOut)
        (Coin fees)
        mempty
        (Coin 0)

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
