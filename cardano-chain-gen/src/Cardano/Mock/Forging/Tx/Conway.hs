{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Tx.Conway (
  consTxBody,
  mkPaymentTx,
  mkSimpleTx,
  mkFullTx,
  consPoolParams,
  mkScriptInput,
  mkWitnesses,
  mkUTxO,
  mkTxHash,
) where

import Cardano.Ledger.Address (Addr (..), RewardAcnt (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data (), DataHash (), Datum (..), hashData)
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), RdmrPtr (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), Network (..), TxIx (..), textToUrl)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxOut (BabbageTxOut (..), referenceScriptTxOutL)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential (), StakeReference (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), hashVerKeyVRF)
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), valueFromList)
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Ledger.Shelley.TxBody (PoolMetadata (..), PoolParams (..), StakePoolRelay (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Mock.Forging.Crypto (RawSeed (..), mkVRFKeyPair)
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import Data.Sequence.Strict (StrictSeq ())
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (LedgerState ())
import Ouroboros.Consensus.Shelley.Eras (StandardConway (), StandardCrypto ())
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Prelude ((!!))
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

mkPaymentTx ::
  ConwayUTxOIndex ->
  ConwayUTxOIndex ->
  Integer ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkPaymentTx inputIndex outputIndex amount fees state' = do
  (inputPair, _) <- resolveUTxOIndex inputIndex state'
  addr <- resolveAddress outputIndex state'

  let input = Set.singleton $ fst inputPair
      output = BabbageTxOut addr (valueFromList (fromIntegral amount) []) NoDatum SNothing
      BabbageTxOut addr' (MaryValue inputValue _) _ _ = snd inputPair
      change = BabbageTxOut addr' (valueFromList (fromIntegral $ fromInteger inputValue - amount - fees) []) NoDatum SNothing

  pure $
    mkSimpleTx True $
      consPaymentTxBody
        input
        mempty
        mempty
        (StrictSeq.fromList [output, change])
        SNothing
        (Coin fees)
        mempty

mkSimpleTx :: Bool -> ConwayTxBody StandardConway -> AlonzoTx StandardConway
mkSimpleTx isValid' txBody =
  AlonzoTx
    { body = txBody
    , wits = mempty
    , isValid = IsValid isValid'
    , auxiliaryData = maybeToStrictMaybe Nothing
    }

mkFullTx ::
  Int ->
  Integer ->
  ConwayLedgerState ->
  Either ForgingError (AlonzoTx StandardConway)
mkFullTx n m state' = do
  inputPairs <- fmap fst <$> mapM (`resolveUTxOIndex` state') inputs
  let redeemers = mapMaybe mkScriptInput $ zip [0 ..] inputPairs
      witnesses =
        mkWitnesses
          redeemers
          [(hashData @StandardConway plutusDataList, plutusDataList @StandardConway)]
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
      MaryValue 20 $ MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]
    policy0 = PolicyID alwaysMintScriptHash
    policy1 = PolicyID alwaysSucceedsScriptHash
    assets0 = Map.fromList [(Prelude.head assetNames, 5), (assetNames !! 1, 2)]

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
    assetsMinted0 = Map.fromList [(Prelude.head assetNames, 10), (assetNames !! 1, 4)]

    -- Auxiliary data
    auxiliaryData' = AlonzoTxAuxData auxiliaryDataMap mempty (Map.singleton PlutusV2 auxiliaryDataScripts)
    auxiliaryDataMap = Map.fromList [(1, List []), (2, List [])]
    auxiliaryDataScripts = NonEmpty.fromList [toBinaryPlutus alwaysFailsScript]

-- TODO[sgillespie]: This is the same as Babbage.consPoolParams
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

-- TODO[sgillespie]: This is the same as Babbage.consPoolParams
mkScriptInput ::
  (Word64, (TxIn StandardCrypto, Core.TxOut StandardConway)) ->
  Maybe (RdmrPtr, Maybe (Core.ScriptHash StandardCrypto, Core.Script StandardConway))
mkScriptInput (n, (_txIn, txOut)) =
  case mscr of
    SNothing
      | addr == alwaysFailsScriptAddr ->
          Just (RdmrPtr Spend n, Just (alwaysFailsScriptHash, alwaysFailsScript))
      | addr == alwaysSucceedsScriptAddr ->
          Just (RdmrPtr Spend n, Just (alwaysSucceedsScriptHash, alwaysSucceedsScript))
      | addr == alwaysMintScriptAddr ->
          Just (RdmrPtr Spend n, Just (alwaysMintScriptHash, alwaysMintScript))
    SJust _ ->
      Just (RdmrPtr Spend n, Nothing)
    _ -> Nothing
  where
    addr = txOut ^. Core.addrTxOutL
    mscr = txOut ^. referenceScriptTxOutL

mkWitnesses ::
  [(RdmrPtr, Maybe (Core.ScriptHash StandardCrypto, Core.Script StandardConway))] ->
  [(DataHash StandardCrypto, Data StandardConway)] ->
  AlonzoTxWits StandardConway
mkWitnesses rdmrs datas =
  AlonzoTxWits
    { txwitsVKey = mempty
    , txwitsBoot = mempty
    , txscripts = Map.fromList $ mapMaybe snd rdmrs
    , txdats = TxDats $ Map.fromList datas
    , txrdmrs = Redeemers $ Map.fromList redeemers
    }
  where
    redeemers =
      fmap
        (,(plutusDataList, ExUnits 100 100))
        (fst <$> rdmrs)

-- TODO[sgillespe]: No difference here either
mkUTxO :: AlonzoTx StandardConway -> [(TxIn StandardCrypto, Core.TxOut StandardConway)]
mkUTxO tx =
  [ (TxIn (mkTxHash tx) idx, out)
  | (out, idx) <- zip (toList (tx ^. outputsL)) (TxIx <$> [0 ..])
  ]
  where
    outputsL = Core.bodyTxL . Core.outputsTxBodyL

-- TODO[sgillespie]: And this one
mkTxHash :: AlonzoTx StandardConway -> TxId StandardCrypto
mkTxHash = txid . getField @"body"

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
