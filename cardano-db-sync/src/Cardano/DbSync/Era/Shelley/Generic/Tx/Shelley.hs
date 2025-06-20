{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (
  fromShelleyTx,
  getTxCBOR,
  getTxSize,
  mkTxIn,
  fromTxIn,
  mkTxOut,
  mkTxWithdrawals,
  mkTxWithdrawal,
  mkTxCertificates,
  toShelleyCert,
  mkTxCertificate,
  calcWithdrawalSum,
  getTxMetadata,
  mkTxParamProposal,
  txHashId,
  mkTxId,
  txHashFromSafe,
) where

import Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (ScriptType (..))
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Shelley.Generic.Script (fromMultiSig)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Era.Shelley.Generic.Witness
import Cardano.Ledger.BaseTypes (TxIx (..), strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Hashes (SafeHash, ScriptHash (..))
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (ShelleyEra)

fromShelleyTx :: (Word64, Core.Tx ShelleyEra) -> Tx
fromShelleyTx (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txLedgerTxId = mkTxId tx
    , txBlockIndex = blkIndex
    , txCBOR = getTxCBOR tx
    , txSize = getTxSize tx
    , txValidContract = True
    , txInputs = mkTxIn txBody
    , txCollateralInputs = [] -- Shelley does not have collateral inputs
    , txReferenceInputs = [] -- Shelley does not have reference inputs
    , txOutputs = outputs
    , txCollateralOutputs = [] -- Shelley does not have collateral outputs
    , txFees = Just $ txBody ^. Core.feeTxBodyL
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = Nothing
    , txInvalidHereafter = Just $ txBody ^. Shelley.ttlTxBodyL
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromShelleyMetadata <$> getTxMetadata tx
    , txCertificates = mkTxCertificates txBody
    , txWithdrawals = mkTxWithdrawals txBody
    , txParamProposal = mkTxParamProposal (Shelley Standard) txBody
    , txMint = mempty -- Shelley does not support multi-assets
    , txRedeemer = [] -- Shelley does not support redeemers
    , txData = []
    , txScriptSizes = [] -- Shelley does not support plutus scripts
    , txScripts = scripts
    , txExtraKeyWitnesses = []
    , txVotingProcedure = []
    , txProposalProcedure = []
    , txTreasuryDonation = mempty -- Shelley does not support treasury donations
    }
  where
    txBody :: Core.TxBody ShelleyEra
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = mkTxOut txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript <$> Map.toList (tx ^. Core.witsTxL . Core.scriptTxWitsL)

    mkTxScript :: (ScriptHash, MultiSig ShelleyEra) -> TxScript
    mkTxScript (hsh, script) =
      TxScript
        { txScriptHash = unScriptHash hsh
        , txScriptType = MultiSig
        , txScriptPlutusSize = Nothing
        , txScriptJson = Just . LBS.toStrict . Aeson.encode $ fromMultiSig script
        , txScriptCBOR = Nothing
        }

mkTxOut ::
  forall era.
  (Core.EraTxBody era, Core.Value era ~ Coin) =>
  Core.TxBody era ->
  [TxOut]
mkTxOut txBody = zipWith fromTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)
  where
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = txOut ^. Core.addrTxOutL
        , txOutAdaValue = txOut ^. Core.valueTxOutL
        , txOutMaValue = mempty -- Shelley does not support multi-assets
        , txOutScript = Nothing
        , txOutDatum = NoDatum -- Shelley does not support plutus data
        }

fromTxIn :: Ledger.TxIn -> TxIn
fromTxIn (Ledger.TxIn (Ledger.TxId txid) (TxIx w64)) =
  TxIn
    { txInIndex = fromIntegral w64
    , txInRedeemerIndex = Nothing
    , txInTxId = Ledger.TxId txid
    }

txHashId :: Core.EraTx era => Core.Tx era -> ByteString
txHashId = safeHashToByteString . txSafeHash

txSafeHash :: Core.EraTx era => Core.Tx era -> SafeHash Shelley.EraIndependentTxBody
txSafeHash tx = Core.hashAnnotated (tx ^. Core.bodyTxL)

mkTxId :: Core.EraTx era => Core.Tx era -> Ledger.TxId
mkTxId = Ledger.TxId . txSafeHash

txHashFromSafe :: SafeHash Core.EraIndependentTxBody -> ByteString
txHashFromSafe = Crypto.hashToBytes . Core.extractHash

getTxSize :: Core.EraTx era => Core.Tx era -> Word64
getTxSize tx = fromIntegral $ tx ^. Core.sizeTxF

getTxCBOR :: Core.EraTx era => Core.Tx era -> ByteString
getTxCBOR = serialize'

mkTxIn ::
  Core.EraTxBody era =>
  Core.TxBody era ->
  [TxIn]
mkTxIn txBody = map fromTxIn $ toList $ txBody ^. Core.inputsTxBodyL

calcWithdrawalSum ::
  Core.EraTxBody era =>
  Core.TxBody era ->
  Coin
calcWithdrawalSum bd =
  Coin $ sum $ map unCoin $ Map.elems $ Shelley.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL

getTxMetadata :: Core.EraTx era => Core.Tx era -> Maybe (Core.TxAuxData era)
getTxMetadata tx = strictMaybeToMaybe (tx ^. Core.auxDataTxL)

mkTxWithdrawals ::
  Shelley.ShelleyEraTxBody era =>
  Core.TxBody era ->
  [TxWithdrawal]
mkTxWithdrawals bd =
  map mkTxWithdrawal $ Map.toList $ Shelley.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL

mkTxWithdrawal :: (Shelley.RewardAccount, Coin) -> TxWithdrawal
mkTxWithdrawal (ra, c) =
  TxWithdrawal
    { txwRedeemerIndex = Nothing
    , txwRewardAccount = ra
    , txwAmount = c
    }

mkTxParamProposal ::
  Shelley.ShelleyEraTxBody era =>
  Witness era ->
  Core.TxBody era ->
  [ParamProposal]
mkTxParamProposal witness txBody =
  maybe [] (convertParamProposal witness) $ strictMaybeToMaybe (txBody ^. Shelley.updateTxBodyL)

mkTxCertificates ::
  forall era.
  ( Shelley.ShelleyEraTxBody era
  , TxCert era ~ ShelleyTxCert era
  ) =>
  Core.TxBody era ->
  [TxCertificate]
mkTxCertificates bd =
  zipWith mkTxCertificate [0 ..] $ toShelleyCert <$> toList (bd ^. Core.certsTxBodyL)

toShelleyCert :: ShelleyTxCert era -> ShelleyCert
toShelleyCert cert = case cert of
  ShelleyTxCertDelegCert a -> ShelleyTxCertDelegCert a
  ShelleyTxCertPool a -> ShelleyTxCertPool a
  ShelleyTxCertGenesisDeleg a -> ShelleyTxCertGenesisDeleg a
  ShelleyTxCertMir a -> ShelleyTxCertMir a

mkTxCertificate :: Word16 -> ShelleyCert -> TxCertificate
mkTxCertificate idx dcert =
  TxCertificate
    { txcRedeemerIndex = Nothing
    , txcIndex = idx
    , txcCert = Left dcert
    }
