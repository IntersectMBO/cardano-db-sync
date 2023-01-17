{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (
  fromShelleyTx,
  getTxSize,
  mkTxIn,
  fromTxIn,
  mkTxOut,
  mkTxWithdrawals,
  mkTxWithdrawal,
  mkTxCertificates,
  mkTxCertificate,
  calcWithdrawalSum,
  getTxMetadata,
  mkTxParamProposal,
  txHashId,
) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (ScriptType (..))
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Era.Shelley.Generic.Witness
import Cardano.Ledger.BaseTypes (TxIx (..), strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.SafeHash as Ledger
import Cardano.Ledger.Shelley.Scripts (MultiSig, ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardShelley)

fromShelleyTx :: (Word64, Core.Tx StandardShelley) -> Tx
fromShelleyTx (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txBlockIndex = blkIndex
    , txSize = getTxSize tx
    , txValidContract = True
    , txInputs = mkTxIn txBody
    , txCollateralInputs = [] -- Shelley does not have collateral inputs
    , txReferenceInputs = [] -- Shelley does not have reference inputs
    , txOutputs = outputs
    , txCollateralOutputs = [] -- Shelley does not have collateral outputs
    , txFees = Just $ Shelley._txfee txBody
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = Nothing
    , txInvalidHereafter = Just $ Shelley._ttl txBody
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
    }
  where
    txBody :: Core.TxBody StandardShelley
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = mkTxOut txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript <$> Map.toList (ShelleyTx.txwitsScript tx)

    mkTxScript :: (ScriptHash StandardCrypto, MultiSig StandardCrypto) -> TxScript
    mkTxScript (hsh, script) =
      TxScript
        { txScriptHash = unScriptHash hsh
        , txScriptType = MultiSig
        , txScriptPlutusSize = Nothing
        , txScriptJson = Just . LBS.toStrict . Aeson.encode $ Api.fromShelleyMultiSig script
        , txScriptCBOR = Nothing
        }

mkTxOut ::
  forall era.
  (Core.EraTxBody era, Core.Value era ~ Coin, Crypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  [TxOut]
mkTxOut txBody = zipWith fromTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)
  where
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = txOut ^. Core.addrTxOutL
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = txOut ^. Core.valueTxOutL
        , txOutMaValue = mempty -- Shelley does not support multi-assets
        , txOutScript = Nothing
        , txOutDatum = NoDatum -- Shelley does not support plutus data
        }
      where
        Ledger.UnsafeCompactAddr bs = txOut ^. Core.compactAddrTxOutL

fromTxIn :: ShelleyTx.TxIn StandardCrypto -> TxIn
fromTxIn (ShelleyTx.TxIn (ShelleyTx.TxId txid) (TxIx w64)) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = w64
    , txInRedeemerIndex = Nothing
    }

txHashId :: (Crypto era ~ StandardCrypto, Core.EraTx era) => Core.Tx era -> ByteString
txHashId tx = Crypto.hashToBytes $ Ledger.extractHash $ Ledger.hashAnnotated (tx ^. Core.bodyTxL)

getTxSize :: Core.EraTx era => Core.Tx era -> Word64
getTxSize tx = fromIntegral $ tx ^. Core.sizeTxF

mkTxIn ::
  (Core.EraTxBody era, Crypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  [TxIn]
mkTxIn txBody = map fromTxIn $ toList $ txBody ^. Core.inputsTxBodyL

calcWithdrawalSum ::
  (Shelley.ShelleyEraTxBody era, Crypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  Coin
calcWithdrawalSum bd =
  Coin $ sum $ map unCoin $ Map.elems $ Shelley.unWdrl $ bd ^. Shelley.wdrlsTxBodyL

getTxMetadata :: Core.EraTx era => Core.Tx era -> Maybe (Core.AuxiliaryData era)
getTxMetadata tx = strictMaybeToMaybe (tx ^. Core.auxDataTxL)

mkTxWithdrawals ::
  (Shelley.ShelleyEraTxBody era, Crypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  [TxWithdrawal]
mkTxWithdrawals bd =
  map mkTxWithdrawal $ Map.toList $ Shelley.unWdrl $ bd ^. Shelley.wdrlsTxBodyL

mkTxWithdrawal :: (Shelley.RewardAcnt StandardCrypto, Coin) -> TxWithdrawal
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
  (Shelley.ShelleyEraTxBody era, Crypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  [TxCertificate]
mkTxCertificates bd =
  zipWith mkTxCertificate [0 ..] $ toList $ bd ^. Shelley.certsTxBodyL

mkTxCertificate :: Word16 -> Shelley.DCert StandardCrypto -> TxCertificate
mkTxCertificate idx dcert =
  TxCertificate
    { txcRedeemerIndex = Nothing
    , txcIndex = idx
    , txcCert = dcert
    }
