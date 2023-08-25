{-# LANGUAGE CPP #-}
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
  toShelleyCert,
  mkTxCertificate,
  calcWithdrawalSum,
  getTxMetadata,
  mkTxParamProposal,
  txHashId,
  txHashFromSafe,
) where

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (ScriptType (..))
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Shelley.Generic.Script (fromMultiSig)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Era.Shelley.Generic.Witness
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes (TxIx (..), strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (EraCrypto)
import qualified Cardano.Ledger.SafeHash as Ledger
import Cardano.Ledger.Shelley.Scripts (MultiSig, ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
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
    }
  where
    txBody :: Core.TxBody StandardShelley
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = mkTxOut txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript <$> Map.toList (ShelleyTx.txwitsScript tx)

    mkTxScript :: (ScriptHash (EraCrypto StandardShelley), MultiSig StandardShelley) -> TxScript
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
  (Core.EraTxBody era, Core.Value era ~ Coin, EraCrypto era ~ StandardCrypto) =>
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
        bs = Ledger.unCompactAddr $ txOut ^. Core.compactAddrTxOutL

fromTxIn :: ShelleyTx.TxIn StandardCrypto -> TxIn
fromTxIn (ShelleyTx.TxIn (ShelleyTx.TxId txid) (TxIx w64)) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = w64
    , txInRedeemerIndex = Nothing
    }

txHashId :: (EraCrypto era ~ StandardCrypto, Core.EraTx era) => Core.Tx era -> ByteString
txHashId tx = Crypto.hashToBytes $ Ledger.extractHash $ Ledger.hashAnnotated (tx ^. Core.bodyTxL)

txHashFromSafe :: Ledger.SafeHash StandardCrypto Core.EraIndependentTxBody -> ByteString
txHashFromSafe = Crypto.hashToBytes . Ledger.extractHash

getTxSize :: Core.EraTx era => Core.Tx era -> Word64
getTxSize tx = fromIntegral $ tx ^. Core.sizeTxF

mkTxIn ::
  (Core.EraTxBody era, EraCrypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  [TxIn]
mkTxIn txBody = map fromTxIn $ toList $ txBody ^. Core.inputsTxBodyL

calcWithdrawalSum ::
  (Shelley.ShelleyEraTxBody era, EraCrypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  Coin
calcWithdrawalSum bd =
  Coin $ sum $ map unCoin $ Map.elems $ Shelley.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL

getTxMetadata :: Core.EraTx era => Core.Tx era -> Maybe (Core.TxAuxData era)
getTxMetadata tx = strictMaybeToMaybe (tx ^. Core.auxDataTxL)

mkTxWithdrawals ::
  (Shelley.ShelleyEraTxBody era, EraCrypto era ~ StandardCrypto) =>
  Core.TxBody era ->
  [TxWithdrawal]
mkTxWithdrawals bd =
  map mkTxWithdrawal $ Map.toList $ Shelley.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL

mkTxWithdrawal :: (Shelley.RewardAcnt StandardCrypto, Coin) -> TxWithdrawal
mkTxWithdrawal (ra, c) =
  TxWithdrawal
    { txwRedeemerIndex = Nothing
    , txwRewardAccount = ra
    , txwAmount = c
    }

mkTxParamProposal ::
  (Shelley.ShelleyEraTxBody era, EraCrypto era ~ StandardCrypto, Core.ProtVerAtMost era 8) =>
  Witness era ->
  Core.TxBody era ->
  [ParamProposal]
mkTxParamProposal witness txBody =
  maybe [] (convertParamProposal witness) $ strictMaybeToMaybe (txBody ^. Shelley.updateTxBodyL)

mkTxCertificates ::
  forall era.
  ( Shelley.ShelleyEraTxBody era
  , TxCert era ~ ShelleyTxCert era
  , EraCrypto era ~ StandardCrypto
  ) =>
  Core.TxBody era ->
  [TxCertificate]
mkTxCertificates bd =
  zipWith mkTxCertificate [0 ..] $ toShelleyCert <$> toList (bd ^. Core.certsTxBodyL)

toShelleyCert :: EraCrypto era ~ StandardCrypto => ShelleyTxCert era -> ShelleyCert
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
