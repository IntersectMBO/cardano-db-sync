{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley where

import           Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import           Cardano.Ledger.Core (Value)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra)

import qualified Cardano.Api.Shelley as Api

import           Cardano.Db (ScriptType (..))

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Witness

fromShelleyTx :: (Word64, ShelleyTx.Tx StandardShelley) -> Tx
fromShelleyTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map fromTxIn (toList . Shelley._inputs $ ShelleyTx.body tx)
      , txCollateralInputs = []  -- Shelley does not have collateral inputs
      , txReferenceInputs = []   -- Shelley does not have reference inputs
      , txOutputs = outputs
      , txCollateralOutputs = [] -- Shelley does not have collateral outputs
      , txFees = Shelley._txfee (ShelleyTx.body tx)
      , txOutSum = sumOutputs outputs
      , txInvalidBefore = Nothing
      , txInvalidHereafter = Just $ Shelley._ttl (ShelleyTx.body tx)
      , txWithdrawalSum = getWithdrawalSum $ Shelley._wdrls (ShelleyTx.body tx)
      , txMetadata = fromShelleyMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = zipWith mkTxCertificate [0..] (toList . Shelley._certs $ ShelleyTx.body tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . Shelley._wdrls $ ShelleyTx.body tx)
      , txParamProposal = maybe [] (convertParamProposal (Shelley Standard)) $ strictMaybeToMaybe (ShelleyTx._txUpdate $ ShelleyTx.body tx)
      , txMint = mempty       -- Shelley does not support multi-assets
      , txRedeemer = []       -- Shelley does not support redeemers
      , txData = []
      , txScriptSizes = []    -- Shelley does not support plutus scripts
      , txScripts = scripts
      , txScriptsFee = Coin 0 -- Shelley does not support plutus scripts
      , txExtraKeyWitnesses = []
      }
  where
    outputs :: [TxOut]
    outputs = getOutputs tx

    scripts :: [TxScript]
    scripts =
      mkTxScript <$> Map.toList (ShelleyTx.scriptWits $ getField @"wits" tx)

    mkTxScript :: (ScriptHash StandardCrypto, Shelley.MultiSig StandardCrypto) -> TxScript
    mkTxScript (hsh, script) = TxScript
      { txScriptHash = unScriptHash hsh
      , txScriptType = MultiSig
      , txScriptPlutusSize = Nothing
      , txScriptJson = Just . LBS.toStrict . Aeson.encode $ Api.fromShelleyMultiSig script
      , txScriptCBOR = Nothing
      }

getOutputs :: ShelleyTx.Tx StandardShelley -> [TxOut]
getOutputs tx = zipWith fromTxOut [0 .. ] $ toList (Shelley._outputs $ ShelleyTx.body tx)

fromTxOut :: (Value era ~ Coin, Ledger.Crypto era ~ StandardCrypto, Ledger.Era era) => Word16 -> ShelleyTx.TxOut era -> TxOut
fromTxOut index txOut =
  TxOut
    { txOutIndex = index
    , txOutAddress = addr
    , txOutAddressRaw = SBS.fromShort bs
    , txOutAdaValue = ada
    , txOutMaValue = mempty  -- Shelley does not support multi-assets
    , txOutScript = Nothing
    , txOutDatum = NoDatum   -- Shelley does not support plutus data
    }
  where
    ShelleyTx.TxOutCompact (Ledger.UnsafeCompactAddr bs) _ = txOut
    -- This pattern match also does the deserialisation of the address
    ShelleyTx.TxOut addr ada = txOut


fromTxIn :: ShelleyTx.TxIn StandardCrypto -> TxIn
fromTxIn (ShelleyTx.TxIn (ShelleyTx.TxId txid) (TxIx w16)) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = fromIntegral w16
    , txInRedeemerIndex = Nothing
    }

txHashId :: (Ledger.Crypto era ~ StandardCrypto, ShelleyBasedEra era) => ShelleyTx.Tx era -> ByteString
txHashId = Crypto.hashToBytes . Ledger.extractHash . Ledger.hashAnnotated . ShelleyTx.body

getWithdrawalSum :: Shelley.Wdrl StandardCrypto -> Coin
getWithdrawalSum = Coin . sum . map unCoin . Map.elems . Shelley.unWdrl

mkTxWithdrawal :: (Shelley.RewardAcnt StandardCrypto, Coin) -> TxWithdrawal
mkTxWithdrawal (ra, c) =
  TxWithdrawal
    { txwRedeemerIndex = Nothing
    , txwRewardAccount = ra
    , txwAmount = c
    }

mkTxCertificate :: Word16 -> Shelley.DCert StandardCrypto -> TxCertificate
mkTxCertificate ix dcert =
  TxCertificate
    { txcRedeemerIndex = Nothing
    , txcIndex = ix
    , txcCert = dcert
    }