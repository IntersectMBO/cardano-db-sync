{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Types where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.Alonzo.Scripts (Tag (..))
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import           Cardano.Api.Shelley (TxMetadataValue (..))

import           Cardano.Db (ScriptType (..))

import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal


data Tx = Tx
  { txHash :: !ByteString
  , txBlockIndex :: !Word64
  , txSize :: !Word64
  , txValidContract :: !Bool
  , txInputs :: ![TxIn]
  , txCollateralInputs :: ![TxIn]
  , txOutputs :: ![TxOut]
  , txFees :: !Coin
  , txOutSum :: !Coin
  , txInvalidBefore :: !(Maybe SlotNo)
  , txInvalidHereafter :: !(Maybe SlotNo)
  , txWithdrawalSum :: !Coin
  , txMetadata :: !(Maybe (Map Word64 TxMetadataValue))
  , txCertificates :: ![TxCertificate]
  , txWithdrawals :: ![TxWithdrawal]
  , txParamProposal :: ![ParamProposal]
  , txMint :: !(Value StandardCrypto)
  , txRedeemer :: [TxRedeemer]
  , txData :: [TxDatum]
  , txScriptSizes :: [Word64] -- this contains only the sizes of plutus scripts in witnesses
  , txScripts :: [TxScript]
  , txScriptsFee :: Coin -- fees for plutus scripts
  , txExtraKeyWitnesses :: ![ByteString]
  }

data TxCertificate = TxCertificate
  { txcRedeemerIndex :: !(Maybe Word64)
  , txcIndex :: !Word16
  , txcCert :: !(Shelley.DCert StandardCrypto)
  }

data TxWithdrawal = TxWithdrawal
  { txwRedeemerIndex :: !(Maybe Word64)
  , txwRewardAccount :: !(Shelley.RewardAcnt StandardCrypto)
  , txwAmount :: !Coin
  }

data TxIn = TxIn
  { txInHash :: !ByteString
  , txInIndex :: !Word16
  , txInRedeemerIndex :: !(Maybe Word64) -- This only has a meaning for Alonzo.
  }

data TxOut = TxOut
  { txOutIndex :: !Word16
  , txOutAddress :: !(Ledger.Addr StandardCrypto)
  , txOutAddressRaw :: !ByteString
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  , txOutDataHash :: !(Maybe ByteString)
  }

data TxRedeemer = TxRedeemer
  { txRedeemerMem :: !Word64
  , txRedeemerSteps :: !Word64
  , txRedeemerPurpose :: !Tag
  , txRedeemerFee :: !Coin
  , txRedeemerIndex :: !Word64
  , txRedeemerScriptHash :: Maybe (Either TxIn ByteString)
  , txRedeemerDatum :: TxDatum
  }

data TxScript = TxScript
  { txScriptHash :: !ByteString
  , txScriptType :: ScriptType
  , txScriptPlutusSize :: Maybe Word64
  , txScriptJson :: Maybe ByteString
  , txScriptCBOR :: Maybe ByteString
  }

data TxDatum = TxDatum
  { txDatumHash :: !ByteString
  , txDatumValue :: !ByteString -- we turn this into json later.
  }

sumOutputs :: [TxOut] -> Coin
sumOutputs = Coin . sum . map (unCoin . txOutAdaValue)
