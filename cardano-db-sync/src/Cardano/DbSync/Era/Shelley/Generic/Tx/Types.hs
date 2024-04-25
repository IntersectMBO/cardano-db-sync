{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Types (
  Tx (..),
  ShelleyCert,
  ConwayCert,
  Cert,
  TxCertificate (..),
  TxWithdrawal (..),
  TxIn (..),
  TxOut (..),
  TxRedeemer (..),
  TxScript (..),
  PlutusData (..),
  TxOutDatum (..),
  DBScriptPurpose (..),
  DBPlutusScript (..),
  toTxCert,
  whenInlineDatum,
  getTxOutDatumHash,
  getMaybeDatumHash,
  sumTxOutCoin,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Metadata (TxMetadataValue (..))
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Scripts
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Core (TxBody)
import Cardano.Ledger.Mary.Value (AssetName, MultiAsset, PolicyID)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Cardano.Block (StandardAlonzo, StandardBabbage, StandardConway, StandardCrypto, StandardShelley)

data Tx = Tx
  { txHash :: !ByteString
  , txBlockIndex :: !Word64
  , txSize :: !Word64
  , txValidContract :: !Bool
  , txInputs :: ![TxIn]
  , txCollateralInputs :: ![TxIn]
  , txReferenceInputs :: ![TxIn]
  , txOutputs :: ![TxOut]
  , txCollateralOutputs :: ![TxOut]
  , txFees :: !(Maybe Coin) -- Nothing means it needs to be computed by inSum - outSum and happens on phase 2 failures.
  , txOutSum :: !Coin
  , txInvalidBefore :: !(Maybe SlotNo)
  , txInvalidHereafter :: !(Maybe SlotNo)
  , txWithdrawalSum :: !Coin
  , txMetadata :: !(Maybe (Map Word64 TxMetadataValue))
  , txCertificates :: ![TxCertificate]
  , txWithdrawals :: ![TxWithdrawal]
  , txParamProposal :: ![ParamProposal]
  , txMint :: !(MultiAsset StandardCrypto)
  , txRedeemer :: [(Word64, TxRedeemer)]
  , txData :: [PlutusData]
  , txScriptSizes :: [Word64] -- this contains only the sizes of plutus scripts in witnesses
  , txScripts :: [TxScript]
  , txExtraKeyWitnesses :: ![ByteString]
  , txVotingProcedure :: ![(Voter StandardCrypto, [(GovActionId StandardCrypto, VotingProcedure StandardConway)])]
  , txProposalProcedure :: ![ProposalProcedure StandardConway]
  }

type ShelleyCert = ShelleyTxCert StandardShelley
type ConwayCert = ConwayTxCert StandardConway
type Cert = Either ShelleyCert ConwayCert

data TxCertificate = TxCertificate
  { txcRedeemerIndex :: !(Maybe Word64)
  , txcIndex :: !Word16
  , txcCert :: !Cert
  }

data TxWithdrawal = TxWithdrawal
  { txwRedeemerIndex :: !(Maybe Word64)
  , txwRewardAccount :: !(Shelley.RewardAcnt StandardCrypto)
  , txwAmount :: !Coin
  }

data TxIn = TxIn
  { txInHash :: !ByteString
  , txInIndex :: !Word64
  , txInRedeemerIndex :: !(Maybe Word64) -- This only has a meaning for Alonzo.
  }
  deriving (Show)

data TxOut = TxOut
  { txOutIndex :: !Word64
  , txOutAddress :: !(Ledger.Addr StandardCrypto)
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  , txOutScript :: Maybe TxScript
  , txOutDatum :: !TxOutDatum
  }

data TxRedeemer = TxRedeemer
  { txRedeemerMem :: !Word64
  , txRedeemerSteps :: !Word64
  , txRedeemerPurpose :: !DB.ScriptPurpose
  , txRedeemerFee :: !(Maybe Coin)
  , txRedeemerIndex :: !Word64
  , txRedeemerScriptHash :: Maybe (Either TxIn ByteString)
  , txRedeemerData :: PlutusData
  }

-- Fields are intentionally left lazy, to avoid transformation if the entry already
-- exists in db.
data TxScript = TxScript
  { txScriptHash :: !ByteString
  , txScriptType :: DB.ScriptType
  , txScriptPlutusSize :: Maybe Word64
  , txScriptJson :: Maybe ByteString
  , txScriptCBOR :: Maybe ByteString
  }

-- Fields are intentionally left lazy, to avoid transformation if the entry already
-- exists in db.
data PlutusData = PlutusData
  { txDataHash :: !DataHash
  , txDataValue :: ByteString -- we turn this into json later.
  , txDataBytes :: ByteString
  }

data TxOutDatum = InlineDatum PlutusData | DatumHash DataHash | NoDatum

toTxCert :: Word16 -> Cert -> TxCertificate
toTxCert idx dcert =
  TxCertificate
    { txcRedeemerIndex = Nothing
    , txcIndex = idx
    , txcCert = dcert
    }

whenInlineDatum :: Monad m => TxOutDatum -> (PlutusData -> m a) -> m (Maybe a)
whenInlineDatum (InlineDatum pd) f = Just <$> f pd
whenInlineDatum _ _ = pure Nothing

getTxOutDatumHash :: TxOutDatum -> Maybe DataHash
getTxOutDatumHash (InlineDatum txDatum) = Just $ txDataHash txDatum
getTxOutDatumHash (DatumHash hsh) = Just hsh
getTxOutDatumHash NoDatum = Nothing

getMaybeDatumHash :: Maybe DataHash -> TxOutDatum
getMaybeDatumHash Nothing = NoDatum
getMaybeDatumHash (Just hsh) = DatumHash hsh

sumTxOutCoin :: [TxOut] -> Coin
sumTxOutCoin = Coin . sum . map (unCoin . txOutAdaValue)

class AlonzoEraTxBody era => DBScriptPurpose era where
  getPurpose :: PlutusPurpose AsIndex era -> (DB.ScriptPurpose, Word32)
  toAlonzoPurpose :: TxBody era -> PlutusPurpose AsItem era -> Maybe (Either (AlonzoPlutusPurpose AsItem era, Maybe (PlutusPurpose AsIndex era)) (ConwayPlutusPurpose AsItem era))

instance DBScriptPurpose StandardAlonzo where
  getPurpose = \case
    AlonzoSpending idx -> (DB.Spend, unAsIndex idx)
    AlonzoMinting idx -> (DB.Mint, unAsIndex idx)
    AlonzoCertifying idx -> (DB.Cert, unAsIndex idx)
    AlonzoRewarding idx -> (DB.Rewrd, unAsIndex idx)

  toAlonzoPurpose txBody pp = case pp of
    AlonzoSpending a -> Just $ Left (AlonzoSpending a, Nothing)
    AlonzoMinting a -> Just $ Left (AlonzoMinting a, Nothing)
    AlonzoRewarding a -> Just $ Left (AlonzoRewarding a, Nothing)
    AlonzoCertifying a -> Just $ Left (AlonzoCertifying a, strictMaybeToMaybe (redeemerPointer txBody pp))

instance DBScriptPurpose StandardBabbage where
  getPurpose = \case
    AlonzoSpending idx -> (DB.Spend, unAsIndex idx)
    AlonzoMinting idx -> (DB.Mint, unAsIndex idx)
    AlonzoCertifying idx -> (DB.Cert, unAsIndex idx)
    AlonzoRewarding idx -> (DB.Rewrd, unAsIndex idx)

  toAlonzoPurpose txBody pp = case pp of
    AlonzoSpending a -> Just $ Left (AlonzoSpending a, Nothing)
    AlonzoMinting a -> Just $ Left (AlonzoMinting a, Nothing)
    AlonzoRewarding a -> Just $ Left (AlonzoRewarding a, Nothing)
    AlonzoCertifying a -> Just $ Left (AlonzoCertifying a, strictMaybeToMaybe (redeemerPointer txBody pp))

instance DBScriptPurpose StandardConway where
  getPurpose = \case
    ConwaySpending idx -> (DB.Spend, unAsIndex idx)
    ConwayMinting idx -> (DB.Mint, unAsIndex idx)
    ConwayCertifying idx -> (DB.Cert, unAsIndex idx)
    ConwayRewarding idx -> (DB.Rewrd, unAsIndex idx)
    ConwayVoting idx -> (DB.Vote, unAsIndex idx)
    ConwayProposing idx -> (DB.Propose, unAsIndex idx)

  toAlonzoPurpose _ = \case
    ConwayVoting _ -> Nothing
    ConwayProposing _ -> Nothing
    a -> Just $ Right a

class AlonzoEraScript era => DBPlutusScript era where
  getPlutusScriptType :: PlutusScript era -> DB.ScriptType

instance DBPlutusScript StandardAlonzo where
  getPlutusScriptType _ = DB.PlutusV1

instance DBPlutusScript StandardBabbage where
  getPlutusScriptType (BabbagePlutusV1 _) = DB.PlutusV1
  getPlutusScriptType (BabbagePlutusV2 _) = DB.PlutusV2

instance DBPlutusScript StandardConway where
  getPlutusScriptType (ConwayPlutusV1 _) = DB.PlutusV1
  getPlutusScriptType (ConwayPlutusV2 _) = DB.PlutusV2
  getPlutusScriptType (ConwayPlutusV3 _) = DB.PlutusV3
