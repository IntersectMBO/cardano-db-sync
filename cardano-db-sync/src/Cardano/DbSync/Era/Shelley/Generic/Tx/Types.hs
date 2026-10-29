{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Types (
  Tx (..),
  ShelleyCert,
  ConwayCert,
  DijkstraCert,
  Cert (..),
  Voting (..),
  Proposal (..),
  GovStateW (..),
  DrepSnapW (..),
  TxCertificate (..),
  TxWithdrawal (..),
  TxDirectDeposit (..),
  TxGuard (..),
  TxRequiredGuard (..),
  TxAccountBalanceInterval (..),
  TxIn (..),
  TxOut (..),
  TxRedeemer (..),
  TxScript (..),
  PlutusData (..),
  TxOutDatum (..),
  PoolStats (..),
  DBScriptPurpose (..),
  DBPlutusScript (..),
  toTxCert,
  whenInlineDatum,
  getTxOutDatumHash,
  getMaybeDatumHash,
  sumTxOutCoin,
  toTxHash,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Metadata (TxMetadataValue (..))
import Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Scripts
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Mary.Value (AssetName, MultiAsset, PolicyID)
import Cardano.Ledger.Shelley.TxCert
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Cardano.Block (AlonzoEra, BabbageEra, ConwayEra, DijkstraEra, ShelleyEra)

data Tx = Tx
  { txHash :: !ByteString
  , txLedgerTxId :: !Ledger.TxId
  , txBlockIndex :: !Word64
  , txCBOR :: ByteString
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
  , txDirectDeposits :: ![TxDirectDeposit]
  , txParamProposal :: ![ParamProposal]
  , txMint :: !MultiAsset
  , txRedeemer :: [(Word64, TxRedeemer)]
  , txData :: [PlutusData]
  , txScriptSizes :: [Word64] -- this contains only the sizes of plutus scripts in witnesses
  , txScripts :: [TxScript]
  , txExtraKeyWitnesses :: ![ByteString]
  , txGuards :: ![TxGuard]
  , txRequiredGuards :: ![TxRequiredGuard]
  , txAccountBalanceIntervals :: ![TxAccountBalanceInterval]
  , txVotingProcedure :: ![(Voter, [(GovActionId, Voting)])]
  , txProposalProcedure :: ![(GovActionId, Proposal)]
  , txTreasuryDonation :: !Coin
  , -- Dijkstra nested sub-transactions, each with its own TxId. Empty in every other era.
    txSubTransactions :: ![Tx]
  }

type ShelleyCert = ShelleyTxCert ShelleyEra
type ConwayCert = ConwayTxCert ConwayEra
type DijkstraCert = DijkstraTxCert DijkstraEra
data Cert = SCert ShelleyCert | CCert ConwayCert | DCert DijkstraCert
  deriving (Eq)

data Voting
  = VotingC !(VotingProcedure ConwayEra)
  | VotingD !(VotingProcedure DijkstraEra)

data Proposal
  = ProposalC !(ProposalProcedure ConwayEra)
  | ProposalD !(ProposalProcedure DijkstraEra)

data GovStateW
  = GovStateC !(ConwayGovState ConwayEra)
  | GovStateD !(ConwayGovState DijkstraEra)

data DrepSnapW
  = DrepSnapC !(DRepPulsingState ConwayEra)
  | DrepSnapD !(DRepPulsingState DijkstraEra)

data TxCertificate = TxCertificate
  { txcRedeemerIndex :: !(Maybe Word64)
  , txcIndex :: !Word16
  , txcCert :: !Cert
  }

data TxWithdrawal = TxWithdrawal
  { txwRedeemerIndex :: !(Maybe Word64)
  , txwRewardAccount :: !Ledger.AccountAddress
  , txwAmount :: !Coin
  }

-- | A Dijkstra direct deposit: lovelace paid straight into an account, with no
-- corresponding output. The mirror of a 'TxWithdrawal', and carries no redeemer.
data TxDirectDeposit = TxDirectDeposit
  { txddAccount :: !Ledger.AccountAddress
  , txddAmount :: !Coin
  }

-- | A guard credential a Dijkstra transaction body supplies ('guards'): a credential
-- whose key must sign, or whose script must run, for the body to be valid. 'txgIndex' is
-- the credential's position in the body's ordered guard set, which is the index the
-- matching redeemer with purpose 'guard' points at.
data TxGuard = TxGuard
  { txgIndex :: !Word64
  , txgCredential :: !ByteString
  , txgHasScript :: !Bool
  }

-- | A guard credential a Dijkstra transaction body requires of its enclosing top-level
-- transaction ('requiredTopLevelGuards'), with the datum the body supplies for it. The
-- ledger requires a datum exactly for a Plutus script credential and forbids one for a
-- key hash or a native script, so 'txrgDatum' is 'Nothing' for those.
data TxRequiredGuard = TxRequiredGuard
  { txrgCredential :: !ByteString
  , txrgHasScript :: !Bool
  , txrgDatum :: !(Maybe PlutusData)
  }

-- | A balance assertion a Dijkstra transaction body makes about an account: the body is
-- only valid if the account's balance falls in the interval. 'txabiStarting' tells the two
-- assertion sets apart: 'accountBalanceIntervals' (False) is checked against the account
-- as the body is applied, while 'startingAccountBalanceIntervals' (True) is checked
-- against the account as it stood before the whole transaction, and is carried by
-- top-level bodies only. Exactly one of 'txabiExactValue' and the bound pair is set; the
-- lower bound is inclusive and the upper bound exclusive.
data TxAccountBalanceInterval = TxAccountBalanceInterval
  { txabiStarting :: !Bool
  , txabiAccount :: !Ledger.AccountAddress
  , txabiLowerBound :: !(Maybe Coin)
  , txabiUpperBound :: !(Maybe Coin)
  , txabiExactValue :: !(Maybe Coin)
  }

data TxIn = TxIn
  { txInIndex :: !Word64
  , txInTxId :: !Ledger.TxId
  , txInRedeemerIndex :: !(Maybe Word64) -- This only has a meaning for Alonzo.
  }
  deriving (Show)

data TxOut = TxOut
  { txOutIndex :: !Word64
  , txOutAddress :: !Ledger.Addr
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map PolicyID (Map AssetName Integer))
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

data PoolStats = PoolStats
  { nBlocks :: Natural
  , nDelegators :: Word64
  , stake :: Coin
  , votingPower :: Maybe Coin
  }

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

toTxHash :: TxIn -> ByteString
toTxHash = unTxHash . txInTxId

class AlonzoEraTxBody era => DBScriptPurpose era where
  getPurpose :: PlutusPurpose AsIx era -> (DB.ScriptPurpose, Word32)
  toAlonzoPurpose :: TxBody l era -> PlutusPurpose AsItem era -> Maybe (Either (AlonzoPlutusPurpose AsItem era, Maybe (PlutusPurpose AsIx era)) (ConwayPlutusPurpose AsItem era))

instance DBScriptPurpose AlonzoEra where
  getPurpose = \case
    AlonzoSpending idx -> (DB.Spend, unAsIx idx)
    AlonzoMinting idx -> (DB.Mint, unAsIx idx)
    AlonzoCertifying idx -> (DB.Cert, unAsIx idx)
    AlonzoWithdrawing idx -> (DB.Rewrd, unAsIx idx)

  toAlonzoPurpose txBody pp = case pp of
    AlonzoSpending a -> Just $ Left (AlonzoSpending a, Nothing)
    AlonzoMinting a -> Just $ Left (AlonzoMinting a, Nothing)
    AlonzoWithdrawing a -> Just $ Left (AlonzoWithdrawing a, Nothing)
    AlonzoCertifying a -> Just $ Left (AlonzoCertifying a, strictMaybeToMaybe (alonzoRedeemerPointer txBody pp))

instance DBScriptPurpose BabbageEra where
  getPurpose = \case
    AlonzoSpending idx -> (DB.Spend, unAsIx idx)
    AlonzoMinting idx -> (DB.Mint, unAsIx idx)
    AlonzoCertifying idx -> (DB.Cert, unAsIx idx)
    AlonzoWithdrawing idx -> (DB.Rewrd, unAsIx idx)

  toAlonzoPurpose txBody pp = case pp of
    AlonzoSpending a -> Just $ Left (AlonzoSpending a, Nothing)
    AlonzoMinting a -> Just $ Left (AlonzoMinting a, Nothing)
    AlonzoWithdrawing a -> Just $ Left (AlonzoWithdrawing a, Nothing)
    AlonzoCertifying a -> Just $ Left (AlonzoCertifying a, strictMaybeToMaybe (alonzoRedeemerPointer txBody pp))

instance DBScriptPurpose ConwayEra where
  getPurpose = \case
    ConwaySpending idx -> (DB.Spend, unAsIx idx)
    ConwayMinting idx -> (DB.Mint, unAsIx idx)
    ConwayCertifying idx -> (DB.Cert, unAsIx idx)
    ConwayWithdrawing idx -> (DB.Rewrd, unAsIx idx)
    ConwayVoting idx -> (DB.Vote, unAsIx idx)
    ConwayProposing idx -> (DB.Propose, unAsIx idx)

  toAlonzoPurpose _ = \case
    ConwayVoting _ -> Nothing
    ConwayProposing _ -> Nothing
    a -> Just $ Right a

instance DBScriptPurpose DijkstraEra where
  getPurpose = \case
    DijkstraSpending idx -> (DB.Spend, unAsIx idx)
    DijkstraMinting idx -> (DB.Mint, unAsIx idx)
    DijkstraCertifying idx -> (DB.Cert, unAsIx idx)
    DijkstraWithdrawing idx -> (DB.Rewrd, unAsIx idx)
    DijkstraVoting idx -> (DB.Vote, unAsIx idx)
    DijkstraProposing idx -> (DB.Propose, unAsIx idx)
    DijkstraGuarding idx -> (DB.Guard, unAsIx idx)

  toAlonzoPurpose _ = \case
    DijkstraVoting _ -> Nothing
    DijkstraProposing _ -> Nothing
    DijkstraGuarding _ -> Nothing
    DijkstraSpending idx -> Just $ Right $ ConwaySpending idx
    DijkstraMinting idx -> Just $ Right $ ConwayMinting idx
    DijkstraCertifying idx -> Just $ Right $ ConwayCertifying idx
    DijkstraWithdrawing idx -> Just $ Right $ ConwayWithdrawing idx

class AlonzoEraScript era => DBPlutusScript era where
  getPlutusScriptType :: PlutusScript era -> DB.ScriptType

instance DBPlutusScript AlonzoEra where
  getPlutusScriptType _ = DB.PlutusV1

instance DBPlutusScript BabbageEra where
  getPlutusScriptType (BabbagePlutusV1 _) = DB.PlutusV1
  getPlutusScriptType (BabbagePlutusV2 _) = DB.PlutusV2

instance DBPlutusScript ConwayEra where
  getPlutusScriptType (ConwayPlutusV1 _) = DB.PlutusV1
  getPlutusScriptType (ConwayPlutusV2 _) = DB.PlutusV2
  getPlutusScriptType (ConwayPlutusV3 _) = DB.PlutusV3

instance DBPlutusScript DijkstraEra where
  getPlutusScriptType (DijkstraPlutusV1 _) = DB.PlutusV1
  getPlutusScriptType (DijkstraPlutusV2 _) = DB.PlutusV2
  getPlutusScriptType (DijkstraPlutusV3 _) = DB.PlutusV3
  getPlutusScriptType (DijkstraPlutusV4 _) = DB.PlutusV4
