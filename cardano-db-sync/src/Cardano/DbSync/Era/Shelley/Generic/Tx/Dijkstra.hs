{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Dijkstra (
  fromDijkstraTx,
  fromDijkstraTxOut,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo (extraKeyWits, getPlutusSizes, mkCollTxIn, mkTxData, resolveRedeemers, rmCerts, rmInps, rmWdrl, txDataWitness)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util (unScriptHash)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxAuxData (getAlonzoTxAuxDataScripts)
import Cardano.Ledger.Babbage.Core as Core hiding (Tx, TxOut)
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import qualified Cardano.Ledger.Plutus.Data as Alonzo
import Cardano.Ledger.TxIn
import Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (DijkstraEra)

fromDijkstraTx :: Bool -> Maybe Alonzo.Prices -> (Word64, Core.Tx Core.TopTx DijkstraEra) -> Tx
fromDijkstraTx ioExtraPlutus mprices (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txLedgerTxId = mkTxId tx
    , txBlockIndex = blkIndex
    , txCBOR = getTxCBOR tx
    , txSize = getTxSize tx
    , txValidContract = isValid2
    , txInputs =
        if not isValid2
          then collInputs
          else Map.elems $ rmInps finalMaps
    , txCollateralInputs = collInputs
    , txReferenceInputs = map fromTxIn . toList $ txBody ^. referenceInputsTxBodyL
    , txOutputs =
        if not isValid2
          then collOutputs
          else outputs
    , txCollateralOutputs =
        collOutputs
    , txFees =
        if not isValid2
          then strictMaybeToMaybe $ txBody ^. totalCollateralTxBodyL
          else Just $ txBody ^. feeTxBodyL
    , txOutSum =
        if not isValid2
          then sumTxOutCoin collOutputs
          else sumTxOutCoin outputs
    , txInvalidBefore = invalidBef
    , txInvalidHereafter = invalidAfter
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromAlonzoMetadata <$> getTxMetadata tx
    , txCertificates = snd <$> rmCerts finalMaps
    , txWithdrawals = Map.elems $ rmWdrl finalMaps
    , txParamProposal = []
    , txMint = txBody ^. mintTxBodyL
    , txRedeemer = redeemers
    , txData = txDataWitness tx
    , txScriptSizes = getPlutusSizes tx
    , txScripts = getDijkstraScripts tx
    , txExtraKeyWitnesses = extraKeyWits txBody
    , txVotingProcedure = Map.toList $ fmap (Map.toList . fmap VotingD) (unVotingProcedures $ dtbVotingProcedures txBody)
    , txProposalProcedure = zipWith mkProposalIndex [0 ..] $ map ProposalD $ toList $ dtbProposalProcedures txBody
    , txTreasuryDonation = dtbTreasuryDonation txBody
    , txSubTransactions =
        map (fromDijkstraSubTx ioExtraPlutus mprices blkIndex) $
          toList (txBody ^. subTransactionsTxBodyL)
    }
  where
    txBody :: Core.TxBody Core.TopTx DijkstraEra
    txBody = tx ^. Core.bodyTxL

    txId :: TxId
    txId = mkTxId tx

    outputs :: [TxOut]
    outputs = zipWith fromDijkstraTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)

    -- TODO when collateral output is used as output, its index is not 0, but length of outputs
    -- even though it is the unique output of the tx.
    collOutputs :: [TxOut]
    collOutputs = zipWith fromDijkstraTxOut [collIndex ..] . toList $ (txBody ^. collateralReturnTxBodyL)

    collIndex :: Word64
    collIndex =
      case txIxFromIntegral (length outputs) of
        Just (TxIx i) -> fromIntegral i
        Nothing -> fromIntegral (maxBound :: Word16)

    -- This is true if second stage contract validation passes.
    isValid2 :: Bool
    isValid2 =
      case tx ^. Alonzo.isPhase2ValidTxL of
        Alonzo.Phase2Valid -> True
        Alonzo.Phase2Invalid -> False

    (finalMaps, redeemers) = resolveRedeemers ioExtraPlutus mprices tx DCert
    (invalidBef, invalidAfter) = getInterval txBody

    collInputs = mkCollTxIn txBody

    mkProposalIndex :: Word16 -> a -> (GovActionId, a)
    mkProposalIndex gix a = (GovActionId txId (GovActionIx gix), a)

-- | A Dijkstra nested sub-transaction. It has its own TxId, body and witnesses, but no
-- collateral, no fee and no phase-2 validity flag of its own; the enclosing top-level tx
-- governs all three. It cannot nest further. The parent link is set at insertion time.
-- It does carry its own certificates, withdrawals, votes, proposals and treasury donation.
fromDijkstraSubTx :: Bool -> Maybe Alonzo.Prices -> Word64 -> Core.Tx Core.SubTx DijkstraEra -> Tx
fromDijkstraSubTx ioExtraPlutus mprices blkIndex tx =
  Tx
    { txHash = txHashId tx
    , txLedgerTxId = mkTxId tx
    , txBlockIndex = blkIndex
    , txCBOR = getTxCBOR tx
    , txSize = getTxSize tx
    , -- Only sub-transactions of a batch that passed phase 2 are inserted at all.
      txValidContract = True
    , txInputs = Map.elems $ rmInps finalMaps
    , txCollateralInputs = []
    , txReferenceInputs = map fromTxIn . toList $ txBody ^. referenceInputsTxBodyL
    , txOutputs = outputs
    , txCollateralOutputs = []
    , txFees = Just mempty -- the top-level tx pays
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = invalidBef
    , txInvalidHereafter = invalidAfter
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromAlonzoMetadata <$> getTxMetadata tx
    , txCertificates = snd <$> rmCerts finalMaps
    , txWithdrawals = Map.elems $ rmWdrl finalMaps
    , txParamProposal = []
    , txMint = txBody ^. mintTxBodyL
    , txRedeemer = redeemers
    , txData = txDataWitness tx
    , txScriptSizes = getPlutusSizes tx
    , txScripts = getDijkstraScripts tx
    , txExtraKeyWitnesses = extraKeyWits txBody
    , -- A proposal made in a sub-tx is keyed by that sub-tx's own TxId, which is what
      -- the ledger uses in the governance state, so insertProposal's lookup finds it.
      txVotingProcedure = Map.toList $ fmap (Map.toList . fmap VotingD) (unVotingProcedures $ dstbVotingProcedures txBody)
    , txProposalProcedure = zipWith mkProposalIndex [0 ..] $ map ProposalD $ toList $ dstbProposalProcedures txBody
    , txTreasuryDonation = dstbTreasuryDonation txBody
    , txSubTransactions = [] -- cannot nest further
    }
  where
    txBody :: Core.TxBody Core.SubTx DijkstraEra
    txBody = tx ^. Core.bodyTxL

    txId :: TxId
    txId = mkTxId tx

    outputs :: [TxOut]
    outputs = zipWith fromDijkstraTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)

    (finalMaps, redeemers) = resolveRedeemers ioExtraPlutus mprices tx DCert
    (invalidBef, invalidAfter) = getInterval txBody

    mkProposalIndex :: Word16 -> a -> (GovActionId, a)
    mkProposalIndex gix a = (GovActionId txId (GovActionIx gix), a)

-- | Dijkstra-specific TxOut conversion. DijkstraNativeScript is not Timelock,
-- so we can't reuse Babbage.fromTxOut which requires NativeScript era ~ Timelock era.
fromDijkstraTxOut :: Word64 -> BabbageTxOut DijkstraEra -> TxOut
fromDijkstraTxOut index txOut =
  TxOut
    { txOutIndex = index
    , txOutAddress = txOut ^. Core.addrTxOutL
    , txOutAdaValue = ada
    , txOutMaValue = maMap
    , txOutScript = fromDijkstraScript <$> strictMaybeToMaybe mScript
    , txOutDatum = fromDijkstraDatum datum
    }
  where
    MaryValue ada (MultiAsset maMap) = txOut ^. Core.valueTxOutL
    datum = txOut ^. Core.datumTxOutL
    mScript = txOut ^. Core.referenceScriptTxOutL

fromDijkstraScript :: Alonzo.AlonzoScript DijkstraEra -> TxScript
fromDijkstraScript scr = mkDijkstraTxScript (Core.hashScript @DijkstraEra scr, scr)

fromDijkstraDatum :: Alonzo.Datum DijkstraEra -> TxOutDatum
fromDijkstraDatum bdat =
  case bdat of
    Alonzo.NoDatum -> NoDatum
    Alonzo.DatumHash hdh -> DatumHash hdh
    Alonzo.Datum binaryData ->
      let plutusData = Alonzo.binaryDataToData binaryData
       in InlineDatum $ mkTxData (Alonzo.hashData plutusData, plutusData)

-- | Dijkstra-specific script extraction. DijkstraNativeScript has no ToJSON,
-- so we store CBOR for native scripts instead of JSON.
getDijkstraScripts :: Core.Tx l DijkstraEra -> [TxScript]
getDijkstraScripts tx =
  mkDijkstraTxScript
    <$> ( Map.toList (tx ^. (Core.witsTxL . Core.scriptTxWitsL))
            ++ getAuxScripts (tx ^. Core.auxDataTxL)
        )
  where
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just auxData ->
          map (\scr -> (Core.hashScript @DijkstraEra scr, scr)) $
            toList $
              getAlonzoTxAuxDataScripts auxData

mkDijkstraTxScript :: (ScriptHash, Alonzo.AlonzoScript DijkstraEra) -> TxScript
mkDijkstraTxScript (hsh, script) =
  TxScript
    { txScriptHash = unScriptHash hsh
    , txScriptType = getScriptType
    , txScriptPlutusSize = getPlutusSize
    , txScriptJson = Nothing -- DijkstraNativeScript has no JSON serialization
    , txScriptCBOR = Just $ Core.originalBytes script
    }
  where
    getScriptType :: DB.ScriptType
    getScriptType =
      case script of
        Alonzo.NativeScript {} -> DB.Timelock
        Alonzo.PlutusScript ps -> getPlutusScriptType ps

    getPlutusSize :: Maybe Word64
    getPlutusSize =
      case script of
        Alonzo.NativeScript {} -> Nothing
        Alonzo.PlutusScript ps ->
          Just $ fromIntegral $ SBS.length $ Alonzo.unPlutusBinary $ Alonzo.plutusScriptBinary ps
