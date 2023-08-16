{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Mary (
  fromMaryTx,
) where

import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval, getScripts)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Witness
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary.TxBody
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (StandardMary)

fromMaryTx :: (Word64, Core.Tx StandardMary) -> Tx
fromMaryTx (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txBlockIndex = blkIndex
    , txSize = getTxSize tx
    , txValidContract = True
    , txInputs = mkTxIn txBody
    , txCollateralInputs = [] -- Mary does not have collateral inputs
    , txReferenceInputs = [] -- Mary does not have reference inputs
    , txOutputs = outputs
    , txCollateralOutputs = [] -- Mary does not have collateral outputs
    , txFees = Just $ txBody ^. Core.feeTxBodyL
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = invBefore
    , txInvalidHereafter = invAfter
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromMaryMetadata <$> getTxMetadata tx
    , txCertificates = mkTxCertificates txBody
    , txWithdrawals = mkTxWithdrawals txBody
    , txParamProposal = mkTxParamProposal (Mary Standard) txBody
    , txMint = txBody ^. mintTxBodyL
    , txRedeemer = [] -- Mary does not support redeemers
    , txData = []
    , txScriptSizes = [] -- Mary does not support plutus scripts
    , txScripts = getScripts tx
    , txExtraKeyWitnesses = []
    , txVotingProcedure = []
    , txProposalProcedure = []
    }
  where
    txBody :: Core.TxBody StandardMary
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)

    fromTxOut :: Word64 -> ShelleyTx.ShelleyTxOut StandardMary -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = txOut ^. Core.addrTxOutL
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutScript = Nothing
        , txOutDatum = NoDatum -- Mary does not support plutus data
        }
      where
        bs = Ledger.unCompactAddr $ txOut ^. Core.compactAddrTxOutL
        MaryValue ada (MultiAsset maMap) = txOut ^. Core.valueTxOutL

    (invBefore, invAfter) = getInterval txBody
