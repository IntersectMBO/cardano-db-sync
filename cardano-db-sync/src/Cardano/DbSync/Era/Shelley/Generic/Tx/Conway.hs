{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Conway (
  fromConwayTx,
) where

import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo
import qualified Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage as Babbage
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Babbage.Core as Core hiding (Tx, TxOut)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.TxBody
import qualified Cardano.Ledger.Core as Core
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (StandardConway)

fromConwayTx :: Bool -> Maybe Alonzo.Prices -> (Word64, Core.Tx StandardConway) -> Tx
fromConwayTx ioExtraPlutus mprices (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txBlockIndex = blkIndex
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
    , txInvalidBefore = invalidBefore
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
    , txScripts = getScripts tx
    , txExtraKeyWitnesses = extraKeyWits txBody
    , txVotingProcedure = toList $ ctbVotingProcedures txBody
    , txProposalProcedure = toList $ ctbProposalProcedures txBody
    }
  where
    txBody :: Core.TxBody StandardConway
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith Babbage.fromTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)

    -- TODO when collateral output is used as output, its index is not 0, but length of outputs
    -- even though it is the unique output of the tx.
    collOutputs :: [TxOut]
    collOutputs = zipWith Babbage.fromTxOut [collIndex ..] . toList $ (txBody ^. collateralReturnTxBodyL)

    collIndex :: Word64
    collIndex =
      case txIxFromIntegral (length outputs) of
        Just (TxIx i) -> i
        Nothing -> fromIntegral (maxBound :: Word16)

    -- This is true if second stage contract validation passes.
    isValid2 :: Bool
    isValid2 =
      case Alonzo.isValid tx of
        Alonzo.IsValid x -> x

    (finalMaps, redeemers) = resolveRedeemers ioExtraPlutus mprices tx Right
    (invalidBefore, invalidAfter) = getInterval txBody

    collInputs = mkCollTxIn txBody
