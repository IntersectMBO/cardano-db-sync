{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Mary (
  fromMaryTx,
) where

import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval, mkTxScript)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Witness
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Mary as Mary
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import Cardano.Ledger.ShelleyMA.AuxiliaryData (MAAuxiliaryData (AuxiliaryData'))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa
import Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardMary)

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
    , txFees = Just $ ShelleyMa.txfee' txBody
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = invalidBefore
    , txInvalidHereafter = invalidAfter
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromMaryMetadata <$> getTxMetadata tx
    , txCertificates = mkTxCertificates txBody
    , txWithdrawals = mkTxWithdrawals txBody
    , txParamProposal = mkTxParamProposal (Mary Standard) txBody
    , txMint = ShelleyMa.mint' txBody
    , txRedeemer = [] -- Mary does not support redeemers
    , txData = []
    , txScriptSizes = [] -- Mary does not support plutus scripts
    , txScripts = scripts
    , txExtraKeyWitnesses = []
    }
  where
    txBody :: Core.TxBody StandardMary
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 ..] $ toList (ShelleyMa.outputs' txBody)

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
        Ledger.UnsafeCompactAddr bs = txOut ^. Core.compactAddrTxOutL
        MaryValue ada maMap = txOut ^. Core.valueTxOutL

    scripts :: [TxScript]
    scripts =
      mkTxScript -- Mary and Allegra have the same kind of scripts
        <$> ( Map.toList (tx ^. (Core.witsTxL . Core.scriptWitsL))
                ++ getAuxScripts (tx ^. Core.auxDataTxL)
            )

    (invalidBefore, invalidAfter) = getInterval $ ShelleyMa.vldt' txBody

getAuxScripts ::
  StrictMaybe (MAAuxiliaryData (Mary.MaryEra StandardCrypto)) ->
  [(ScriptHash StandardCrypto, Timelock StandardCrypto)]
getAuxScripts maux =
  case strictMaybeToMaybe maux of
    Nothing -> []
    Just (AuxiliaryData' _ scrs) ->
      map (\scr -> (Core.hashScript @(Mary.MaryEra StandardCrypto) scr, scr)) $ toList scrs
