{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Mary
  ( fromMaryTx
  ) where

import           Cardano.Prelude

import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Mary as Mary
import           Cardano.Ledger.Mary.Value (MaryValue (..))
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Lens.Micro

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardMary)

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval, mkTxScript)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (calcWithdrawalSum, fromTxIn,
                   mkTxCertificate, mkTxWithdrawal, txHashId)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Witness

fromMaryTx :: (Word64, ShelleyTx.ShelleyTx StandardMary) -> Tx
fromMaryTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ tx ^. Core.sizeTxF
      , txValidContract = True
      , txInputs = map fromTxIn (toList $ ShelleyMa.inputs' txBody)
      , txCollateralInputs = []  -- Mary does not have collateral inputs
      , txReferenceInputs = []   -- Mary does not have reference inputs
      , txOutputs = outputs
      , txCollateralOutputs = [] -- Mary does not have collateral outputs
      , txFees = Just $ ShelleyMa.txfee' txBody
      , txOutSum = sumTxOutCoin outputs
      , txInvalidBefore = invalidBefore
      , txInvalidHereafter = invalidAfter
      , txWithdrawalSum = calcWithdrawalSum $ ShelleyMa.wdrls' txBody
      , txMetadata = fromMaryMetadata <$> strictMaybeToMaybe (tx ^. Core.auxDataTxL)
      , txCertificates = zipWith mkTxCertificate [0..] (toList $ ShelleyMa.certs' txBody)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl $ ShelleyMa.wdrls' txBody)
      , txParamProposal = maybe [] (convertParamProposal (Mary Standard)) $ strictMaybeToMaybe (ShelleyMa.update' txBody)
      , txMint = ShelleyMa.mint' txBody
      , txRedeemer = []       -- Mary does not support redeemers
      , txData = []
      , txScriptSizes = []    -- Mary does not support plutus scripts
      , txScripts = scripts
      , txExtraKeyWitnesses = []
      }
  where
    txBody :: ShelleyMa.MATxBody StandardMary
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs' txBody)

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
        <$> (Map.toList (tx ^. (Core.witsTxL . Core.scriptWitsL))
            ++ getAuxScripts (tx ^. Core.auxDataTxL))

    (invalidBefore, invalidAfter) = getInterval $ ShelleyMa.vldt' txBody

getAuxScripts
    :: ShelleyMa.StrictMaybe (ShelleyMa.MAAuxiliaryData (Mary.MaryEra StandardCrypto))
    -> [(ScriptHash StandardCrypto, ShelleyMa.Timelock StandardCrypto)]
getAuxScripts maux =
  case strictMaybeToMaybe maux of
    Nothing -> []
    Just (ShelleyMa.AuxiliaryData' _ scrs) ->
      map (\scr -> (Core.hashScript @(Mary.MaryEra StandardCrypto) scr, scr)) $ toList scrs
