{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Mary where

import           Cardano.Prelude

import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Mary as Mary
import           Cardano.Ledger.Mary.Value (Value (..))
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Data.MemoBytes (MemoBytes (..))

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardMary)

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval, mkTxScript)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (fromTxIn, getWithdrawalSum,
                   mkTxCertificate, mkTxWithdrawal, txHashId)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Witness

fromMaryTx :: (Word64, ShelleyTx.Tx StandardMary) -> Tx
fromMaryTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map fromTxIn (toList . ShelleyMa.inputs $ unTxBodyRaw tx)
      , txCollateralInputs = []  -- Mary does not have collateral inputs
      , txReferenceInputs = []   -- Mary does not have reference inputs
      , txOutputs = outputs
      , txCollateralOutputs = [] -- Mary does not have collateral outputs
      , txFees = Just $ ShelleyMa.txfee (unTxBodyRaw tx)
      , txOutSum = sumOutputs outputs
      , txInvalidBefore = invalidBefore
      , txInvalidHereafter = invalidAfter
      , txWithdrawalSum = getWithdrawalSum $ ShelleyMa.wdrls (unTxBodyRaw tx)
      , txMetadata = fromMaryMetadata <$> txMeta tx
      , txCertificates = zipWith mkTxCertificate [0..] (toList . ShelleyMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . ShelleyMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = maybe [] (convertParamProposal (Mary Standard)) $ strictMaybeToMaybe (ShelleyMa.update $ unTxBodyRaw tx)
      , txMint = ShelleyMa.mint $ unTxBodyRaw tx
      , txRedeemer = []       -- Mary does not support redeemers
      , txData = []
      , txScriptSizes = []    -- Mary does not support plutus scripts
      , txScripts = scripts
      , txScriptsFee = Coin 0 -- Mary does not support plutus scripts
      , txExtraKeyWitnesses = []
      }
  where
    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs $ unTxBodyRaw tx)

    fromTxOut :: Word16 -> ShelleyTx.TxOut StandardMary -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = addr
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutScript = Nothing
        , txOutDatum = NoDatum -- Mary does not support plutus data
        }
      where
        Ledger.UnsafeCompactAddr bs = Ledger.getTxOutCompactAddr txOut

        -- This pattern match also does the deserialisation of the address
        ShelleyTx.TxOut addr (Value ada maMap) = txOut

    txMeta :: ShelleyTx.Tx StandardMary -> Maybe (ShelleyMa.AuxiliaryData StandardMary)
    txMeta (ShelleyTx.Tx _body _wit md) = strictMaybeToMaybe md

    unTxBodyRaw :: ShelleyTx.Tx StandardMary -> ShelleyMa.TxBodyRaw StandardMary
    unTxBodyRaw (ShelleyTx.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = memotype txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript -- Mary and Allegra have the same kind of scripts
        <$> (Map.toList (ShelleyTx.scriptWits $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))

    getAuxScripts
        :: ShelleyMa.StrictMaybe (ShelleyMa.AuxiliaryData StandardMary)
        -> [(ScriptHash StandardCrypto, Mary.Script StandardMary)]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (ShelleyMa.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @StandardMary scr, scr)) $ toList scrs

    (invalidBefore, invalidAfter) = getInterval $ getField @"body" tx
