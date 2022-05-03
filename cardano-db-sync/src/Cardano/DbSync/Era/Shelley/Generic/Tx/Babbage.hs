{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage where

import           Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Ledger.Alonzo.Scripts (txscriptfee)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Mary.Value (Value (..))
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Shelley.Eras (StandardBabbage)

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (fromTxIn)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Witness


fromTxOut :: Word16 -> Babbage.TxOut StandardBabbage -> TxOut
fromTxOut index txOut =
    TxOut
      { txOutIndex = index
      , txOutAddress = addr
      , txOutAddressRaw = SBS.fromShort caddr
      , txOutAdaValue = Coin ada
      , txOutMaValue = maMap
      , txOutDataHash = Nothing -- TODO getDataHash <$> strictMaybeToMaybe undefined
      }
  where
    Ledger.UnsafeCompactAddr caddr = Ledger.getTxOutCompactAddr txOut

    -- This pattern match also does the deserialisation of the address
    Babbage.TxOut addr (Value ada maMap) _datum _mScript = txOut

fromBabbageTx :: Alonzo.Prices -> (Word64, Ledger.Tx StandardBabbage) -> Tx
fromBabbageTx prices (blkIndex, tx) =
    Tx
      { txHash = Crypto.hashToBytes . Ledger.extractHash $ Ledger.hashAnnotated txBody
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = isValid2
      , txInputs =
          if not isValid2
            then map fromTxIn . toList $ getField @"collateral" txBody
            else Map.elems $ rmInps finalMaps
      , txCollateralInputs = map fromTxIn . toList $ getField @"collateral" txBody
      , txOutputs =
          if not isValid2
            then [] -- TODO
            else zipWith fromTxOut [0 .. ] . toList $ getField @"outputs" txBody
      , txFees =
          if not isValid2
            then getField @"txfee" txBody -- TODO
            else getField @"txfee" txBody
      , txOutSum =
          if not isValid2
            then Coin 0
            else sumOutputs outputs
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ getField @"vldt" txBody
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ getField @"vldt" txBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ getField @"wdrls" txBody
      , txMetadata = fromAlonzoMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = snd <$> rmCerts finalMaps
      , txWithdrawals = Map.elems $ rmWdrl finalMaps
      , txParamProposal = maybe [] (convertParamProposal (Babbage Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = getField @"mint" txBody
      , txRedeemer = redeemers
      , txData = txData'
      , txScriptSizes = getPlutusSizes tx
      , txScripts = getScripts tx
      , txScriptsFee = minFees
      , txExtraKeyWitnesses = extraKeyWits txBody
      }
  where

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList $ getField @"outputs" txBody

    txBody :: Ledger.TxBody StandardBabbage
    txBody = getField @"body" tx

    minFees :: Coin
    minFees = txscriptfee prices $ Alonzo.totExUnits tx

    -- This is true if second stage contract validation passes.
    isValid2 :: Bool
    isValid2 =
      case Alonzo.isValid tx of
        Alonzo.IsValid x -> x

    txData' = txDataWitness tx
    (finalMaps, redeemers) = resolveRedeemers prices tx
