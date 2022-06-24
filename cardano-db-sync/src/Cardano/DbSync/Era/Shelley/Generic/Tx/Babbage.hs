{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage
  ( fromBabbageTx
  ) where

import           Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
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

import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Shelley.Eras (StandardBabbage, StandardCrypto)

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (calcWithdrawalSum, fromTxIn)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Witness

fromBabbageTx :: Maybe Alonzo.Prices -> (Word64, Ledger.Tx StandardBabbage) -> Tx
fromBabbageTx mprices (blkIndex, tx) =
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
      , txReferenceInputs = map fromTxIn . toList $ getField @"referenceInputs" txBody
      , txOutputs =
          if not isValid2
            then collOutputs
            else outputs
      , txCollateralOutputs =
          collOutputs
      , txFees =
          if not isValid2
            then Just $ fromMaybe (Coin 0) (strictMaybeToMaybe $ getField @"totalCollateral" txBody)
            else Just $ getField @"txfee" txBody
      , txOutSum =
          if not isValid2
            then sumTxOutCoin collOutputs
            else sumTxOutCoin outputs
      , txInvalidBefore = invalidBefore
      , txInvalidHereafter = invalidAfter
      , txWithdrawalSum = calcWithdrawalSum $ getField @"wdrls" txBody
      , txMetadata = fromAlonzoMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = snd <$> rmCerts finalMaps
      , txWithdrawals = Map.elems $ rmWdrl finalMaps
      , txParamProposal = maybe [] (convertParamProposal (Babbage Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = getField @"mint" txBody
      , txRedeemer = redeemers
      , txData = txDataWitness tx
      , txScriptSizes = getPlutusSizes tx
      , txScripts = getScripts tx
      , txExtraKeyWitnesses = extraKeyWits txBody
      }
  where
    fromTxOut :: Word64 -> Babbage.TxOut StandardBabbage -> TxOut
    fromTxOut index txOut =
        TxOut
          { txOutIndex = index
          , txOutAddress = addr
          , txOutAddressRaw = SBS.fromShort caddr
          , txOutAdaValue = Coin ada
          , txOutMaValue = maMap
          , txOutScript = fromScript <$> strictMaybeToMaybe mScript
          , txOutDatum = fromDatum datum
          }
      where
        Ledger.UnsafeCompactAddr caddr = Ledger.getTxOutCompactAddr txOut

        -- This pattern match also does the deserialisation of the address
        Babbage.TxOut addr (Value ada maMap) datum mScript = txOut

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList $ getField @"outputs" txBody

    -- TODO when collateral output is used as output, its index is not 0, but length of outputs
    -- even though it is the unique output of the tx.
    collOutputs :: [TxOut]
    collOutputs = zipWith fromTxOut [collIndex .. ] . toList $ getField @"collateralReturn" txBody
      where
        collIndex = case txIxFromIntegral (length outputs) of
          Just (TxIx i) -> i
          Nothing -> fromIntegral (maxBound :: Word16)

    txBody :: Ledger.TxBody StandardBabbage
    txBody = getField @"body" tx

    -- This is true if second stage contract validation passes.
    isValid2 :: Bool
    isValid2 =
      case Alonzo.isValid tx of
        Alonzo.IsValid x -> x

    (finalMaps, redeemers) = resolveRedeemers mprices tx
    (invalidBefore, invalidAfter) = getInterval txBody


fromScript
    :: forall era.
    ( Ledger.Crypto era ~ StandardCrypto
    , Ledger.Script era ~ Alonzo.Script era
    , Ledger.ValidateScript era)
    => Ledger.Script era -> TxScript
fromScript scr = mkTxScript (Ledger.hashScript @era scr, scr)

fromDatum :: (Ledger.Crypto era ~ StandardCrypto, Ledger.Era era) => Babbage.Datum era -> TxOutDatum
fromDatum bdat =
    case bdat of
      Babbage.NoDatum -> NoDatum
      Babbage.DatumHash hdh -> DatumHash $ dataHashToBytes hdh
      Babbage.Datum binaryData ->
        let plutusData = Alonzo.binaryDataToData binaryData
         in InlineDatum $ mkTxData (Alonzo.hashData plutusData, plutusData)
