{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage (
  fromBabbageTx,
) where

import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Witness
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut)
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Lens.Micro
import Ouroboros.Consensus.Shelley.Eras (StandardBabbage, StandardCrypto)

fromBabbageTx :: Maybe Alonzo.Prices -> (Word64, Core.Tx StandardBabbage) -> Tx
fromBabbageTx mprices (blkIndex, tx) =
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
    , txReferenceInputs = map fromTxIn . toList $ Babbage.referenceInputs' txBody
    , txOutputs =
        if not isValid2
          then collOutputs
          else outputs
    , txCollateralOutputs =
        collOutputs
    , txFees =
        if not isValid2
          then strictMaybeToMaybe $ Babbage.totalCollateral' txBody
          else Just $ Babbage.txfee' txBody
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
    , txParamProposal = mkTxParamProposal (Babbage Standard) txBody
    , txMint = Babbage.mint' txBody
    , txRedeemer = redeemers
    , txData = txDataWitness tx
    , txScriptSizes = getPlutusSizes tx
    , txScripts = getScripts tx
    , txExtraKeyWitnesses = extraKeyWits txBody
    }
  where
    txBody :: Core.TxBody StandardBabbage
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 ..] $ toList (Babbage.outputs' txBody)

    fromTxOut :: Word64 -> BabbageTxOut StandardBabbage -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = txOut ^. Core.addrTxOutL
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutScript = fromScript <$> strictMaybeToMaybe mScript
        , txOutDatum = fromDatum datum
        }
      where
        Ledger.UnsafeCompactAddr bs = txOut ^. Core.compactAddrTxOutL
        MaryValue ada maMap = txOut ^. Core.valueTxOutL
        datum = txOut ^. Babbage.datumTxOutL
        mScript = txOut ^. Babbage.referenceScriptTxOutL

    -- TODO when collateral output is used as output, its index is not 0, but length of outputs
    -- even though it is the unique output of the tx.
    collOutputs :: [TxOut]
    collOutputs = zipWith fromTxOut [collIndex ..] . toList $ Babbage.collateralReturn' txBody

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

    (finalMaps, redeemers) = resolveRedeemers mprices tx
    (invalidBefore, invalidAfter) = getInterval $ Babbage.vldt' txBody

    collInputs = mkCollTxIn txBody

fromScript ::
  forall era.
  ( Ledger.Crypto era ~ StandardCrypto
  , Core.Script era ~ Alonzo.AlonzoScript era
  , Core.EraScript era
  ) =>
  Alonzo.AlonzoScript era ->
  TxScript
fromScript scr = mkTxScript (Core.hashScript @era scr, scr)

fromDatum :: (Ledger.Crypto era ~ StandardCrypto, Ledger.Era era) => Babbage.Datum era -> TxOutDatum
fromDatum bdat =
  case bdat of
    Babbage.NoDatum -> NoDatum
    Babbage.DatumHash hdh -> DatumHash hdh
    Babbage.Datum binaryData ->
      let plutusData = Alonzo.binaryDataToData binaryData
       in InlineDatum $ mkTxData (Alonzo.hashData plutusData, plutusData)
