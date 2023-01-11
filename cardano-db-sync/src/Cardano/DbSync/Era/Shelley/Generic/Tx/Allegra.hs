{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (
  fromAllegraTx,
  getInterval,
  mkTxScript,
) where

import qualified Cardano.Api.Shelley as Api
import Cardano.Db (ScriptType (..))
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Era.Shelley.Generic.Witness
import qualified Cardano.Ledger.Allegra as Allegra
import Cardano.Ledger.BaseTypes (StrictMaybe, strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import Cardano.Ledger.ShelleyMA.AuxiliaryData (MAAuxiliaryData (AuxiliaryData'))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardCrypto)

fromAllegraTx :: (Word64, Core.Tx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txBlockIndex = blkIndex
    , txSize = getTxSize tx
    , txValidContract = True
    , txInputs = mkTxIn txBody
    , txCollateralInputs = [] -- Allegra does not have collateral inputs
    , txReferenceInputs = [] -- Allegra does not have reference inputs
    , txOutputs = outputs
    , txCollateralOutputs = [] -- Allegra does not have collateral outputs
    , txFees = Just $ ShelleyMa.txfee' txBody
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = invalidBefore
    , txInvalidHereafter = invalidAfter
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromAllegraMetadata <$> getTxMetadata tx
    , txCertificates = mkTxCertificates txBody
    , txWithdrawals = mkTxWithdrawals txBody
    , txParamProposal = mkTxParamProposal (Allegra Standard) txBody
    , txMint = mempty -- Allegra does not support Multi-Assets
    , txRedeemer = [] -- Allegra does not support redeemers
    , txData = []
    , txScriptSizes = [] -- Allegra does not support plutus scripts
    , txScripts = scripts
    , txExtraKeyWitnesses = []
    }
  where
    txBody :: Core.TxBody StandardAllegra
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = mkTxOut txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> ( Map.toList (tx ^. (Core.witsTxL . Core.scriptWitsL))
                ++ getAuxScripts (tx ^. Core.auxDataTxL)
            )

    (invalidBefore, invalidAfter) = getInterval $ ShelleyMa.vldt' txBody

getAuxScripts ::
  StrictMaybe (MAAuxiliaryData (Allegra.AllegraEra StandardCrypto)) ->
  [(ScriptHash StandardCrypto, Timelock StandardCrypto)]
getAuxScripts maux =
  case strictMaybeToMaybe maux of
    Nothing -> []
    Just (AuxiliaryData' _ scrs) ->
      map (\scr -> (Core.hashScript @(Allegra.AllegraEra StandardCrypto) scr, scr)) $ toList scrs

mkTxScript :: (ScriptHash StandardCrypto, Timelock StandardCrypto) -> TxScript
mkTxScript (hsh, script) =
  TxScript
    { txScriptHash = unScriptHash hsh
    , txScriptType = Timelock
    , txScriptPlutusSize = Nothing
    , txScriptJson =
        Just . LBS.toStrict . Aeson.encode $
          Api.fromAllegraTimelock Api.TimeLocksInSimpleScriptV2 script
    , txScriptCBOR = Nothing
    }

getInterval ::
  ShelleyMa.ValidityInterval -> (Maybe SlotNo, Maybe SlotNo)
getInterval interval =
  ( strictMaybeToMaybe $ ShelleyMa.invalidBefore interval
  , strictMaybeToMaybe $ ShelleyMa.invalidHereafter interval
  )
