{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (
  fromAllegraTx,
  getInterval,
  getScripts,
) where

import Cardano.Db (ScriptType (..))
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Script (fromTimelock)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (
  calcWithdrawalSum,
  getTxMetadata,
  getTxSize,
  mkTxCertificates,
  mkTxIn,
  mkTxOut,
  mkTxParamProposal,
  mkTxWithdrawals,
  txHashId,
 )
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Era.Shelley.Generic.Witness
import Cardano.Ledger.Allegra.Core hiding (Tx, TxOut)
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.BaseTypes (StrictMaybe, strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
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
    , txFees = Just $ txBody ^. Core.feeTxBodyL
    , txOutSum = sumTxOutCoin outputs
    , txInvalidBefore = invBefore
    , txInvalidHereafter = invAfter
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
    , txVotingProcedure = []
    , txProposalProcedure = []
    }
  where
    txBody :: Core.TxBody StandardAllegra
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = mkTxOut txBody

    scripts :: [TxScript]
    scripts = getScripts tx

    (invBefore, invAfter) = getInterval txBody

getScripts ::
  forall era.
  (EraCrypto era ~ StandardCrypto, Core.Tx era ~ ShelleyTx era, TxAuxData era ~ AllegraTxAuxData era, Script era ~ Timelock era, EraTx era) =>
  ShelleyTx era ->
  [TxScript]
getScripts tx =
  mkTxScript
    <$> ( Map.toList (tx ^. (Core.witsTxL . Core.scriptTxWitsL))
            ++ getAuxScripts (tx ^. Core.auxDataTxL)
        )

getAuxScripts ::
  forall era.
  (EraCrypto era ~ StandardCrypto, EraScript era, Script era ~ Timelock era) =>
  StrictMaybe (AllegraTxAuxData era) ->
  [(ScriptHash StandardCrypto, Timelock era)]
getAuxScripts maux =
  case strictMaybeToMaybe maux of
    Nothing -> []
    Just (AllegraTxAuxData _ scrs) ->
      map (\scr -> (Core.hashScript @era scr, scr)) $ toList scrs

mkTxScript ::
  (Era era) =>
  (ScriptHash StandardCrypto, Timelock era) ->
  TxScript
mkTxScript (hsh, script) =
  TxScript
    { txScriptHash = unScriptHash hsh
    , txScriptType = Timelock
    , txScriptPlutusSize = Nothing
    , txScriptJson =
        Just . LBS.toStrict . Aeson.encode $
          fromTimelock script
    , txScriptCBOR = Nothing
    }

getInterval ::
  AllegraEraTxBody era =>
  TxBody era ->
  (Maybe SlotNo, Maybe SlotNo)
getInterval txBody =
  ( strictMaybeToMaybe $ invalidBefore interval
  , strictMaybeToMaybe $ invalidHereafter interval
  )
  where
    interval = txBody ^. vldtTxBodyL
