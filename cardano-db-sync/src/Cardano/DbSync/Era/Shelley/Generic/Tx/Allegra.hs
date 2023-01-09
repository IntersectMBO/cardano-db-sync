{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra
  ( fromAllegraTx
  , getInterval
  , mkTxScript
  ) where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Ledger.Allegra as Allegra
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMa

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import           Lens.Micro

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardCrypto)

import qualified Cardano.Api.Shelley as Api

import           Cardano.Db (ScriptType (..))

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Witness

fromAllegraTx :: (Word64, Shelley.ShelleyTx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ tx ^. Core.sizeTxF
      , txValidContract = True
      , txInputs = map fromTxIn (toList $ ShelleyMa.inputs' txBody)
      , txCollateralInputs = []  -- Allegra does not have collateral inputs
      , txReferenceInputs = []   -- Allegra does not have reference inputs
      , txOutputs = outputs
      , txCollateralOutputs = [] -- Allegra does not have collateral outputs
      , txFees = Just $ ShelleyMa.txfee' txBody
      , txOutSum = sumTxOutCoin outputs
      , txInvalidBefore = invalidBefore
      , txInvalidHereafter = invalidAfter
      , txWithdrawalSum = calcWithdrawalSum $ ShelleyMa.wdrls' txBody
      , txMetadata = fromAllegraMetadata <$> strictMaybeToMaybe (tx ^. Core.auxDataTxL)
      , txCertificates = zipWith mkTxCertificate [0..] (toList $ ShelleyMa.certs' txBody)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl $ ShelleyMa.wdrls' txBody)
      , txParamProposal = maybe [] (convertParamProposal (Allegra Standard)) $ strictMaybeToMaybe (ShelleyMa.update' txBody)
      , txMint = mempty       -- Allegra does not support Multi-Assets
      , txRedeemer = []       -- Allegra does not support redeemers
      , txData = []
      , txScriptSizes = []    -- Allegra does not support plutus scripts
      , txScripts = scripts
      , txExtraKeyWitnesses = []
      }
  where
    txBody :: ShelleyMa.MATxBody StandardAllegra
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs' txBody)

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> (Map.toList (tx ^. (Core.witsTxL . Core.scriptWitsL))
            ++ getAuxScripts (tx ^. Core.auxDataTxL))

    (invalidBefore, invalidAfter) = getInterval $ ShelleyMa.vldt' txBody

getAuxScripts
    :: ShelleyMa.StrictMaybe (ShelleyMa.MAAuxiliaryData (Allegra.AllegraEra StandardCrypto))
    -> [(ScriptHash StandardCrypto, ShelleyMa.Timelock StandardCrypto)]
getAuxScripts maux =
  case strictMaybeToMaybe maux of
    Nothing -> []
    Just (ShelleyMa.AuxiliaryData' _ scrs) ->
      map (\scr -> (Core.hashScript @(Allegra.AllegraEra StandardCrypto) scr, scr)) $ toList scrs

mkTxScript :: (ScriptHash StandardCrypto, ShelleyMa.Timelock StandardCrypto) -> TxScript
mkTxScript (hsh, script) = TxScript
    { txScriptHash = unScriptHash hsh
    , txScriptType = Timelock
    , txScriptPlutusSize = Nothing
    , txScriptJson =
        Just . LBS.toStrict . Aeson.encode
        $ Api.fromAllegraTimelock Api.TimeLocksInSimpleScriptV2 script
    , txScriptCBOR = Nothing
    }

getInterval
    :: ShelleyMa.ValidityInterval -> (Maybe SlotNo, Maybe SlotNo)
getInterval interval =
    ( strictMaybeToMaybe $ ShelleyMa.invalidBefore interval
    , strictMaybeToMaybe $ ShelleyMa.invalidHereafter interval
    )
