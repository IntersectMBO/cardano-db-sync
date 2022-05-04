{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Ledger.Allegra as Allegra
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import           Data.MemoBytes (MemoBytes (..))

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardCrypto)

import qualified Cardano.Api.Shelley as Api

import           Cardano.Db (ScriptType (..))

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Witness

fromAllegraTx :: (Word64, Shelley.Tx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map fromTxIn (toList $ ShelleyMa.inputs rawTxBody)
      , txCollateralInputs = []  -- Allegra does not have collateral inputs
      , txReferenceInputs = []   -- Allegra does not have reference inputs
      , txOutputs = outputs
      , txCollateralOutputs = [] -- Allegra does not have collateral outputs
      , txFees = ShelleyMa.txfee rawTxBody
      , txOutSum = sumOutputs outputs
      , txInvalidBefore = invalidBefore
      , txInvalidHereafter = invalidAfter
      , txWithdrawalSum = getWithdrawalSum $ ShelleyMa.wdrls rawTxBody
      , txMetadata = fromAllegraMetadata <$> txMeta tx
      , txCertificates = zipWith mkTxCertificate [0..] (toList $ ShelleyMa.certs rawTxBody)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl $ ShelleyMa.wdrls rawTxBody)
      , txParamProposal = maybe [] (convertParamProposal (Allegra Standard)) $ strictMaybeToMaybe (ShelleyMa.update rawTxBody)
      , txMint = mempty       -- Allegra does not support Multi-Assets
      , txRedeemer = []       -- Allegra does not support redeemers
      , txData = []
      , txScriptSizes = []    -- Allegra does not support plutus scripts
      , txScripts = scripts
      , txScriptsFee = Coin 0 -- Allegra does not support plutus scripts
      , txExtraKeyWitnesses = []
      }
  where
    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs rawTxBody)

    txMeta :: Shelley.Tx StandardAllegra -> Maybe (ShelleyMa.AuxiliaryData StandardAllegra)
    txMeta (Shelley.Tx _body _wit md) = strictMaybeToMaybe md

    rawTxBody :: ShelleyMa.TxBodyRaw StandardAllegra
    rawTxBody =
      case tx of
        (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) -> memotype txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> (Map.toList (Shelley.scriptWits $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))

    (invalidBefore, invalidAfter) = getInterval $ getField @"body" tx

getAuxScripts
    :: ShelleyMa.StrictMaybe (Allegra.AuxiliaryData StandardAllegra)
    -> [(ScriptHash StandardCrypto, Allegra.Script StandardAllegra)]
getAuxScripts maux =
  case strictMaybeToMaybe maux of
    Nothing -> []
    Just (ShelleyMa.AuxiliaryData _ scrs) ->
      map (\scr -> (Ledger.hashScript @StandardAllegra scr, scr)) $ toList scrs

mkTxScript :: (ScriptHash StandardCrypto, Allegra.Script StandardAllegra) -> TxScript
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
    :: HasField "vldt" (Core.TxBody era) ShelleyMa.ValidityInterval
    => Core.TxBody era -> (Maybe SlotNo, Maybe SlotNo)
getInterval txBody =
    ( strictMaybeToMaybe $ ShelleyMa.invalidBefore interval
    , strictMaybeToMaybe $ ShelleyMa.invalidHereafter interval
    )
  where
    interval = getField @"vldt" txBody
