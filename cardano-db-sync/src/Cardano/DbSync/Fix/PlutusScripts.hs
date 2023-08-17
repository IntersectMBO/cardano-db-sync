{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Fix.PlutusScripts where

import Cardano.Prelude (mapMaybe)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Lens.Micro

import Cardano.Slotting.Slot (SlotNo (..))

import Cardano.Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Language as Ledger

import Cardano.Db (ScriptType (..), maybeToEither)
import qualified Cardano.Db.Old.V13_0 as DB_V_13_0

import Cardano.BM.Trace (Trace, logInfo, logWarning)

import Cardano.DbSync.Api
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Block
import Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo
import qualified Cardano.DbSync.Era.Shelley.Generic.Tx.Babbage as Babbage
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Types

import Database.Persist (Entity (..))
import Database.Persist.Sql (SqlBackend)

import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockAllegra, BlockAlonzo, BlockBabbage, BlockByron, BlockMary, BlockShelley))
import Ouroboros.Consensus.Shelley.Eras

import Cardano.DbSync.Fix.PlutusDataBytes

newtype FixPlutusScripts = FixPlutusScripts {scriptsInfo :: [FixPlutusInfo]}

nullPlutusScripts :: FixPlutusScripts -> Bool
nullPlutusScripts = null . scriptsInfo

sizeFixPlutusScripts :: FixPlutusScripts -> Int
sizeFixPlutusScripts = length . scriptsInfo

spanFPSOnNextPoint :: FixPlutusScripts -> Maybe (CardanoPoint, FixPlutusScripts, FixPlutusScripts)
spanFPSOnNextPoint fps = do
  point <- getNextPointList $ scriptsInfo fps
  Just $ spanFPSOnPoint fps point

spanFPSOnPoint :: FixPlutusScripts -> CardanoPoint -> (CardanoPoint, FixPlutusScripts, FixPlutusScripts)
spanFPSOnPoint fps point =
  (point, FixPlutusScripts atPoint, FixPlutusScripts rest)
  where
    (atPoint, rest) = span ((point ==) . fpPrevPoint) (scriptsInfo fps)

getWrongPlutusScripts ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  ReaderT SqlBackend m FixPlutusScripts
getWrongPlutusScripts tracer = do
  liftIO $
    logInfo tracer $
      mconcat
        [ "Starting the fixing Plutus Script procedure. This may take a couple minutes on mainnet if there are wrong values."
        , " You can skip it using --skip-plutus-script-fix."
        , " It will fix Script with wrong bytes. See more in Issue #1214 and #1348."
        , " This procedure makes resyncing unnecessary."
        ]
  FixPlutusScripts <$> findWrongPlutusScripts tracer

findWrongPlutusScripts ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  ReaderT SqlBackend m [FixPlutusInfo]
findWrongPlutusScripts tracer =
  findWrongPlutusData
    tracer
    "Script"
    DB_V_13_0.queryScriptCount
    DB_V_13_0.queryScriptPage
    (fmap f . DB_V_13_0.queryScriptInfo . entityKey)
    (DB_V_13_0.scriptHash . entityVal)
    (DB_V_13_0.scriptBytes . entityVal)
    (hashPlutusScript . entityVal)
  where
    f queryRes = do
      (prevBlockHsh, mPrevSlotNo) <- queryRes
      prevSlotNo <- mPrevSlotNo
      prevPoint <- convertToPoint (SlotNo prevSlotNo) prevBlockHsh
      Just prevPoint

    hashPlutusScript dbScript = do
      lang <- getLang
      bytes <- maybeToEither "No bytes found for plutus script" id $ DB_V_13_0.scriptBytes dbScript
      let script :: AlonzoScript StandardAlonzo = PlutusScript lang (SBS.toShort bytes)
      let hsh :: Ledger.ScriptHash StandardCrypto = Ledger.hashScript @StandardAlonzo script
      Right $ Generic.unScriptHash hsh
      where
        getLang = case DB_V_13_0.scriptType dbScript of
          PlutusV1 -> Right Ledger.PlutusV1
          PlutusV2 -> Right Ledger.PlutusV2
          _ -> Left "Non plutus script found where it shouldn't."

fixPlutusScripts :: MonadIO m => Trace IO Text -> CardanoBlock -> FixPlutusScripts -> ReaderT SqlBackend m ()
fixPlutusScripts tracer cblk fpss = do
  mapM_ fixData $ scriptsInfo fpss
  where
    fixData :: MonadIO m => FixPlutusInfo -> ReaderT SqlBackend m ()
    fixData fpi = do
      case Map.lookup (fpHash fpi) correctBytesMap of
        Nothing -> pure ()
        Just correctBytes -> do
          mScriptId <- DB_V_13_0.queryScript $ fpHash fpi
          case mScriptId of
            Just scriptId ->
              DB_V_13_0.updateScriptBytes scriptId correctBytes
            Nothing ->
              liftIO $
                logWarning tracer $
                  mconcat
                    ["Script", " not found in block"]

    correctBytesMap = scrapScriptBlock cblk

scrapScriptBlock :: CardanoBlock -> Map ByteString ByteString
scrapScriptBlock cblk = case cblk of
  BlockBabbage blk -> Map.unions $ scrapScriptTxBabbage . snd <$> getTxs blk
  BlockAlonzo blk -> Map.unions $ scrapScriptTxAlonzo . snd <$> getTxs blk
  BlockByron _ -> error "No Plutus Scripts in Byron"
  BlockShelley _ -> error "No Plutus Scripts in Shelley"
  BlockAllegra _ -> error "No Plutus Scripts in Allegra"
  BlockMary _ -> error "No Plutus Scripts in Mary"
  _ -> mempty -- This bug existed in a version that didn't support Conway or later eras

scrapScriptTxBabbage :: Ledger.Tx StandardBabbage -> Map ByteString ByteString
scrapScriptTxBabbage tx = Map.union txMap txOutMap
  where
    txMap = Map.fromList $ mapMaybe getTxScript $ getScripts tx
    txOutMap =
      Map.fromList $
        mapMaybe getOutputScript $
          toList $
            Babbage.outputs' $
              tx ^. Ledger.bodyTxL

    getOutputScript :: Ledger.TxOut StandardBabbage -> Maybe (ByteString, ByteString)
    getOutputScript txOut = do
      script :: AlonzoScript StandardBabbage <- strictMaybeToMaybe $ txOut ^. Babbage.referenceScriptTxOutL
      getTxScript $ Babbage.fromScript script

scrapScriptTxAlonzo :: Ledger.Tx StandardAlonzo -> Map ByteString ByteString
scrapScriptTxAlonzo tx = Map.fromList $ mapMaybe getTxScript $ getScripts tx

getTxScript :: Generic.TxScript -> Maybe (ByteString, ByteString)
getTxScript txScript =
  if txScriptType txScript `elem` [PlutusV1, PlutusV2]
    then do
      cbor <- txScriptCBOR txScript
      Just (txScriptHash txScript, cbor)
    else Nothing
