{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.DbSync.Fix.PlutusDataBytes where

import           Cardano.Prelude (mapMaybe)

import           Control.Monad.Except
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Extra (mapMaybeM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Foldable (toList)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word64)
import           GHC.Records (HasField(getField))

import           Cardano.Slotting.Slot (SlotNo(..))

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger

import qualified Cardano.Db as DB

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo
import           Cardano.DbSync.Types
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Block
import           Cardano.DbSync.Api
import           Cardano.DbSync.Error (bsBase16Encode)

import           Database.Persist (Entity(..))
import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage, BlockAlonzo, BlockShelley, BlockAllegra, BlockByron, BlockMary))

data FixData = FixData
  { fdDatum :: [FixPlutusData]
  , fdRedeemerData :: [FixPlutusData]
  }

data FixPlutusData = FixPlutusData
  { fpdHash :: ByteString
  , fpdPrevPoint :: CardanoPoint
  } deriving Show

sizeFixData :: FixData -> Int
sizeFixData fd = length (fdDatum fd) + length (fdRedeemerData fd)

spanOnNextPoint :: FixData -> Maybe (CardanoPoint, FixData, FixData)
spanOnNextPoint fd = case (getNextPointList (fdDatum fd), getNextPointList (fdRedeemerData fd)) of
    (Nothing, Nothing) -> Nothing
    (Just p, Nothing) -> Just $ spanOnPoint fd p
    (Nothing, Just p) -> Just $ spanOnPoint fd p
    (Just p, Just p') -> Just $ spanOnPoint fd (min p p')

spanOnPoint :: FixData -> CardanoPoint -> (CardanoPoint, FixData, FixData)
spanOnPoint fd point =
    (point, FixData datum rdmData, FixData datumRest rdmDataRest)
  where
    (datum, datumRest) = span ((point ==) . fpdPrevPoint) (fdDatum fd)
    (rdmData, rdmDataRest) = span ((point ==) . fpdPrevPoint) (fdRedeemerData fd)

getNextPointList :: [FixPlutusData] -> Maybe CardanoPoint
getNextPointList fds = case fds of
    [] -> Nothing
    fd : _ -> Just $ fpdPrevPoint fd

getWrongPlutusData :: SyncEnv -> IO FixData
getWrongPlutusData env = do
    dbBackend <- getBackend env
    liftIO $ logInfo (getTrace env) $ mconcat
      [ "Starting the fixing Plutus Data bytes procedure. This may take a couple hours on mainnet if there are wrong values."
      , " You can skip it using --skip-plutus-data-fix."
      , " It will fix Datum and RedeemerData with wrong bytes. See more in Issue #1214 and #1278."
      , " This procedure makes resyncing unnecessary."
      ]
    datumList <- DB.runDbIohkNoLogging dbBackend $
      findWrongPlutusData
        (getTrace env) "Datum"
        DB.queryDatumCount DB.queryDatumPage (fmap f . DB.querydatumInfo . entityKey)
        (DB.datumHash . entityVal) (DB.datumBytes . entityVal)
    redeemerDataList <- DB.runDbIohkNoLogging dbBackend $
      findWrongPlutusData
        (getTrace env) "RedeemerData"
        DB.queryRedeemerDataCount DB.queryRedeemerDataPage (fmap f . DB.queryRedeemerDataInfo . entityKey)
        (DB.redeemerDataHash . entityVal) (DB.redeemerDataBytes . entityVal)
    pure $ FixData datumList redeemerDataList
  where
    f queryRes = do
      (prevBlockHsh, mPrevSlotNo) <- queryRes
      prevSlotNo <- mPrevSlotNo
      prevPoint <- convertToPoint (SlotNo prevSlotNo) prevBlockHsh
      Just prevPoint

findWrongPlutusData ::
    forall a m.
    (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text
    -> Text
    -> m Word64 -- query count
    -> (Int64 -> Int64 -> m [a]) -- query a page
    -> (a -> m (Maybe CardanoPoint)) -- get point and previous block point
    -> (a -> ByteString) -- get the hash
    -> (a -> ByteString) -- get the stored bytes
    -> m [FixPlutusData]
findWrongPlutusData tracer tableName qCount qPage qGetInfo getHash getBytes = do
    liftIO $ logInfo tracer $ mconcat
      ["Trying to find ", tableName, " with wrong bytes"]
    count <- qCount
    liftIO $ logInfo tracer $ mconcat
      ["There are ", DB.textShow count, " ", tableName, ". Need to scan them all."]
    datums <- findRec False 0 []
    liftIO $ logInfo tracer $
      Text.concat
        [ "Found ", DB.textShow (length datums), " ", tableName
        , " with mismatch between bytes and hash."
        ]
    pure datums
 where
    findRec :: Bool -> Int64 -> [[FixPlutusData]] -> m [FixPlutusData]
    findRec printedSome offset acc = do
      when (mod offset (10 * limit) == 0 && offset > 0) $
        liftIO $ logInfo tracer $ mconcat ["Checked ", DB.textShow offset, " ", tableName]
      ls <- qPage offset limit
      ls' <- filterM checkValidBytes ls
      ls'' <- mapMaybeM convertToFixPlutusData ls'
      newPrintedSome <- if null ls' || printedSome then pure printedSome else do
        liftIO $ logInfo tracer $ Text.concat
          [ "Found some wrong values already. The oldest ones are (hash, bytes): "
          , DB.textShow $ (\a -> (bsBase16Encode $ getHash a, bsBase16Encode $ getBytes a)) <$> take 5 ls'
          ]
        pure True
      let !newAcc = ls'' : acc
      if fromIntegral (length ls) < limit
      then pure $ reverse $ mconcat newAcc
      else findRec newPrintedSome (offset + limit) newAcc

    checkValidBytes :: a -> m Bool
    checkValidBytes a = case mHashedBytes of
        Left msg -> do
          liftIO $ logWarning tracer $
            Text.concat ["Invalid Binary Data for hash ", DB.textShow actualHash , ": ", Text.pack msg]
          pure False
        Right hashedBytes -> pure $ hashedBytes /= actualHash
      where
        bytes = getBytes a
        actualHash = getHash a
        mHashedBytes = dataHashToBytes . Alonzo.hashBinaryData @StandardAlonzo <$> Alonzo.makeBinaryData (SBS.toShort bytes)

    convertToFixPlutusData :: a -> m (Maybe FixPlutusData)
    convertToFixPlutusData a = do
      mPoint <- qGetInfo a
      case mPoint of
        Nothing -> pure Nothing
        Just prevPoint ->
          pure $ Just $ FixPlutusData
                          { fpdHash = getHash a
                          , fpdPrevPoint = prevPoint
                          }

    limit = 100_000

fixPlutusData :: SyncEnv -> CardanoBlock -> FixData -> IO ()
fixPlutusData env cblk fds = do
    dbBackend <- liftIO $ getBackend env
    DB.runDbIohkNoLogging dbBackend $ do
      mapM_ (fixData True) $ fdDatum fds
      mapM_ (fixData False) $ fdRedeemerData fds
  where
    fixData :: MonadIO m => Bool -> FixPlutusData -> ReaderT SqlBackend m ()
    fixData isDatum fd = do
      case Map.lookup (fpdHash fd) correctBytesMap of
        Nothing -> pure ()
        Just correctBytes | isDatum -> do
          mDatumId <- DB.queryDatum $ fpdHash fd
          case mDatumId of
            Just datumId ->
              DB.upateDatumBytes datumId correctBytes
            Nothing ->
              liftIO $ logWarning tracer $ mconcat
                ["Datum", " not found in block"]
        Just correctBytes -> do
          mRedeemerDataId <- DB.queryRedeemerData $ fpdHash fd
          case mRedeemerDataId of
            Just redeemerDataId ->
              DB.upateRedeemerDataBytes redeemerDataId correctBytes
            Nothing ->
              liftIO $ logWarning tracer $ mconcat
                ["RedeemerData", " not found in block"]

    tracer = getTrace env
    correctBytesMap = Map.union (scrapDatumsBlock cblk) (scrapRedeemerDataBlock cblk)

scrapDatumsBlock :: CardanoBlock -> Map ByteString ByteString
scrapDatumsBlock cblk = case cblk of
    BlockBabbage blk -> Map.unions $ scrapDatumsTxBabbage . snd <$> babbageBlockTxs blk
    BlockAlonzo blk -> Map.unions $ scrapDatumsTxAlonzo . snd <$> alonzoBlockTxs blk
    BlockByron _ -> error "No Datums in Byron"
    BlockShelley _  -> error "No Datums in Shelley"
    BlockAllegra _  -> error "No Datums in Allegra"
    BlockMary _ -> error "No Datums in Mary"

scrapDatumsTxBabbage :: Ledger.Tx StandardBabbage -> Map ByteString ByteString
scrapDatumsTxBabbage tx =
    Map.fromList $ fmap mkTuple $
      witnessData <> outputData <> collOutputData
  where
    mkTuple pd = (txDataHash pd, txDataBytes pd)
    witnessData = txDataWitness tx
    txBody = getField @"body" tx
    outputData = mapMaybe getDatumOutput $ toList $ getField @"outputs" txBody
    collOutputData = mapMaybe getDatumOutput $ toList $ getField @"collateralReturn" txBody

    getDatumOutput :: Babbage.TxOut StandardBabbage -> Maybe PlutusData
    getDatumOutput txOut = case txOut of
        Babbage.TxOut _addr _val (Babbage.Datum binaryData) _mScript ->
          let plutusData = Alonzo.binaryDataToData binaryData
          in Just $ mkTxData (Alonzo.hashData plutusData, plutusData)
        _ -> Nothing

scrapDatumsTxAlonzo :: Ledger.Tx StandardAlonzo -> Map ByteString ByteString
scrapDatumsTxAlonzo tx =
    Map.fromList $ fmap mkTuple witnessData
  where
    mkTuple pd = (txDataHash pd, txDataBytes pd)
    witnessData = txDataWitness tx

scrapRedeemerDataBlock :: CardanoBlock -> Map ByteString ByteString
scrapRedeemerDataBlock cblk = case cblk of
    BlockBabbage blk -> Map.unions $ scrapRedeemerDataTx . snd <$> babbageBlockTxs blk
    BlockAlonzo blk -> Map.unions $ scrapRedeemerDataTx . snd <$> alonzoBlockTxs blk
    BlockByron _ -> error "No RedeemerData in Byron"
    BlockShelley _  -> error "No RedeemerData in Shelley"
    BlockAllegra _  -> error "No RedeemerData in Allegra"
    BlockMary _ -> error "No RedeemerData in Mary"

scrapRedeemerDataTx :: forall era.
    ( Ledger.Crypto era ~ StandardCrypto, Ledger.Era era,
      HasField "txrdmrs" (Ledger.Witnesses era) (Alonzo.Redeemers era)
    )
    => Ledger.Tx era -> Map ByteString ByteString
scrapRedeemerDataTx tx =
    Map.fromList $ mkTuple . fst <$> Map.elems (Alonzo.unRedeemers (getField @"txrdmrs" (getField @"wits" tx)))
  where
    mkTuple dt = mkTuple' $ mkTxData (Alonzo.hashData dt, dt)
    mkTuple' pd = (txDataHash pd, txDataBytes pd)
