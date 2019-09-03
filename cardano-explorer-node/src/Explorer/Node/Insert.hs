{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Node.Insert
  ( insertByronBlockOrEBB
  , insertValidateGenesisDistribution
  ) where

import           Cardano.BM.Trace (Trace, logInfo)

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Explorer DB functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Common as Ledger
import qualified Cardano.Chain.UTxO as Ledger

import qualified Cardano.Crypto as Crypto

import           Cardano.Prelude

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Explorer.DB as DB
import           Explorer.Node.Insert.Genesis
import           Explorer.Node.Util

import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB (..))

insertByronBlockOrEBB :: MonadIO m => Trace IO Text -> ByronBlockOrEBB cfg -> m ()
insertByronBlockOrEBB tracer blk =
  liftIO $ case unByronBlockOrEBB blk of
            Ledger.ABOBBlock ablk -> insertABlock tracer ablk
            Ledger.ABOBBoundary abblk -> insertABOBBoundary tracer abblk

insertABOBBoundary :: Trace IO Text -> Ledger.ABoundaryBlock ByteString -> IO ()
insertABOBBoundary tracer blk = do
    if False
      then DB.runDbIohkLogging tracer insertAction
      else DB.runDbNoLogging insertAction

    logInfo tracer $ Text.concat
                    [ "insertABOBBoundary: epoch "
                    , textShow (Ledger.boundaryEpoch $ Ledger.boundaryHeader blk)
                    , " hash "
                    , renderAbstractHash (Ledger.boundaryHashAnnotated blk)
                    ]
  where
    insertAction :: MonadIO m => ReaderT SqlBackend m ()
    insertAction = do
      let prevHash = case Ledger.boundaryPrevHash (Ledger.boundaryHeader blk) of
                        Left gh -> genesisToHeaderHash gh
                        Right hh -> hh
      pbid <- leftPanic "insertABOBBoundary: "
                  <$> DB.queryBlockId (unHeaderHash prevHash)
      slid <- DB.insertSlotLeader $ DB.SlotLeader (BS.replicate 28 '\0') "Epoch boundary slot leader"
      void . DB.insertBlock $
                DB.Block
                  { DB.blockHash = unHeaderHash $ Ledger.boundaryHashAnnotated blk
                  , DB.blockSlotNo = Nothing -- No slotNo for a boundary block
                  , DB.blockBlockNo = Nothing
                  , DB.blockPrevious = Just pbid
                  , DB.blockMerkelRoot = Nothing -- No merkelRoot for a boundary block
                  , DB.blockSlotLeader = slid
                  , DB.blockSize = fromIntegral $ Ledger.boundaryBlockLength blk
                  }
      supply <- DB.queryTotalSupply
      liftIO $ logInfo tracer $ Text.concat
                    [ "Total supply at start of epoch ", textShow (boundaryEpochNumber blk)
                    , " is ", DB.renderAda supply, " Ada"
                    ]

insertABlock :: Trace IO Text -> Ledger.ABlock ByteString -> IO ()
insertABlock tracer blk = do
    if False
      then DB.runDbIohkLogging tracer insertAction
      else DB.runDbNoLogging insertAction

    when False $
      logInfo tracer $ "insertABlock: " <> renderAbstractHash (blockHash blk)
  where
    insertAction :: MonadIO m => ReaderT SqlBackend m ()
    insertAction = do
      pbid <- leftPanic "insertABlock: "
                  <$> DB.queryBlockId (unHeaderHash $ blockPreviousHash blk)

      slid <- DB.insertSlotLeader $ mkSlotLeader blk
      blkId <- DB.insertBlock $
                    DB.Block
                      { DB.blockHash = unHeaderHash $ blockHash blk
                      , DB.blockSlotNo = Just $ slotNumber blk
                      , DB.blockBlockNo = Just $ blockNumber blk
                      , DB.blockPrevious = Just pbid
                      , DB.blockMerkelRoot = Just $ unCryptoHash (blockMerkelRoot blk)
                      , DB.blockSlotLeader = slid
                      , DB.blockSize = fromIntegral $ Ledger.blockLength blk
                      }

      mapM_ (insertTx tracer blkId) $ blockPayload blk

insertTx :: MonadIO m => Trace IO Text -> DB.BlockId -> Ledger.TxAux -> ReaderT SqlBackend m ()
insertTx tracer blkId tx = do
    let txHash = Crypto.hash $ Ledger.taTx tx
    fee <- calculateTxFee $ Ledger.taTx tx
    txId <- DB.insertTx $
                DB.Tx
                  { DB.txHash = unTxHash txHash
                  , DB.txBlock = blkId
                  , DB.txFee = fee
                  }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (noit sure this can even happen).
    zipWithM_ (insertTxOut tracer txId) [0 ..] (toList . Ledger.txOutputs $ Ledger.taTx tx)
    mapM_ (insertTxIn tracer txId) (Ledger.txInputs $ Ledger.taTx tx)


insertTxOut :: MonadIO m => Trace IO Text -> DB.TxId -> Word32 -> Ledger.TxOut -> ReaderT SqlBackend m ()
insertTxOut _tracer txId index txout = do
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = fromIntegral index
              , DB.txOutAddress = Text.decodeUtf8 $ Ledger.addrToBase58 (Ledger.txOutAddress txout)
              , DB.txOutValue = Ledger.unsafeGetLovelace $ Ledger.txOutValue txout
              }


insertTxIn :: MonadIO m => Trace IO Text -> DB.TxId -> Ledger.TxIn -> ReaderT SqlBackend m ()
insertTxIn _tracer txInId (Ledger.TxInUtxo txHash inIndex) = do
  txOutId <- leftPanic "insertTxIn: "
                <$> DB.queryTxId (unTxHash txHash)
  void $ DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral inIndex
              }

-- -----------------------------------------------------------------------------

calculateTxFee :: MonadIO m => Ledger.Tx -> ReaderT SqlBackend m Word64
calculateTxFee tx = do
    case output of
      Left err -> panic $ "calculateTxFee: " <> textShow err
      Right outval -> do
        inval <- sum <$> mapM DB.queryTxOutValue inputs
        if outval > inval
          then panic $ "calculateTxFee: " <> textShow (outval, inval)
          else pure $ inval - outval
  where
    -- [(Hash of tx, index within tx)]
    inputs :: [(ByteString, Word16)]
    inputs = map unpack $ toList (Ledger.txInputs tx)

    unpack :: Ledger.TxIn -> (ByteString, Word16)
    unpack (Ledger.TxInUtxo txHash index) = (unTxHash txHash, fromIntegral index)

    output :: Either Ledger.LovelaceError Word64
    output =
      Ledger.unsafeGetLovelace
        <$> Ledger.sumLovelace (map Ledger.txOutValue $ Ledger.txOutputs tx)
