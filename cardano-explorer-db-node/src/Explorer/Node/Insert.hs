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
  , insertGenesisDistribution
  ) where

import           Cardano.Prelude

import qualified Cardano.Crypto as Crypto

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Explorer DB functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Common as Ledger
-- import qualified Cardano.Chain.Common.Lovelace as Ledger
import qualified Cardano.Chain.Genesis as Ledger
import qualified Cardano.Chain.Slotting as Ledger
import qualified Cardano.Chain.UTxO as Ledger

import           Explorer.Node.Insert.Genesis as X

import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB (..))

import           System.Exit (exitFailure)


insertByronBlockOrEBB :: MonadIO m => ByronBlockOrEBB cfg -> m ()
insertByronBlockOrEBB blk = do
  -- liftIO . putTextLn $ sformat ("applying block at depth " % int) ((unChainDifficulty . blockToDifficulty) blk)
  case unByronBlockOrEBB blk of
    Ledger.ABOBBlock ablk -> insertABlock ablk
    Ledger.ABOBBoundary abblk -> insertABOBBoundary abblk

insertABOBBoundary :: MonadIO m => Ledger.ABoundaryBlock ByteString -> m ()
insertABOBBoundary blk = do
  putStrLn $ "ABOBBoundary : " ++ show (Ledger.boundaryHashAnnotated blk)
  putStrLn $ "epochNo      : " ++ show (Ledger.boundaryEpoch $ Ledger.boundaryHeader blk)
  putStrLn $ ("slotNo       : Nothing" :: Text)
  putStrLn $ "blockNo      : " ++ show (Ledger.unChainDifficulty . Ledger.boundaryDifficulty $ Ledger.boundaryHeader blk)
  case Ledger.boundaryPrevHash $ Ledger.boundaryHeader blk of
    Left gh -> validateGenesisTxs (Ledger.unGenesisHash gh)
    Right hh -> putStrLn $ "previous     : " ++ show hh
  putStrLn $ ("merkelRoot   : ????" :: Text)
  putStrLn $ "size         : " ++ show (Ledger.boundaryBlockLength blk)

  putStrLn ("" :: Text) -- FFS Serokell are idiots!

insertABlock :: MonadIO m => Ledger.ABlock ByteString -> m ()
insertABlock blk = do
  putStrLn $ "ABlock       : " ++ show (Ledger.blockHashAnnotated blk)
  let (epoch, slot) = (Ledger.unSlotNumber . Ledger.headerSlot $ Ledger.blockHeader blk) `divMod` 21600
  putStrLn $ "epochNo      : " ++ show epoch
  putStrLn $ "slotNo       : " ++ show slot
  let blockNo = Ledger.unChainDifficulty . Ledger.headerDifficulty $ Ledger.blockHeader blk
  putStrLn $ "blockNo      : " ++ show blockNo
  putStrLn $ "previous     : " ++ show (Ledger.headerPrevHash $ Ledger.blockHeader blk)
  putStrLn $ "merkelRoot   : " ++ show (Ledger.getMerkleRoot . Ledger.txpRoot . Ledger.recoverTxProof . Ledger.bodyTxPayload $ Ledger.blockBody blk)
  putStrLn $ "size         : " ++ show (Ledger.blockLength blk)

  let txs = blockPayload blk
  putStrLn $ "tx count     : " ++ show (length txs)

  putStrLn ("" :: Text)

  mapM_ (insertTx blockNo) txs


blockPayload :: Ledger.ABlock a -> [Ledger.TxAux]
blockPayload =
  Ledger.unTxPayload . Ledger.bodyTxPayload . Ledger.blockBody

insertTx :: MonadIO m => Word64 -> Ledger.TxAux -> m ()
insertTx blkNo tx = do
    let txHash = Crypto.hash $ Ledger.taTx tx
    putStrLn $ "    Tx      : " ++ show txHash
    putStrLn $ "    blockNo : " ++ show blkNo
    putStrLn $ "    inputs  : " ++ show (toList $ Ledger.txInputs (Ledger.taTx  tx))
    putStrLn $ "    output  : " ++ show (either (panic "insertTx") identity $ calculateFee (Ledger.taTx tx))
    putStrLn ( "    fee     : ????" :: Text)

    putStrLn ("" :: Text)
    mapM_ (insertTxIn txHash) (Ledger.txInputs $ Ledger.taTx tx)
    zipWithM_ (insertTxOut txHash) [0 ..] (toList . Ledger.txOutputs $ Ledger.taTx tx)
    putStrLn ("" :: Text)


    when (length (Ledger.txOutputs $ Ledger.taTx tx) > 2 || length (Ledger.txInputs (Ledger.taTx  tx)) > 2) $
      liftIO exitFailure

-- TODO : Actually calculate fee. Currently returns the sum of the outputs.
calculateFee :: Ledger.Tx -> Either Ledger.LovelaceError Ledger.Lovelace
calculateFee tx =
    output -- input - output
  where
    output :: Either Ledger.LovelaceError Ledger.Lovelace
    output = Ledger.sumLovelace (map Ledger.txOutValue $ Ledger.txOutputs tx)
    -- input = lookup all inputs using 'Ledger.txInputs tx'

insertTxOut :: MonadIO m => Ledger.TxId -> Word32 -> Ledger.TxOut -> m ()
insertTxOut txHash index txout = do
  putStrLn $ "    TxOut     : " ++ show txHash
  putStrLn $ "      index   : " ++ show index
  putStrLn $ "      address : " ++ show (Ledger.addrRoot $ Ledger.txOutAddress txout)
  putStrLn $ "      value   : " ++ show (Ledger.txOutValue txout)

insertTxIn :: MonadIO m => Ledger.TxId -> Ledger.TxIn -> m ()
insertTxIn txHash (Ledger.TxInUtxo inTxHash inIndex) = do
  putStrLn $ "    TxIn      : " ++ show txHash
  putStrLn $ "      input   : " ++ show inTxHash
  putStrLn $ "      index   : " ++ show inIndex

