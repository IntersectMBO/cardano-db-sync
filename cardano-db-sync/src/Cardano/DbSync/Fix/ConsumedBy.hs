{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Fix.ConsumedBy (fixConsumedBy) where

import Cardano.BM.Trace (Trace, logWarning)
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Byron.Insert
import Cardano.DbSync.Era.Byron.Util (blockPayload, unTxHash)
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.Prelude hiding (length)
import Database.Persist.SqlBackend.Internal
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

fixConsumedBy :: SqlBackend -> Trace IO Text -> Integer -> CardanoBlock -> IO (Integer, Bool)
fixConsumedBy backend tracer lastSize cblk = case cblk of
  BlockByron blk -> (\(n, bl) -> (n + lastSize, bl)) <$> fixBlock backend tracer blk
  _ -> pure (lastSize, True)

fixBlock :: SqlBackend -> Trace IO Text -> ByronBlock -> IO (Integer, Bool)
fixBlock backend tracer bblk = case byronBlockRaw bblk of
  Byron.ABOBBoundary _ -> pure (0, False)
  Byron.ABOBBlock blk -> do
    runReaderT (fix 0 (blockPayload blk)) backend
  where
    fix totalSize [] = pure (totalSize, False)
    fix totalSize (tx : txs) = do
      mn <- runExceptT $ fixTx tx
      case mn of
        Right n -> fix (totalSize + n) txs
        Left err -> do
          liftIO $
            logWarning tracer $
              mconcat
                [ "While fixing tx "
                , textShow tx
                , ", encountered error "
                , textShow err
                ]
          pure (totalSize, True)

fixTx :: MonadIO m => Byron.TxAux -> ExceptT SyncNodeError (ReaderT SqlBackend m) Integer
fixTx tx = do
  txId <- liftLookupFail "resolving tx" $ DB.queryTxId txHash
  resolvedInputs <- mapM resolveTxInputs (toList $ Byron.txInputs (Byron.taTx tx))
  lift $ DB.updateListTxOutConsumedByTxId (prepUpdate txId <$> resolvedInputs)
  pure $ fromIntegral $ length resolvedInputs
  where
    txHash = unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
    prepUpdate txId (_, _, txOutId, _) = (txOutId, txId)
