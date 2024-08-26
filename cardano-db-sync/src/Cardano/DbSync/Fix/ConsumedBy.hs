{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Fix.ConsumedBy (FixEntry, fixConsumedBy, fixEntriesConsumed) where

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
import Cardano.Prelude hiding (length, (.))
import Database.Persist.SqlBackend.Internal
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

type FixEntry = (DB.TxOutId, DB.TxId)

-- | Nothing when the syncing must stop.
fixConsumedBy :: SqlBackend -> Trace IO Text -> CardanoBlock -> IO (Maybe [FixEntry])
fixConsumedBy backend tracer cblk = case cblk of
  BlockByron blk -> fixBlock backend tracer blk
  _ -> pure Nothing

fixBlock :: SqlBackend -> Trace IO Text -> ByronBlock -> IO (Maybe [FixEntry])
fixBlock backend tracer bblk = case byronBlockRaw bblk of
  Byron.ABOBBoundary _ -> pure $ Just []
  Byron.ABOBBlock blk -> do
    mEntries <- runReaderT (runExceptT $ mapM fixTx (blockPayload blk)) backend
    case mEntries of
      Right newEntries -> pure $ Just $ concat newEntries
      Left err -> do
        liftIO $
          logWarning tracer $
            mconcat
              [ "While fixing block "
              , textShow bblk
              , ", encountered error "
              , textShow err
              ]
        pure Nothing

fixTx :: MonadIO m => Byron.TxAux -> ExceptT SyncNodeError (ReaderT SqlBackend m) [FixEntry]
fixTx tx = do
  txId <- liftLookupFail "resolving tx" $ DB.queryTxId txHash
  resolvedInputs <- mapM resolveTxInputs (toList $ Byron.txInputs (Byron.taTx tx))
  pure (prepUpdate txId <$> resolvedInputs)
  where
    txHash = unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
    prepUpdate txId (_, _, txOutId, _) = (txOutId, txId)

fixEntriesConsumed :: SqlBackend -> Trace IO Text -> [FixEntry] -> IO ()
fixEntriesConsumed backend tracer = DB.runDbIohkLogging backend tracer . DB.updateListTxOutConsumedByTxId
