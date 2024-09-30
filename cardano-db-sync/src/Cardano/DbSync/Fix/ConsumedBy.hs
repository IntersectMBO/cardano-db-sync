{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Fix.ConsumedBy (FixEntry, fixConsumedBy, fixEntriesConsumed) where

import Cardano.BM.Trace (Trace, logWarning)
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTrace, getTxOutTableType)
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Era.Byron.Insert
import Cardano.DbSync.Era.Byron.Util (blockPayload, unTxHash)
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.Prelude hiding (length, (.))
import Database.Persist.SqlBackend.Internal
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

type FixEntry = (DB.TxOutIdW, DB.TxId)

-- | Nothing when the syncing must stop.
fixConsumedBy :: SqlBackend -> SyncEnv -> CardanoBlock -> IO (Maybe [FixEntry])
fixConsumedBy backend syncEnv cblk = case cblk of
  BlockByron blk -> fixBlock backend syncEnv blk
  _ -> pure Nothing

fixBlock :: SqlBackend -> SyncEnv -> ByronBlock -> IO (Maybe [FixEntry])
fixBlock backend syncEnv bblk = case byronBlockRaw bblk of
  Byron.ABOBBoundary _ -> pure $ Just []
  Byron.ABOBBlock blk -> do
    mEntries <- runReaderT (runExceptT $ mapM (fixTx syncEnv) (blockPayload blk)) backend
    case mEntries of
      Right newEntries -> pure $ Just $ concat newEntries
      Left err -> do
        liftIO $
          logWarning (getTrace syncEnv) $
            mconcat
              [ "While fixing block "
              , textShow bblk
              , ", encountered error "
              , textShow err
              ]
        pure Nothing

fixTx :: MonadIO m => SyncEnv -> Byron.TxAux -> ExceptT SyncNodeError (ReaderT SqlBackend m) [FixEntry]
fixTx syncEnv tx = do
  txId <- liftLookupFail "resolving tx" $ DB.queryTxId txHash
  resolvedInputs <- mapM (resolveTxInputs txOutTableType) (toList $ Byron.txInputs (Byron.taTx tx))
  pure (prepUpdate txId <$> resolvedInputs)
  where
    txOutTableType = getTxOutTableType syncEnv
    txHash = unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
    prepUpdate txId (_, _, txOutId, _) = (txOutId, txId)

fixEntriesConsumed :: SqlBackend -> Trace IO Text -> [FixEntry] -> IO ()
fixEntriesConsumed backend tracer = DB.runDbIohkLogging backend tracer . DB.updateListTxOutConsumedByTxId
