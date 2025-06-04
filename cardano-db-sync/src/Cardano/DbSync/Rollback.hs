{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Rollback (
  prepareRollback,
  rollbackFromBlockNo,
  rollbackLedger,
  unsafeRollback,
) where

import Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)
import Ouroboros.Consensus.HeaderValidation hiding (TipInfo)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Network.Block
import Ouroboros.Network.Point

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import Control.Monad.Extra (whenJust)

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Cache
import Cardano.DbSync.Error (SyncNodeError (..), logAndThrowIO)
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types (CardanoLedgerState (..), SnapshotPoint (..))
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)

rollbackFromBlockNo ::
  MonadIO m =>
  SyncEnv ->
  BlockNo ->
  DB.DbAction m ()
rollbackFromBlockNo syncEnv blkNo = do
  nBlocks <- DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
  mres <- DB.queryBlockNoAndEpoch (unBlockNo blkNo)
  -- Use whenJust like the original - silently skip if block not found
  whenJust mres $ \(blockId, epochNo) -> do
    liftIO . logInfo trce $
      mconcat
        [ "Deleting "
        , textShow nBlocks
        , " numbered equal to or greater than "
        , textShow blkNo
        ]

    deletedBlockCount <- DB.deleteBlocksBlockId trce txOutVariantType blockId epochNo (DB.pcmConsumedTxOut $ getPruneConsume syncEnv)
    when (deletedBlockCount > 0) $ do
      -- We use custom constraints to improve input speeds when syncing.
      -- If they don't already exists we add them here as once a rollback has happened
      -- we always need the constraints.
      addConstraintsIfNotExist syncEnv trce

    rollbackCache cache blockId
    liftIO . logInfo trce $ "Blocks deleted"
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv
    txOutVariantType = getTxOutVariantType syncEnv

-- Also fix the error type in prepareRollback
prepareRollback :: SyncEnv -> CardanoPoint -> Tip CardanoBlock -> IO (Either SyncNodeError Bool)
prepareRollback syncEnv point serverTip = do
  DB.runDbIohkNoLogging (envDbEnv syncEnv) $ runExceptT action
  where
    trce = getTrace syncEnv

    action :: MonadIO m => ExceptT SyncNodeError (DB.DbAction m) Bool
    action = do
      case getPoint point of
        Origin -> do
          nBlocks <- lift DB.queryCountSlotNo
          if nBlocks == 0
            then do
              liftIO . logInfo trce $ "Starting from Genesis"
              pure True
            else do
              liftIO . logInfo trce $
                mconcat
                  [ "Delaying delete of "
                  , textShow nBlocks
                  , " while rolling back to genesis."
                  , " Applying blocks until a new block is found."
                  , " The node is currently at "
                  , textShow serverTip
                  ]
              pure False
        At blk -> do
          nBlocks <- lift $ DB.queryCountSlotNosGreaterThan (unSlotNo $ blockPointSlot blk)
          mBlockNo <- lift $ DB.queryBlockHashBlockNo (SBS.fromShort . getOneEraHash $ blockPointHash blk)
          case mBlockNo of
            Nothing -> throwError $ SNErrRollback "Rollback.prepareRollback: queryBlockHashBlockNo: Block hash not found"
            Just blockN -> do
              liftIO . logInfo trce $
                mconcat
                  [ "Delaying delete of "
                  , textShow nBlocks
                  , " blocks after "
                  , textShow blockN
                  , " while rolling back to ("
                  , renderPoint point
                  , "). Applying blocks until a new block is found. The node is currently at "
                  , textShow serverTip
                  ]
              pure False

rollbackLedger :: SyncEnv -> CardanoPoint -> IO (Maybe [CardanoPoint])
rollbackLedger syncEnv point =
  case envLedgerEnv syncEnv of
    HasLedger hle -> do
      mst <- loadLedgerAtPoint hle point
      case mst of
        Right st -> do
          let statePoint = headerStatePoint $ headerState $ clsState st
          -- This is an extra validation that should always succeed.
          unless (point == statePoint) $
            logAndThrowIO (getTrace syncEnv) $
              SNErrDatabaseRollBackLedger $
                mconcat
                  [ "Ledger "
                  , show statePoint
                  , " and ChainSync "
                  , show point
                  , " don't match."
                  ]
          pure Nothing
        Left lsfs ->
          Just . fmap fst <$> verifySnapshotPoint syncEnv (OnDisk <$> lsfs)
    NoLedger _ -> pure Nothing

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.TxOutVariantType -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce txOutVariantType config slotNo = do
  logWarning trce $ "Starting a forced rollback to slot: " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteBlocksSlotNo trce txOutVariantType slotNo True)
