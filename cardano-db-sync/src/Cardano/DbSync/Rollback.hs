{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Rollback (
  prepareRollback,
  rollbackFromBlockNo,
  unsafeRollback,
) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache
import Cardano.DbSync.DbEvent (liftDbIO)
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)
import Cardano.Prelude
import Control.Monad.Extra (whenJust)
import qualified Data.ByteString.Short as SBS
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)
import Ouroboros.Network.Block
import Ouroboros.Network.Point

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackFromBlockNo ::
  MonadIO m =>
  SyncEnv ->
  BlockNo ->
  DB.DbAction m ()
rollbackFromBlockNo syncEnv blkNo = do
  nBlocks <- DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
  mres <- DB.queryBlockNoAndEpoch (unBlockNo blkNo)
  whenJust mres $ \(blockId, epochNo) -> do
    liftIO
      . logInfo trce
      $ mconcat
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

prepareRollback :: SyncEnv -> CardanoPoint -> Tip CardanoBlock -> ExceptT SyncNodeError IO Bool
prepareRollback syncEnv point serverTip =
  liftDbIO $ DB.runDbIohkNoLogging (envDbEnv syncEnv) action
  where
    trce = getTrace syncEnv

    action :: MonadIO m => DB.DbAction m Bool
    action = do
      case getPoint point of
        Origin -> do
          nBlocks <- DB.queryCountSlotNo
          if nBlocks == 0
            then do
              liftIO . logInfo trce $ "Starting from Genesis"
            else do
              liftIO
                . logInfo trce
                $ mconcat
                  [ "Delaying delete of "
                  , textShow nBlocks
                  , " while rolling back to genesis."
                  , " Applying blocks until a new block is found."
                  , " The node is currently at "
                  , textShow serverTip
                  ]
        At blk -> do
          nBlocks <- DB.queryCountSlotNosGreaterThan (unSlotNo $ blockPointSlot blk)
          mBlockNo <- DB.queryBlockHashBlockNo (SBS.fromShort . getOneEraHash $ blockPointHash blk)
          liftIO
            . logInfo trce
            $ mconcat
              [ "Delaying delete of "
              , textShow nBlocks
              , " blocks after "
              , textShow mBlockNo
              , " while rolling back to ("
              , renderPoint point
              , "). Applying blocks until a new block is found. The node is currently at "
              , textShow serverTip
              ]
      pure False

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.TxOutVariantType -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce txOutVariantType config slotNo = do
  logWarning trce $ "Starting a forced rollback to slot: " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteBlocksSlotNo trce txOutVariantType slotNo True)
