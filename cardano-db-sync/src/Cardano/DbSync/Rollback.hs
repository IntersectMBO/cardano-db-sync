{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Rollback (
  prepareRollback,
  rollbackFromBlockNo,
  unsafeRollback,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.AppT (App, MonadAppDB (..), SyncEnv (..), askTrace, runAppWithNoLogging)
import Cardano.DbSync.Cache
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error.Types (SyncNodeError)
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
  BlockNo ->
  App ()
rollbackFromBlockNo blkNo = do
  SyncEnv {envCache = cache} <- ask
  trce <- askTrace
  nBlocks <- dbQueryToApp $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
  mres <- dbQueryToApp $ DB.queryBlockNoAndEpoch (unBlockNo blkNo)
  whenJust mres $ \(blockId, epochNo) -> do
    liftIO
      . logInfo trce
      $ mconcat
        [ "Deleting "
        , textShow nBlocks
        , " numbered equal to or greater than "
        , textShow blkNo
        ]
    (mTxId, deletedBlockCount) <- dbQueryToApp $ DB.deleteBlocksBlockId trce blockId
    whenConsumeOrPruneTxOut (dbQueryToApp $ DB.setNullTxOut trce mTxId)
    dbQueryToApp $ DB.deleteEpochRows epochNo
    dbQueryToApp $ DB.deleteDrepDistr epochNo
    dbQueryToApp $ DB.deleteRewardRest epochNo
    dbQueryToApp $ DB.setNullEnacted epochNo
    dbQueryToApp $ DB.setNullRatified epochNo
    dbQueryToApp $ DB.setNullDropped epochNo
    dbQueryToApp $ DB.setNullExpired epochNo
    when (deletedBlockCount > 0) $ do
      -- We use custom constraints to improve input speeds when syncing.
      -- If they don't already exists we add them here as once a rollback has happened
      -- we always need a the constraints.
      addConstraintsIfNotExist
    rollbackCache cache blockId
    liftIO . logInfo trce $ "Blocks deleted"

prepareRollback :: CardanoPoint -> Tip CardanoBlock -> App (Either SyncNodeError Bool)
prepareRollback point serverTip = do
  syncEnv <- ask
  liftIO $ DB.runDbIohkNoLogging (envBackend syncEnv) (runAppWithNoLogging syncEnv action)
  where
    action :: App (Either SyncNodeError Bool)
    action = do
      trce <- askTrace
      case getPoint point of
        Origin -> do
          nBlocks <- dbQueryToApp DB.queryCountSlotNo
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
          nBlocks <- dbQueryToApp $ DB.queryCountSlotNosGreaterThan (unSlotNo $ blockPointSlot blk)
          mBlockNo <-
            liftLookupFail "Rollback.prepareRollback" $
              dbQueryToApp $
                DB.queryBlockHashBlockNo (SBS.fromShort . getOneEraHash $ blockPointHash blk)
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
      pure $ Right False

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce config slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteBlocksSlotNo trce slotNo)
