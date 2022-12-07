{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Cardano.DbSync.Rollback
  ( rollbackToPoint
  , rollbackFromBlockNo
  , unsafeRollback
  ) where

import           Cardano.Prelude
import qualified Data.ByteString.Short as SBS

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Api
import           Cardano.DbSync.Cache
import           Cardano.DbSync.Era.Util
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point

rollbackFromBlockNo :: MonadIO m => SyncEnv -> BlockNo -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
rollbackFromBlockNo env blkNo = do
    nBlocks <- lift $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
    mBlockId <- lift $ DB.queryBlockNo (unBlockNo blkNo)
    whenStrictJust (maybeToStrict mBlockId) $ \blockId -> do
      liftIO . logInfo trce $
        mconcat
          [ "Deleting ", textShow nBlocks, " blocks after "
          , " or equal to "
          , textShow blkNo
          ]
      lift $ rollbackCache cache (Just $ unBlockNo blkNo) True (fromIntegral nBlocks)
      deleted <- lift $ DB.deleteCascadeAfter blockId True
      liftIO . logInfo trce $
                if deleted
                  then "Blocks deleted"
                  else "No blocks need to be deleted"
  where
    trce = getTrace env
    cache = envCache env

-- Rollbacks are done in an Era generic way based on the 'Point' we are
-- rolling back to.
rollbackToPoint :: SyncEnv -> CardanoPoint -> Tip CardanoBlock -> IO (Either SyncNodeError Bool)
rollbackToPoint env point serverTip = do
    backend <- getBackend env
    DB.runDbIohkNoLogging backend $ runExceptT action
  where
    trce = getTrace env
    cache = envCache env

    slotsToDelete :: MonadIO m => WithOrigin SlotNo -> ReaderT SqlBackend m (Maybe SlotNo, Word64)
    slotsToDelete wosl =
      case wosl of
        Origin -> do
          mSlotNo <- DB.queryLastSlotNo
          countSlotNos <- DB.queryCountSlotNo
          pure (mSlotNo, countSlotNos)
        At sl -> do
          mSlotNo <- DB.queryLastSlotNoGreaterThan (unSlotNo sl)
          countSlotNos <- DB.queryCountSlotNosGreaterThan (unSlotNo sl)
          pure (mSlotNo, countSlotNos)

    action :: MonadIO m => ExceptT SyncNodeError (ReaderT SqlBackend m) Bool
    action = do
        (mSlotNo, nBlocks) <- lift $ slotsToDelete (pointSlot point)
        (prevId, mBlockNo) <- liftLookupFail "Rollback.rollbackToPoint" $ queryBlock point

        if nBlocks <= 50 || not (hasLedgerState env) then do
          liftIO . logInfo trce $ "Rolling back to " <> renderPoint point
          whenStrictJust (maybeToStrict mSlotNo) $ \slotNo ->
          -- there may be more deleted blocks than slots, because ebbs don't have
          -- a slot. We can only make an estimation here.
            liftIO . logInfo trce $
                mconcat
                  [ "Deleting ", textShow nBlocks, " blocks up to slot "
                  , textShow (unSlotNo slotNo)
                  ]
          -- We delete the block right after the point we rollback to. This delete
          -- should cascade to the rest of the chain.

          -- 'length xs' here gives an approximation of the blocks deleted. An approximation
          -- is good enough, since it is only used to decide on the best policy and is not
          -- important for correctness.
          -- We need to first cleanup the cache and then delete the blocks from db.
          lift $ rollbackCache cache mBlockNo False (fromIntegral nBlocks)
          deleted <- lift $ DB.deleteCascadeAfter prevId False
          liftIO . logInfo trce $
                    if deleted
                      then "Blocks deleted"
                      else "No blocks need to be deleted"
          pure True
        else do
          liftIO . logInfo trce $
            mconcat
              [ "Delaying delete of ", textShow nBlocks, " blocks after "
              , textShow mBlockNo, " while rolling back to (" , renderPoint point
              , "). Applying blocks until a new block is found. The node is currently at ", textShow serverTip
              ]
          pure False

queryBlock :: MonadIO m => Point CardanoBlock
           -> ReaderT SqlBackend m (Either DB.LookupFail (DB.BlockId, Maybe Word64))
queryBlock pnt = do
  case getPoint pnt of
    Origin ->
      fmap (, Nothing) <$> DB.queryGenesis
    At blkPoint ->
      DB.queryBlockNoId (SBS.fromShort . getOneEraHash $ blockPointHash blkPoint)

-- For testing and debugging.
unsafeRollback :: Trace IO Text -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce config slotNo = do
  logInfo trce $ "Forced rollback to slot " <> textShow (unSlotNo slotNo)
  Right <$> DB.runDbNoLogging (DB.PGPassCached config) (void $ DB.deleteCascadeSlotNo slotNo)
